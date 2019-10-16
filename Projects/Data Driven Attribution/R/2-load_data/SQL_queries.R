raw_query <- "
SELECT 
    date,
    visit_time,
    -- purchase time важен для заказов, где в рамках визита есть несколько токенов
    datetime as purchase_time,
    visit_id,
    client_id,
    agent,
    tag,
    book_agent,
    book_tag,
    campaign,
    keyword,
    order_total,
    profit,
    improved_cpc as cpc,
    was_payment
FROM
(
    SELECT  
        *, 
        multiIf(agent = 'sky001',  5512351,
                agent = 'mom76gh', 512351,
                agent = 'ya2678' AND date <= '2019-06-28', 7347,
                agent = 'ya2678' AND date > '2019-06-28', 634563,
                agent = 'btp012', 13451,
                cpc) as cpc,
        if(match(ref, 'skyscanner|momondo|yandex|google|biletyplus|criteo'), 
                cpc, 
                0) as improved_cpc
    FROM
    (
        SELECT
            date,
            visit_time,
            datetime, 
            visit_id,
            client_id, 
            agent,
            tag,
            book_agent,
            book_tag,
            if(substring(referrer, 1, 4) != 'http', referrer, domainWithoutWWW(referrer)) as ref,
            order_total,
            profit,
            if(isNull(next_utm_term), '', next_utm_term) as keyword,
            if(next_utm_source IN ('yandex', 'direct'), extract(next_utm_campaign, '\\d{8}$'), next_utm_campaign) as campaign,
            was_payment
        FROM 
        (   
            SELECT *
            FROM
            (
                SELECT
                    date,
                    datetime as visit_time, 
                    visit_id,
                    client_id,
                    multiIf(next_tag = 'seo1000', 'seo_yandex', 
                        next_tag = 'seo0000', 'seo_google', 
                        next_tag LIKE 'adw_dynamic%', 'remarketing_google',
                        next_tag LIKE 'cpcretarg%', 'retargeting_yandex',
                        next_agent) AS agent,
                    next_tag as tag,
                    next_utm_source,
                    next_utm_campaign,
                    next_utm_term,
                    referrer
                FROM replicated.`frontend.visit`
                WHERE 
                    date BETWEEN toDate('{{ Период.start }}') - {{ Глубина }}  AND '{{ Период.end }}'
                    AND is_bot = 0
            ) LEFT JOIN (
                            -- Джойним, так как некоторые покупки совершаются с разных визитов
                            SELECT visit_id, token
                            FROM replicated.`site.booking.step0.pre_booking.success`
                            WHERE 
                                date BETWEEN toDate('{{ Период.start }}') - {{ Глубина }} - 1 
                                AND toDate('{{ Период.end }}') + 1
                        ) USING visit_id
        ) LEFT JOIN (
                        SELECT
                            datetime,
                            visit_id,
                            client_id,
                            token,
                            order_total / 100 AS order_total,
                            profit / 100 AS profit,
                            agent AS book_agent,
                            tag AS book_tag,
                            1 AS was_payment
                        FROM replicated.`backend.booking.issuing.success`
                        WHERE  
                            date BETWEEN toDate('{{ Период.start }}') - {{ Глубина }} - 1  
                                AND toDate('{{ Период.end }}') + 1
                            AND visit_id != toUUID(0)
                            AND client_id != toUUID(0)
                    ) USING token
    ) LEFT JOIN (
                    SELECT 
                        date, 
                        campaign,
                        '' as keyword,
                        SUM(cost_ruble) / 100 / SUM(click) as cpc
                    FROM context_ads.`criteo_reports`
                    WHERE 
                        date BETWEEN toDate('{{ Период.start }}') - {{ Глубина }} AND '{{ Период.end }}'
                    GROUP BY 
                        date, 
                        campaign
                    
                    UNION ALL
                    
                    SELECT 
                        date,
                        campaign,
                        keyword,
                        if(isNull(keyword) OR keyword = '', cpc_camp, cpc_key) as cpc
                    FROM 
                    (
                        SELECT
                            date,
                            campaignName as campaign,
                            if(SUM(click) = 0, 0, SUM(cost_ruble) / 100 / SUM(click)) as cpc_camp
                        FROM context_ads.`adwords_reports_campaigns`
                        WHERE 
                            date BETWEEN toDate('{{ Период.start }}') - {{ Глубина }} AND '{{ Период.end }}'
                        GROUP BY 
                            date, 
                            campaign
                    ) LEFT JOIN (
                                    SELECT
                                        date,
                                        max(campaignName) as campaign,
                                        keyword,
                                        if(SUM(click) = 0, 0, SUM(cost_ruble) / 100 / SUM(click)) as cpc_key
                                    FROM context_ads.`adwords_reports_keys`
                                    WHERE 
                                        date BETWEEN toDate('{{ Период.start }}') - {{ Глубина }} AND '{{ Период.end }}'
                                    GROUP BY 
                                        date, 
                                        keyword
                                ) USING date, campaign
                    
                    UNION ALL 
                    
                    SELECT
                        date,
                        campaignId as campaign,
                        keyword,
                        if(SUM(click) = 0, 0, SUM(cost_ruble) / 100 / SUM(click))
                    FROM context_ads.`yandex_reports_keys`
                    WHERE 
                        date BETWEEN toDate('{{ Период.start }}') - {{ Глубина }}  AND '{{ Период.end }}'
                    GROUP BY 
                        date, 
                        campaign,
                        keyword
                ) USING date, campaign, keyword
    ORDER BY datetime
)
"