library(RClickhouse)

con <- DBI::dbConnect(RClickhouse::clickhouse(), 
                 host="", 
                 user = "",
                 password = "",
                 db = "")

# DBI::dbGetQuery(con, "DROP TABLE yandex_reports_keys_test")

DBI::dbGetQuery(con, "CREATE TABLE yandex_reports_keys
                      (
                        date Date,
                        campaignId String,
                        campaignName String,
                        keyword String,
                        impressions UInt32,
                        click UInt32,
                        cost_euro Float32,
                        cost_ruble UInt32,
                        cpc_ruble UInt32,
                        eur_rub UInt32
                      )
                      ENGINE = MergeTree
                      ORDER BY (date, campaignName)")


DBI::dbGetQuery(con, "CREATE TABLE yandex_reports_keys_test
                      (
                date Date,
                campaignId String,
                campaignName String,
                keyword String,
                impressions UInt32,
                click UInt32,
                cost_euro Float32,
                cost_ruble UInt32,
                cpc_ruble UInt32,
                eur_rub UInt32
)
ENGINE = ReplacingMergeTree
ORDER BY (date, campaignId, campaignName, keyword)")
