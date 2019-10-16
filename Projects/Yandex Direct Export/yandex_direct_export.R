require(ryandexdirect)
require(RClickhouse)
require(dplyr)
require(lubridate)
require(DBI)
library(stringr)
require(zoo)
require(XML)

acc <- "kupi.com"
tok <- "yadir_token"
dates_report <- list(from = as.Date('2019-07-03'), to = as.Date('2019-07-03'))

# get currency from cbr
get_cbr_rates <- function(code = "USD", start_date = today() - 7, end_date = today()) {
  
  currency <- data.frame(
    id = c(840, 978, 826),
    code = c("USD", "EUR", "GBP"),
    CBR = c("R01235", "R01239", "R01035"),
    stringsAsFactors = FALSE
  )  
  
  start <- strftime(start_date, "%d/%m/%Y")
  end <- strftime(end_date, "%d/%m/%Y")
  
  fx_url <- 
    httr::modify_url(
      "http://www.cbr.ru/scripts/XML_dynamic.asp", 
      query = list(
        VAL_NM_RQ = currency[currency$code == code, ]$CBR,
        date_req1 = start,
        date_req2 = end
      ))
  res <- httr::POST(fx_url)
  if(httr::http_error(res)) stop("Http ", res$status_code)
  
  xml <- httr::content(res, encoding = "UTF-8")
  xml <- XML::xmlParse(xml)
  
  result <- XML::xmlToDataFrame(xml)
  date <- XML::getNodeSet(xml, '//Record/@Date')
  result$Date <- date
  result$Date <- lubridate::dmy(result$Date)

  result$Value <- as.double(gsub(",", ".", result[[2]]))
  return(result[c(3, 2)])
}
cbr_rates_with_weekends <- function(code = "USD", start_date = today() - 7, end_date = today()){
  if (wday(start_date) >= 6 | wday(start_date) == 1){
    start_date = start_date - 3
  }
  
  cbr_rates <- get_cbr_rates(code = code, start_date = start_date, end_date = end_date)
  from <- min(cbr_rates[[1]])
  to <- max(cbr_rates[[1]])
  all_dates <- data.frame(Date = seq(start_date+1, end_date, by=1))
  res <- dplyr::left_join(all_dates, cbr_rates, by = "Date")
  zoo::na.locf(res)
}

normalize_keywords <- function(vec){
  del <- function(x) str_remove_all(x, "\\s-[а-яА-ЯёЁ\\d\\w!]+")
  match <- function(x) str_extract_all(x, "[а-яА-ЯёЁ\\w\\d\\-\\.]+")
  join <- function(x) sapply(x, paste0, collapse = ' ')
  normalize <- function(x) str_squish(tolower(x))
  
  return (normalize(join(match(del(vec)))))
}

# create connection to clickhouse
con <- dbConnect(RClickhouse::clickhouse(), 
                 host="", 
                 user = "",
                 password = "",
                 db = "")

# get currency rates
EURRUB <- cbr_rates_with_weekends("EUR", start_date = dates_report$from - 5)

# TODO  встроить позицию клика
# get yandex direct data
report <- yadirGetReport(DateRangeType = "CUSTOM_DATE", 
                                DateFrom = dates_report$from,
                                DateTo = dates_report$to,
                                FieldNames = c("Date",
                                               "CampaignId", 
                                               #"AvgClickPosition",
                                               "CampaignName", 
                                               "Criterion",
                                               "Impressions", 
                                               "Clicks",
                                               "Cost"),
                                Login         = acc,
                                TokenPath     = tok)

report[5:7] <- apply(report[5:7], 2, function(x) ifelse(is.na(x), 0, x))

# prepare yandex direct data, add cbr rates and calculate cost_rub and cpc
rep <- report %>% 
  mutate(keyword = normalize_keywords(Criterion)) %>% 
  group_by(Date, CampaignId, CampaignName, keyword) %>% 
  left_join(EURRUB, by = "Date") %>%
  summarise(impressions = sum(Impressions),
            click = sum(Clicks),
            cost_euro = sum(Cost),
            cost_ruble = (cost_euro * (Value * 100))[1],
            cpc_ruble = ifelse(click == 0, cost_ruble, cost_ruble/click),
            eur_rub = mean(Value * 100)) %>% 
  rename(date = Date, 
         campaignId = CampaignId,
         campaignName = CampaignName) %>% 
  ungroup() %>% 
  mutate(campaignId = as.character(campaignId))

# add data to clickhouse
# DBI::dbWriteTable(con, "yandex_reports_keys_test", rep, append=T, row.names=F)
