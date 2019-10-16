require(RClickhouse)
require(DBI)

con <- dbConnect(RClickhouse::clickhouse(), 
                 host="", 
                 user = "",
                 password = "",
                 db = "")


max_date <- DBI::dbGetQuery(con, "SELECT MAX(date)
                                    FROM context_ads.`yandex_reports_keys`")[[1]]

if (max_date == Sys.Date() - 1){
  cat('Yandex Direct report successfully loaded!')
} else {
  cat(' Something went wrong, max date is', as.character(max_date), 'but expected', as.character(max_date + 1), '\n Check it!')
}
