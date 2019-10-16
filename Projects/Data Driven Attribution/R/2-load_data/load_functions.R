source("R/1-base/base-functions.R")

split_dates <- function(start_date, end_date, chunks){
  # Чтобы количество чанков было таким же, какое задаётся в параметрах
  chunks <- chunks + 1
  
  # Чтобы получить хотя бы один сплит
  if(chunks < 2)
    chunks <- 2
  
  # Разбиваем промежуток на последовательность дат
  if(!is.Date(start_date))
    start_date <- as.Date(start_date)
  
  if(!is.Date(end_date))
    end_date <- as.Date(end_date)
  
  seq_dates <- seq(start_date, end_date, length.out = chunks)
  result <- sort(c(seq_dates[-c(1, chunks)] + 1, seq_dates))
  
  return(result)
}

load_from_ch <- function(dt, query, chunks = 10, depth = 14){
  query_dates <- split_dates(as.Date(dt[1]) - depth, dt[2], chunks)
  iter <- 0
  step <- 2
  
  # Загружаем по кусочкам, иначе упираемся в timeout
  for(i in seq(1, length(query_dates), by=step)){
    iter <- iter + 1
    date_range <- query_dates[i:(i+1)]
    q <- replace_expr(query, date_range, depth = 0)
    print_results(
      paste0(
        "Loading from ", as.character(date_range[1]), 
        " up to ", as.character(date_range[2])
      )
    )
    temp_df <- DBI::dbGetQuery(con("read"), q)
    
    if(iter == 1){
      df <- temp_df
    } else {
      df <- dplyr::bind_rows(df, temp_df)
    }
    
    print_results(paste0("Chunk ", iter, " passed"))
  }
  
  rows <- nrow(df)
  df <- distinct(df)
  print_results(paste0("Deleted ", rows - nrow(df), " duplicates"))
  test_orders_reference(df)
  setDT(df)
  return(df)
}
