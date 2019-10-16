source("R/1-base/base-functions.R")

check_is_dt <- function(df){
  if (!is.data.table(df)){
    print_results("Data was not data.table format. Setting")
    setDT(df)
  }
  
  if (!is.POSIXct(df$visit_time)){
    print_results("Data visit_time was not POSIXct format. Setting")
    df[, visit_time := fastPOSIXct(visit_time)]
  }
  
  if (!is.Date(df$date)){
    print_results("Data date was not Date format. Setting")
    df[, date := as.Date(date, "%Y-%m-%d")]
  }
  
  return(df)
}

booking_to_next <- function(book_at, next_at){
  sub_book_at <- substr(book_at, 1, 5)
  sub_next_at <- substr(next_at, 1, 5)
  is_book_empty <- nchar(book_at) == 0 | is.na(book_at)
  res <- next_at
  
  res[!is_book_empty & (sub_book_at != sub_next_at)] <- 
    book_at[!is_book_empty & (sub_book_at != sub_next_at)]
  
  return(res)
}

cpc_improve <- function(df){
  #  Удаляем cpc в каналах, где его быть не должно:
  #  больше 90% значений == 0
  
  df[, paid_clicks := sum(cpc == 0) / .N * 100, by = agent]
  print_results(paste0("Was removed ", 
                       round(df[paid_clicks > 90, sum(cpc)][[1]]), 
                       " junk costs"))
  df[paid_clicks > 90, cpc := 0]
  df[, paid_clicks := NULL]
}

normalize_keywords <- function(vec){
  del <- function(x) str_remove_all(x, "\\s-[а-яА-ЯёЁ\\d\\w!]+")
  match <- function(x) str_extract_all(x, "[а-яА-ЯёЁ\\w\\d\\-\\.]+")
  join <- function(x) sapply(x, paste0, collapse = " ")
  normalize <- function(x) str_squish(tolower(x))
  
  return (normalize(join(match(del(vec)))))
}

create_purchase_number <- function(df){
  print_results("Creating purchase number")
  df <- check_is_dt(df)
  
  # Сортируем данные, чтобы корректно заполнять номер покупки
  setorder(df, client_id, visit_time)
  df[, purchase_num := NA]
  print_results("Data ordered")
  
  # Создаём номер покупки для каждого client_id
  df[,purchase_num := as.integer(purchase_num)]
  df[was_payment == 1, purchase_num := cumsum(was_payment), by = client_id]
  print_results("Added purchase number")
  
  # Чтобы заливка сработала корректно,
  # берём только группы пользователей, в которых последняя строка NA
  # и заменяем её на 0. Так сможем отфильтровать визиты после покупок
  df[, last_na := is.na(purchase_num[.N]), by = client_id]
  indx <- df[(last_na), .I[.N], by = client_id]$V1
  df[indx, purchase_num := as.integer(0), by = client_id]
  df[, last_na := NULL]
  print_results("Last purchase number replaced")
  
  df[,purchase_num := na.locf(purchase_num, fromLast = T)]
  print_results("Purchase number filled")
  
  return(df)
}

create_max_purchase <- function(df){
  df <- check_is_dt(df)
  
  if("purchase_num" %ni% names(df)){
    print_results("In 'create_max_purchase' not found purchase_num. Creating")
    df <- create_purchase_number(df)
  }

  df[, max_p := max(purchase_num), by = client_id]
  
  return(df)
}

remove_visits_after_purchase <- function(df){
  df <- check_is_dt(df)
  
  if("max_p" %ni% names(df)){
    print_results("In 'remove_visits_after_purchase' not found max_p. Creating")
    df <- create_max_purchase(df)
  }

  visits <- nrow(df) - df[(purchase_num == 0 & max_p == 0) | 
                            (max_p > 0 & purchase_num > 0)][,.N][[1]]
  df <- df[(purchase_num == 0 & max_p == 0) | (max_p > 0 & purchase_num > 0)]
  print_results(paste0("Removed ", 
                       visits, 
                       " visits after purchase"))
  
  return(df)
}

remove_chains_from_other_period <- function(df){
  df <- check_is_dt(df)
  if("max_p" %ni% names(df)){
    print_results("In 'remove_chains_from_other_period' not found max_p. Creating")
    df <- create_max_purchase(df)
  }
  
  # Для цепочек без покупок дата покупки - просто дата
  df[max_p == 0, purchase_date := date]
  
  # Для цепочек с покупками выбираем дату, когда произошла покупка
  df[max_p > 0, purchase_date := date[was_payment == 1], 
     by = list(client_id, purchase_num)]
  
  purchases_before <- sum(df[['was_payment']])
  end_date <- max(df[was_payment == 1]$date)
  start_month <- floor_date(end_date, "month")
  start_date <- min(df[was_payment == 1 & date >= start_month]$date)
  df <- df[purchase_date >= start_date & purchase_date <= end_date]
  df[, purchase_date := NULL]
  print_results(paste0("Deleted ", purchases_before - sum(df[['was_payment']]), 
                       " purchases from other period"))
  
  return(df)
}

create_parent_agent <- function(df){
  df <- check_is_dt(df)
  
  if("max_p" %ni% names(df))
    df <- create_max_purchase(df)
  
  # Донор (родительский агент) - агент, в рамках которого произошла покупка
  df[max_p == 0, parent_agent := agent]
  df[max_p > 0, parent_agent := agent[was_payment == 1], 
     by=list(client_id, purchase_num)]
  print_results("Added parent agent")
  
  return(df)
}

delete_fraud_clicks <- function(df){
  df <- check_is_dt(df)
  
  setorder(df, client_id, visit_time)
  print_results("DT ordered")
  
  df[, `:=`(
    num_visit = as.numeric(visit_time),
    agtk = paste(agent, tag, campaign, keyword, sep = "|")
  )]
  print_results("Created 'agent tag campaign keyword'(agtk) & numeric visit time")
  
  df[, diff_visit_time := (num_visit - c(NA, num_visit[-.N])) / 60, by=client_id]
  print_results("Created diff_last_visit_time")
  
  df[, agtk_prev := c(NA, agtk[-.N]), by=client_id]
  print_results("Created agtk_prev")
  
  df[, is_same_agtk := agtk == agtk_prev, by=client_id]

  df[is_same_agtk & diff_visit_time < 1 & agent %like% 'context', cpc := 0]
  print_results("Deleted fraud context clicks")
  
  raw_df[, `:=`(
    num_visit = NULL,
    agtk = NULL,
    diff_visit_time = NULL,
    agtk_prev = NULL,
    is_same_agtk = NULL
  )]
  print_results("Deleted tmp columns")
}

preprocess_table <- function(df, ...){
  df <- check_is_dt(df)
  test_orders_reference(df)
  test_profit_reference(df)
  input_df <- copy(df)
  
  # Создаём сущность "покупка"
  df <- create_purchase_number(df)
  test_equal_output(input_df, df, "was_payment", "was_payment")
  test_equal_output(input_df, df, "profit", "profit")
  
  # Удаляем скликивания
  delete_fraud_clicks(df)
  
  # Замещаем next agent-а на booking agent-а
  df[,`:=`(
    agent = booking_to_next(book_agent, agent),
    tag = booking_to_next(book_tag, tag)
  )]
  
  # Создаём сущность "партнёр"
  df[is_partner(agent) & !is_meta(agent), agent := 'partner']
  
  # Удаляем ненужные расходы
  cpc_improve(df)
  df[, `:=`(
    book_agent=NULL,
    book_tag=NULL
  )]
  print_results("Agents was swaped, cpc was improved, partner was created")
  
  df <- remove_visits_after_purchase(df)
  test_equal_output(input_df, df, "was_payment", "was_payment")
  test_equal_output(input_df, df, "profit", "profit")
  
  df <- remove_chains_from_other_period(df)
  test_equal_output(input_df, df, "was_payment", "was_payment")
  
  df[, net_profit := profit - cpc]
  
  df <- create_parent_agent(df)
  
  # Удаляем max_p, потому что он не нужен дальше
  if('max_p' %in% names(df))
    df[, max_p := NULL]
  
  df[, keyword:=normalize_keywords(keyword)]
  
  df[tag == '', tag:=NA]
  df[campaign == '', campaign:=NA]
  df[keyword == '', keyword:=NA]
  
  # Убеждаемся, что ничего лишнего не удалили
  test_equal_output(input_df, df, "was_payment", "was_payment")
  test_equal_output(input_df, df, "profit", "profit")
  test_orders_reference(df)
  test_profit_reference(df, thresh = 0.01)
  
  return(df)
}
