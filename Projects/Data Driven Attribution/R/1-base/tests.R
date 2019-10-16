source("R/1-base/connect-packages.R")

# TODO добавить тест на то, предобрботаны ли данные

base_test_check <- function(df, ...){
  
  if (all(c("was_payment", ...) %ni% names(df)))
    stop("Dataset should with 'was_payment' column.")
  
  if (all(c("date", ...) %ni% names(df)))
    stop("Dataset should with 'date' column.")
  
  if (all(c("profit", ...) %ni% names(df)))
    stop("Dataset should with 'profit' column.")
  
  if(!is.data.table(df))
    setDT(df)
  
  return(df)
}

check_state <- function(statement=NULL, msg_pass=NULL, msg_fail=NULL){
  # parent.frame важен для того, чтобы функция видела переменные, 
  # объявленные в другой функции
  test_expr <- eval(parse(text=statement), envir = parent.frame())
  if (test_expr){
    print_results(paste0("TEST PASSED. ", msg_pass))
  } else {
    stop(paste0("TEST NOT PASSED. ", msg_fail))
  }
}

test_orders_reference <- function(df, thresh = 0.015){
  df <- base_test_check(df)

  end_date <- max(df[was_payment == 1]$date)
  # сравниваем консистентонсть заказов только в текущем периоде
  start_month <- floor_date(end_date, "month")
  start_date <- min(df[was_payment == 1 & date >= start_month]$date)
  orders_df <- sum(df[date >= start_date]$was_payment)
  orders_real <- DBI::dbGetQuery(
    con("read"), 
    glue(
    "
    SELECT uniq(token)
    FROM `backend.booking.issuing.success`
    WHERE 
      date BETWEEN '{start_date}' AND '{end_date}'
      AND visit_id != toUUID(0)
    "
    )
  )[[1]]
  
  msg_pass <- paste0("Data has equal(±", thresh * 100,"%) orders")
  msg_fail <- paste0("Data has less orders, than exists in real world")
  check_state("orders_df + orders_real * thresh >= orders_real", 
              msg_pass,
              msg_fail)
}

test_profit_reference <- function(df, thresh = 0.015){
  df <- base_test_check(df)
  
  end_date <- max(df[was_payment == 1]$date)
  # сравниваем консистентонсть заказов только в текущем периоде
  start_month <- floor_date(end_date, "month")
  start_date <- min(df[was_payment == 1 & date >= start_month]$date)
  profit_df <- sum(df[date >= start_date]$profit)
  profit_real <- DBI::dbGetQuery(
    con("read"), 
    glue(
      "
    SELECT SUM(profit) / 100
    FROM `backend.booking.issuing.success`
    WHERE 
      date BETWEEN '{start_date}' AND '{end_date}'
      AND visit_id != toUUID(0)
    "
    )
  )[[1]]
  
  msg_pass <- paste0("Data has equal(±", thresh * 100,"%) profit")
  msg_fail <- paste0("Data has less profit, than exists in real world")
  check_state("profit_df + profit_real * thresh >= profit_real &
              profit_df - profit_real * thresh <= profit_real", 
              msg_pass,
              msg_fail)
}

test_equal_output <- function(df_input, df_output, col_inp, col_out){
  df_input <- base_test_check(df_input, col_inp, col_out)
  df_output <- base_test_check(df_output, col_inp, col_out)
  
  end_date <- max(df_input[was_payment == 1][['date']])
  start_month <- floor_date(end_date, "month")
  start_date <- min(df_input[was_payment == 1 & date >= start_month][['date']])
  input <- sum(df_input[date >= start_date][[col_inp]])
  output <- sum(df_output[date >= start_date][[col_out]])
  
  msg_pass <- paste0("Input/output data has equal ", col_inp)
  msg_fail <- paste0("Input/output has not equal ", col_inp)
  check_state("input == output",
              msg_pass,
              msg_fail)
}
