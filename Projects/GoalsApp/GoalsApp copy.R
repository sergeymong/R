# Ver 1
# подумать о механике, как считать часы и делать предсказания на основе линейной регрессии

require(lubridate)
suppressMessages(require(dplyr))
require(formattable)
require(togglr)
require(googlesheets)

# START MODULE BUDGET & BILLS
### Include export from Toggl ###
read_toggle <- function(period){
  set_toggl_api_token("1ade723dc312352135eafc0c30758d199")
  cost <- 750
  data <- get_time_entries(since = period[[1]], until = period[[2]])
  data <- data[c(1, 4, 5, 6, 11, 15)]
  names(data) <- c('Date', 'Duration', 'Project', 'Description', 'Billable', 'Tag')
  data$Billable <- ifelse(data$Project == "Работа", 'TRUE', 'FALSE') 
  data$Date <- as.Date(data$Date)
  data$Time <- hms::hms(data$Duration)
  data$Duration <- seconds_to_period(data$Duration)
  data$Amount <- currency(ifelse(data$Billable == 'TRUE', 
                                 hour(data$Duration) * cost + 
                                   minute(data$Duration)/60 * cost + 
                                   second(data$Duration)/60/60 * cost,
                                 0), 'RUB ')
  data <- na.exclude(data)
  return(data)
}
back_to_week <- function(which_week=1, weeks=1){
  d1 <- which_week * lubridate::days(7)
  d2 <- weeks * lubridate::days(7)
  from <- Sys.time() - d1
  to <- from + d2
  return(c(start=from, end=to))
}
how_much <- function(data){
  data %>%
    group_by(Date, Description) %>% 
    summarise(Price = round(sum(Amount)), Hours = sum(Time)) %>% 
    ungroup() %>% 
    select(Description, Price, Hours) %>% 
    filter(Price > 0) %>% 
    janitor::adorn_totals()
}
# module works in Bills and budget.Rmd
# END MODULE BUDGET & BILLS

# START BASE MODULE GOOGLE SHEETS
goals_table <- gs_title("Цели 2019 2.0") # this is first required step -- registering table
names_of_sheets <- gs_ws_ls(goals_table) 
read_sheet <- function(reg_table, name){
  size <- goals_table$ws[goals_table$ws$ws_title == name, c(4:5)]
  table <- reg_table %>% gs_read(ws = name, range = cell_rows(1:size[[1]]))
  table
}
# END BASE MODULE GOOGLE SHEETS

# START MODULE HABITS STREAK
calculate_streaks <- function(data){
  data <- data[-c(1:7)]
  col_limit <- which(as.Date(names(data), tryFormats = "%d/%m/%Y") == (Sys.Date()-1))
  for_streaks <- data[1:col_limit]
  streaks <- lapply(1:nrow(for_streaks), function(row_x) rbind(rle(for_streaks[row_x, ])$values, 
                                                         rle(for_streaks[row_x, ])$lengths))
   
  result <- sapply(streaks, function(x) 
    if (rev(x)[1,1] == 0) {
      current = 0 
      max_st = max(x[2, which(x[1,] == 1)])
      strength = current/66
      data =c(current, max_st, strength)
    } else {
      current = rev(x)[2,1] 
      max_st = max(x[2, which(x[1,] == 1)])
      strength = current/66
      data = c(current, max_st, strength)
      })
  
  rownames(result) <- c("Streak", "Max streak", "Result")
  result <- as.data.frame(t(result))
  result[[3]] <- formattable::percent(result[[3]])
  result <- tidyr::unnest(result)
  result <- result[c(2, 3, 1)]
  result
}

write_habits_result <- function(reg_table, result){
  anchors <- c("C2", "D2", "E2")
  for (i in 1:length(anchors)){
    reg_table %>% gs_edit_cells(ws = "Habits", anchor = anchors[[i]], input = result[[i]])
    Sys.sleep(0.3)
  }
  cat('Table was updated!')
}
# END MODULE HABITS STREAK

# START MODULE ACTUAL ESTIMATE
calculate_actual_time <- function(tasks, time) {
  time <- time %>% 
    group_by(Description) %>% 
    summarise(time = sum(Time))
  
  work_table <- left_join(tasks, time, by = c("Full name" = "Description"))
  work_table$time <- ifelse(is.na(work_table$time), 0, work_table$time)
  work_table$Actual <- ifelse(is.na(work_table$Actual), 0, work_table$Actual)
  work_table$Actual <- ifelse(work_table$time > work_table$Actual, work_table$time, work_table$Actual)
  work_table$Actual <- hms::hms(work_table$Actual)
  return(work_table[['Actual']])
  work_table
}
write_tasks_time <- function(reg_table, result){
  reg_table %>% gs_edit_cells(ws = "Tasks", anchor = "K2", input = result)
  cat('Table was updated!')
}

# RUN GOALS START

# Habits start
habits <- read_sheet(goals_table, "Habits")
write_habits_result(goals_table, calculate_streaks(habits))
# Habits end

# Actual estimate start
tasks <- read_sheet(goals_table, "Tasks")
time <- read_toggle(back_to_week(10, 10))
write_tasks_time(goals_table, calculate_actual_time(tasks, time))
# Actal estimate end

# END MODULE ACTUAL ESTIMATE
