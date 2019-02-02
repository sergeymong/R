require(googlesheets)
suppressMessages(require(dplyr))
require(togglr)
require(lubridate)

# this is first step -- you register table
goals <- gs_title("Copy of Цели 2019")

#check size
goals$ws[c(3:5)]

Tasks <- goals %>% gs_read(ws = "Tasks", range = cell_rows(1:goals$ws$row_extent[2])) # get nedeed for us data
Habits <- goals %>% gs_read(ws = "Habits", range = cell_rows(1:goals$ws$row_extent[4])) # get nedeed for us data

# # START MODULE SPRINTS
# sprints <- read_sheet(goals_table, 'Sprint')
# tasks <- read_sheet(goals_table, "Tasks")
# 
# add_total_work_time <- function(sleep=9, sprint_data){
#   sprint_data$`Total work time` <-  hms::hms(period_to_seconds(days(dmy(sprint_data$`End Date`) - dmy(sprint_data$`Start Date`))) -
#                                                period_to_seconds(days(dmy(sprint_data$`End Date`) - dmy(sprint_data$`Start Date`)))*sleep/24)
#   sprint_data
# }
# s <- add_total_work_time(sprint_data = sprints)
# 
# t <- tasks
# t$Estimate <- ifelse(is.na(t$Estimate), "00:00:00", t$Estimate)
# t$Estimate <- hms::hms(period_to_seconds(hms(t$Estimate)))
# t$Actual[12] <- hms::hms(0)
# t_es <- t %>% 
#   group_by(Sprint) %>% 
#   filter(!is.na(Sprint)) %>% 
#   summarise(Estimate = sum(Estimate),
#             Actual = sum(Actual),
#             Tasks = n())
# 
# # mutate will be better
# s <- left_join(s, t_es, by = c('Sprint ID' = 'Sprint'))

# Estimate time
# Сумма оцениваемого времени по всем задачам спринта
# Actual Time
# Сколькоо времени потратил по факту
# Tasks start
# Сколько было задач в спринте в начале
# Tasks end
# Сколько было задач в спринте по завершении
# Tasks wasted
# Сколько задач перенесено на след.спринт
# Efficiency
# Время в развитии и работе относительно нецелевого (процент)
# Time of working
# Сколько часов проработал (категория)
# Time of learning
# Сколько часов проучился (категория)
# Time of practicing
# Сколько часов попрактиковался (категория)
# Time wasted
# Чистое нецелевое время
# Completion
# Завершён ли спринт
# END MODULE SPRINTS

# RUN GOALS END
# 
# predictions (unsuccesful)
# forlm <- tasks %>% filter(Status == "Done")
# model <- lm(data = forlm, period_to_seconds(hms(Estimate)) ~ period_to_seconds(hms(forlm$Actual))+`Task priority`)
# 
# summary(model)
# 
# tasks$prediction_Estimate <- predict(model, newdata = tasks)
# 
# 
# period_to_seconds(hms(forlm$Actual))

#cronR::cron_add(cronR::cron_rscript('GoalsApp.R'), frequency = 'daily', at = '17:30')