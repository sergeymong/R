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

as.Date(paste0(names(Habits)[6],"/2019"))

rowSums(Habits[-c(2:5)])

dmy(names(Habits)[6]) - today()
Habits[1,c(8:35)] <- FALSE
Habits[1,c(36:148)] <- T
some <- rle(Habits[1,-c(1:5)])
results <- some$values
results <- rbind(results, some$lengths)
x <- as.numeric(as.Date(names(results)[2], tryFormats = "%d/%m/%Y") - Sys.Date())

rle



# Create Toggl Timesheet
set_toggl_api_token("611ee61412d25f39054c141e99d27a77")

data <- get_time_entries(since = Sys.time()- lubridate::weeks(1), until = Sys.time())
work_data <- data %>% group_by(project_name, description) %>% summarise(duration = sum(duration))
work_data$duration <- seconds_to_period(work_data$duration)
work_data$time <- paste0(hour(work_data$duration) + day(work_data$duration)*24,":", minute(work_data$duration),":", second(work_data$duration))
work_data <- work_data[-3]
#work_data$time <- ifelse(is.na(times(work_data$time)), work_data$time, times(work_data$time))



# rewrite 
some <- left_join(Tasks, work_data, by=c("PROJECT" = "project_name", "NAME" = "description"))
some[is.na(some$time),] <- "00:00:00"


some$ACTUAL <- ifelse(is.na(some$time), as.character(some$ACTUAL), some$time)


goals %>% gs_edit_cells(ws = "Tasks", anchor = "K2", input = some$ACTUAL)







