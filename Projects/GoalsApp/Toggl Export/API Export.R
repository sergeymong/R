install.packages("togglr")
library(togglr)
require(dplyr)
open_toggl_website_profile()
set_toggl_api_token("611ee61412d25f39054c141e99d27a77")


data <- get_time_entries(since = Sys.time()- lubridate::weeks(3), until = Sys.time()- lubridate::weeks(2))

get_project_id_and_name()
