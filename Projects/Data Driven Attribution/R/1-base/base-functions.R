source("R/1-base/connect-packages.R")
source("R/1-base/tests.R")

'%ni%' <- Negate('%in%')

con <- function(type = "write"){
  if (type == "write"){
    DBI::dbConnect(
      RClickhouse::clickhouse(), 
      host="", 
      user = "",
      password = "",
      db = ""
    )
  } else {
    DBI::dbConnect(
      RClickhouse::clickhouse(), 
      host="", 
      user = "",
      password = "",
      db = ""
    )
  }
}

mutate_cond <- function(.data, condition, ..., envir = parent.frame()){
    condition <- eval(substitute(condition), .data, envir)
    .data[condition, ] <- .data[condition, ] %>% mutate(...)
    .data
}

replace_expr <- 
  function(query, dates, depth = NULL, agent = NULL, funnel_type = NULL){
    query <- gsub("\\{\\{ Период.start \\}\\}", dates[1], query)
    query <- gsub("\\{\\{ Период.end \\}\\}", dates[2], query)
    
    if(!is.null(agent))
      query <- gsub("\\{\\{ Agent \\}\\}", agent, query)
    
    if(!is.null(depth))
      query <- gsub("\\{\\{ Глубина \\}\\}", depth, query)
    
    if(!is.null(funnel_type))
      query <- gsub("\\{\\{ funnel_type \\}\\}", funnel_type, query)
    
    query <- gsub("\\\\", "\\\\\\\\", query)
    
    return(query)
  }

print_results <- function(message = "Something without dot"){
  time <- str_extract(capture.output(Sys.time()), "[\\d-]+\\s[\\d{2}:-]+")
  cat(time, paste0("- ", message, ".\n"))
}

is_partner <- function(agent){
  '%ni%' <- Negate('%in%')
  
  agent %ni% c(
    
  )
}

is_meta <- function(agent){
  agent %in% c()
}
  
  