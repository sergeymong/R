source("R/1-base/base-functions.R")

upload_to_ch <- function(connect = con(), agg_df, dt = floor_date(today(), unit = 'm')){
  exist_table <- DBI::dbGetQuery(con(), "EXISTS next_gen_attribution_campaigns")[[1]]
  
  if(!as.logical(exist_table)){
    print_results("Table not exsits, creating")
    DBI::dbGetQuery(con(), "CREATE TABLE IF NOT EXISTS next_gen_attribution_campaigns
              (
                date Date,
                agent LowCardinality(String),
                tag LowCardinality(String),
                campaign LowCardinality(String),
                keyword String,
                purchases UInt32,
                profit Float32,
                costs UInt32,
                net_profit Float32,
                attribution_net_profit Float32
              )
              ENGINE = MergeTree
              ORDER BY (date, agent, tag, campaign, keyword)")
  }
    
  
  agg_df %<>% 
    tibble::add_column(date = dt, .before = 'agent') %>% 
    select(-matches('ROI|difference'))
  
  DBI::dbWriteTable(connect,
               "next_gen_attribution_campaigns",
               agg_df,
               append=T, 
               row.names=F
               )
  
  print_results("Data uploaded")
}
