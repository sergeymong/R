source("R/2-load_data/SQL_queries.R")
source("R/2-load_data/load_functions.R")
source("R/3-preprocessing/preprocessing-functions.R")
source("R/4-attribution/attribution-functions.R")
source("R/5-upload/upload-functions.R")

from <- as.Date("2019-05-01")
to <- as.Date("2019-06-01")
step <- "month"
dates_seq <- sort(c(seq(from, to, by = step), rollback(seq(from, to, by = step))))
dates_seq <- dates_seq[-c(1, length(dates_seq))]

for (i in seq(1, length(dates_seq), by = 2)){
  print_results(paste("Started from", dates_seq[i], "to", dates_seq[i+1]))
  
  dates_report <- c(dates_seq[i], dates_seq[i+1])
  depth <- 14
  chunks <- 8
  dt <- as.Date(max(dates_report))
  
  # raw_df <- load_from_ch(dates_report, query = raw_query, depth = depth, chunks = chunks)
  # write_fst(raw_df, paste0("output/", dt, ".fst"))
  raw_df <- read_fst(paste0("output/", dt, ".fst"), as.data.table = T)
  tmp <- copy(raw_df)
  delete_fraud_clicks(tmp)
  
  df_prep <- preprocess_table(raw_df)
  rm(raw_df)
}
  df_attr <- attribution_calc(df_prep)
  
  # load data
  pre_load_df <- 
    df_prep %>% 
    group_by(agent, tag, campaign, keyword) %>% 
    summarise_if(is.numeric, sum) %>% 
    select(-purchase_num, -order_total) %>% 
    rename(
      purchases = was_payment,
      costs = cpc
    ) %>% 
    left_join(
      df_attr %>% 
        select(-total_conversions) %>% 
        separate(channel, c("agent", "tag", "campaign", "keyword"), "\\|") %>% 
        na_if("NA"), 
      by = c("agent", "tag", "campaign", "keyword")
    ) %>% 
    select(agent, tag, campaign, keyword, purchases, everything()) %>% 
    rename(attribution_net_profit = mc_net_profit)
  
  pre_load_df[is.na(pre_load_df)] <- ""
  
  # upload_to_ch(agg_df = pre_load_df, dt = dt)
  # print_results(paste("Uploaded from", dates_seq[i], "to", dates_seq[i+1]))
}

system("say Sergey, your script was finished")
