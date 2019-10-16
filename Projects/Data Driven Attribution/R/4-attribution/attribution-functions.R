source("R/1-base/base-functions.R")

base_agg_calculate <- function(df, by=c("agent", "tag", "campaign", "keyword")){
  df[,.(
    purchases = sum(was_payment),
    order_total = sum(order_total),
    profit = sum(profit),
    costs = sum(cpc),
    net_profit = sum(net_profit)
  ), by]
}

chains_df_generate <- function(df, val = 'net_profit', ...){
  # Create readable variables
  variables <- rlang::syms(c(...))
  
  df %<>% 
    mutate(grouping_var = with(df, paste(!!! variables, sep = "|"))) %>% 
    setDT()
  
  dt <- max(df[was_payment == 1]$date)
  
  df[,chain := paste(grouping_var, collapse = ' > '), 
     by = list(client_id, purchase_num)][
          ,.(
            conversions = sum(was_payment),
            value = sum(get(val)),
            null_val = sum(was_payment == 0),
            date = dt
          ), by = chain][, chain_length := str_count(chain, ">") + 1]
}

m_chain_calculate <- function(df, 
                              val = 'net_profit', 
                              order = 2,
                              chains_from = 1, ...){
  # TODO добавить тест на то, предобработаны ли данные
  
  # Create dataframe for markov_model
  mc_df <- chains_df_generate(df, val, ...)
  print_results("Chains df generated")
  
  # Calculation Markov Chain attribution
  mc_attr <- 
    markov_model(
      mc_df[chain_length > chains_from],
      var_path = "chain", 
      var_conv = "conversions",
      var_value = "value",
      var_null = "null_val",
      order = order, 
      out_more = TRUE,
      seed = 13
    )
  
  if(round(sum(mc_attr$result[[3]]), -1) != 
     round(sum(mc_df[chain_length > chains_from, value]), -1)){
    mc_attr$result %<>% 
      mutate(
        total_conversion_value = total_conversions / sum(total_conversions) 
        * sum(mc_df[chain_length > chains_from, value])
      )
  }
  
  mc_attr$result %<>% 
    # select(-total_conversions) %>% 
    rename(!!quo_name(paste0("mc_", val)) := total_conversion_value,
           channel = channel_name)
  print_results("Markov model calculated")
  
  mc_one_length <- 
    mc_df[chain_length == chains_from, .(chain, value, total_conversions = conversions)] %>% 
    rename(!!quo_name(paste0("mc_", val)) := value,
           channel = chain)
  
  res_df <- bind_rows(mc_attr$result, mc_one_length)
  res_df %<>% 
    group_by(channel) %>% 
    summarise_if(is.numeric, sum) %>% 
    setDT()
  print_results(paste0("Model and chains with length ", chains_from, " binded"))
  
  return(list(result = res_df, 
              removal_effects = mc_attr$removal_effects,
              transition_matrix = mc_attr$transition_matrix)
  )
}

attribution_calc <- function(df_prep, 
                             order = 2,
                             val = 'net_profit',
                             chains_from = 1,
                             by = c("agent", "tag", "campaign", "keyword")){
  res <- data.table()
  
  if(!is.data.table(df_prep))
    setDT(df_prep)
  
  for (a in df_prep[, unique(agent)]){
    print_results(paste0("Agent ", a, " started"))
    tmp_df_prep <- df_prep[parent_agent == a]
    mc_attr <- m_chain_calculate(tmp_df_prep, 
                                 chains_from = chains_from, 
                                 order = order,
                                 val = val,
                                 by=by)
    res <- rbindlist(list(res, mc_attr$result))
    print_results(paste0("Agent ", a, " finished. Now ", nrow(res), " rows"))
  }
  
  result <- res[, lapply(.SD, sum, na.rm = T), by=channel]
  return(result)
}

get_final_table <- function(df_prep, df_attr, by = "agent"){
    df_attr %>% 
    separate(channel, by, "\\|") %>% 
    group_by(!!! syms(by)) %>% 
    summarise_if(is.numeric, sum) %>% 
    ungroup() %>% 
    left_join(df_prep %>% 
                group_by(!!! syms(by)) %>% 
                summarise(
                  net_profit = sum(net_profit),
                  conversions = sum(was_payment),
                  net_profit_per_conv = round(net_profit / conversions, 2)
                ), by = by) %>% 
    mutate(
      diff = round(mc_net_profit, 1) - round(net_profit, 1),
      conv_shared = round(diff / abs(net_profit_per_conv)),
      p_diff = round(diff / abs(net_profit) * 100, 2)
    ) %>% 
    select(-total_conversions) %>% 
    setDT()
}

