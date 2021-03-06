pop_profile_chart_fn <- function(locality_full, state_var, year = "2021", locality_name = "SAL") {
  # locality_full <- "Zilzie"
  # state_var <- "qld"
  # locality_full <- "Albany Creek"
  code_var <- paste0(locality_name, "_CODE_", year)
  name_var <- paste0(locality_name, "_NAME_", year)
  
  g17_combined_filtered <- g17_combined[state == state_var][, state := NULL]
  # ssc_7l <- ssc_first_7_letter
  
  # patch to select first suburb in case of duplicates
  # locality_full <- g17_combined_filtered[str_sub(SSC_NAME_2016, 1, 7) == ssc_7l, .(SSC_NAME_2016)][1]
  
  # Ignore columns by age
  tot_cols <- names(g17_combined_filtered)[
    str_sub(names(g17_combined_filtered), -3, -1) == "Tot"]
  
  name_var_index_lookup <- names(g17_combined_filtered) %>% match(name_var)
  name_var_index        <- which( name_var_index_lookup == '1') 
  if(year == "2021") {
    all_ages_selected_ssc <- g17_combined_filtered[
      g17_combined_filtered[[name_var_index]] == locality_full, ..tot_cols] %>% 
      melt(value.name = "population_count",
           variable.name = "income_bin") %>% 
      .[, gender := str_sub(income_bin, 1, 1)] %>% 
      .[str_sub(income_bin, -7, -1) != "Tot_Tot" & str_sub(income_bin, -7, -1) != "_ns_Tot" ] %>% 
      .[, tot_by_gender := sum(population_count)
        , .(gender)] %>% 
      .[, population_pp := round(population_count/tot_by_gender * 100, 2)
        , .(gender)]  } else {
    all_ages_selected_ssc <- g17_combined_filtered[
      g17_combined_filtered[[name_var_index]] == locality_full, ..tot_cols] %>% 
      melt(value.name = "population_count",
           variable.name = "income_bin") %>% 
      .[, gender := str_sub(income_bin, 1, 1)] %>% 
      .[str_sub(income_bin, -7, -1) != "Tot_Tot" & str_sub(income_bin, -7, -1) != "_ns_Tot" ] %>% 
      .[, tot_by_gender := sum(population_count)
        , .(gender)] %>% 
      .[, population_pp := round(population_count/tot_by_gender * 100, 2)
        , .(gender)]
  }
  

  
  ssc_median <- all_ages_selected_ssc[
    , `:=`(cumsum = cumsum(population_pp))
    , .(gender)][
      which(cumsum >=50 ), .SD[1], .(gender)]
  
  all_ages_selected_ssc[ssc_median, on = .(gender), median_income_bin := i.income_bin]
  
  levels_for_chart <- all_ages_selected_ssc[
    str_sub(income_bin, 1, 1) == "P", .(levels_for_chart = gsub("P_", "", income_bin))]
  
  ssc_chart_dt <- all_ages_selected_ssc[
    , income_bin_2 := str_sub(income_bin, 3, -1)][
      , weekly_income := factor(income_bin_2, levels = levels_for_chart$levels_for_chart)]
  
  p <- ssc_chart_dt %>% 
    ggplot(aes(weekly_income, population_pp, fill = gender)) +
    geom_col(position = "dodge") + 
    coord_flip() +
    theme_bw() +
    labs(title = 
           locality_full,
         caption = paste0(
           "Median: ",
           ssc_chart_dt[gender == "P", .(unique(as.character(median_income_bin)))],
           ", ",
           ssc_chart_dt[gender == "M", .(unique(as.character(median_income_bin)))],
           ", ",
           ssc_chart_dt[gender == "F", .(unique(as.character(median_income_bin)))],
           ". \n Total populatoin is: ",
           ssc_chart_dt[gender == "P", .(unique(tot_by_gender))]
         )) +
    scale_fill_brewer() +
    ylim(c(0, 50))
  
  p
}

