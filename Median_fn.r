median_csv_fn <- function (state){
  
  state_selected = state
  
  # Ignore columns by age
  g17_combined_filtered <- g17_combined[state == state_selected]
  
  tot_cols <- names(g17_combined_filtered)[
    str_sub(names(g17_combined_filtered), -3, -1) == "Tot"
    | str_sub(names(g17_combined_filtered), -3, -1) == "016"]
  
  # calculate population percentage by gender by bin
  all_ages_long <- g17_combined_filtered[, ..tot_cols] %>% 
    melt(
      id.vars = c("SSC_NAME_2016", "SSC_CODE_2016"),
      value.name = "population_count",
      variable.name = "income_bin") %>% 
    .[, gender := str_sub(income_bin, 1, 1)] %>% 
    .[str_sub(income_bin, -7, -1) != "Tot_Tot" & str_sub(income_bin, -7, -1) != "_ns_Tot" ] %>% 
    .[, tot_by_gender := sum(population_count)
      , .(gender, SSC_CODE_2016, SSC_NAME_2016)] %>% 
    .[, population_pp := round(population_count/tot_by_gender * 100, 2)
      , .(gender, SSC_CODE_2016, SSC_NAME_2016)]
  
  # calculate median by gender
  median_by_gender_by_age <- all_ages_long[
    , `:=`(cumsum = cumsum(population_pp))
    , .(gender, SSC_CODE_2016, SSC_NAME_2016)][
      which(cumsum >=50 ), .SD[1], .(gender, SSC_CODE_2016, SSC_NAME_2016)]
  
  all_ages_long[
    median_by_gender_by_age,
    on = .(gender, SSC_CODE_2016, SSC_NAME_2016),
    median_income_bin := i.income_bin]
  
  ranked_ssc <- all_ages_long[
    ! is.na(median_income_bin)
    & gender == "P"
    & tot_by_gender >=50
    , .(SSC_NAME_2016, median_income_bin, gender)][
      order(median_income_bin, decreasing = TRUE)] %>%
    unique()
  
  write.csv(ranked_ssc, file = paste0("Outputs/", state_selected, "_ranked_ssc.csv"))

}
