median_csv_fn <- function (state, year = "2021", locality_name = "SAL"){
  # state <- "nsw"
  # locality_name <- "SAL"
  # year <- "2021"
  
  code_var <- paste0(locality_name, "_CODE_", year)
  name_var <- paste0(locality_name, "_NAME_", year)
  
  state_selected = state
  
  # Ignore columns by age
  g17_combined_filtered <- g17_combined[state == state_selected]
  
  tot_cols <- names(g17_combined_filtered)[
    str_sub(names(g17_combined_filtered), -3, -1) == "Tot"
    | str_sub(names(g17_combined_filtered), -3, -1) == str_sub(year, -3,-1)]
  
  # calculate population percentage by gender by bin
  if(year == "2021") {
    all_ages_long <- g17_combined_filtered[, ..tot_cols] %>% 
      melt(
        id.vars = c(name_var, code_var),
        value.name = "population_count",
        variable.name = "income_bin") %>% 
      .[, gender := str_sub(income_bin, 1, 1)] %>% 
      .[str_sub(income_bin, -7, -1) != "Tot_Tot" & str_sub(income_bin, -7, -1) != "_ns_Tot" ] %>% 
      .[, tot_by_gender := sum(population_count)
        , by = c("gender", code_var, name_var)] %>% 
      .[, population_pp := round(population_count/tot_by_gender * 100, 2)
        , by = c("gender", code_var, name_var)]
  } else {all_ages_long <- g17_combined_filtered[, ..tot_cols] %>% 
    melt(
      id.vars = c(name_var, code_var),
      value.name = "population_count",
      variable.name = "income_bin") %>% 
    .[, gender := str_sub(income_bin, 1, 1)] %>% 
    .[str_sub(income_bin, -7, -1) != "Tot_Tot" & str_sub(income_bin, -7, -1) != "_ns_Tot" ] %>% 
    .[, tot_by_gender := sum(population_count)
      , by = c("gender", code_var, name_var)] %>% 
    .[, population_pp := round(population_count/tot_by_gender * 100, 2)
      , by = c("gender", code_var, name_var)]}
  
  # calculate median by gender
  median_by_gender_by_age <- all_ages_long[
    , `:=`(cumsum = cumsum(population_pp))
    , by = c("gender", code_var, name_var)][
      which(cumsum >=50 ), .SD[1], by = c("gender", code_var, name_var)]
  
  all_ages_long[
    median_by_gender_by_age,
    on = c("gender", code_var, name_var),
    median_income_bin := i.income_bin]
  
  col_selected <- c(name_var, "median_income_bin", "gender")
  
  ranked_ssc <- copy(all_ages_long)[
    ! is.na(median_income_bin)
    & gender == "P"
    & tot_by_gender >=50
    , ..col_selected][
      order(median_income_bin, decreasing = TRUE)] %>%
    unique()
  
  write.csv(ranked_ssc, file = paste0("Outputs/", year, state_selected, "_ranked_", locality_name, ".csv"))

}
