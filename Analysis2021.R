# Data source -------------------------------------------------------------
# Census data
# https://www.abs.gov.au/census/find-census-data/datapacks?release=2021&product=GCP&geography=SA2&header=S
# SAL code to name mapping
# https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/allocation-files

# Setup -------------------------------------------------------------------
rm(list = ls())
library(data.table)
library(tidyverse)
library(readxl)
source("Pop_prof_fn.r")
source("Median_fn.r")

# Read in data ------------------------------------------------------------
SAL_mapping     <- setDT(read_excel(("Inputs/SAL_2021_AUST.xlsx")))
SAL_mapping_mod <- copy(SAL_mapping)[
  , .(SAL_CODE_2021, SAL_NAME_2021)][
    , SAL_CODE_2021 := paste0("SAL", SAL_CODE_2021)]

g17a_nsw        <- setDT(read.csv("Inputs/2021Census_G17A_NSW_SAL.csv"))
g17b_nsw        <- setDT(read.csv("Inputs/2021Census_G17B_NSW_SAL.csv"))
g17c_nsw        <- setDT(read.csv("Inputs/2021Census_G17C_NSW_SAL.csv"))
g17_combined_nsw     <- merge(g17a_nsw, g17b_nsw, by = "SAL_CODE_2021") %>% merge(g17c_nsw, by = "SAL_CODE_2021") 
g17_combined_nsw[SAL_mapping_mod, on = .(SAL_CODE_2021), SAL_NAME_2021 := i.SAL_NAME_2021][, state := "nsw"]

g17a_qld        <- setDT(read.csv("Inputs/2021Census_G17A_QLD_SAL.csv"))
g17b_qld        <- setDT(read.csv("Inputs/2021Census_G17B_QLD_SAL.csv"))
g17c_qld        <- setDT(read.csv("Inputs/2021Census_G17C_QLD_SAL.csv"))
g17_combined_qld     <- merge(g17a_qld, g17b_qld, by = "SAL_CODE_2021") %>% merge(g17c_qld, by = "SAL_CODE_2021") 
g17_combined_qld[SAL_mapping_mod, on = .(SAL_CODE_2021), SAL_NAME_2021 := i.SAL_NAME_2021]
g17_combined_qld[SAL_mapping_mod, on = .(SAL_CODE_2021), SAL_NAME_2021 := i.SAL_NAME_2021][, state := "qld"]

g17_combined <- rbind(g17_combined_nsw, g17_combined_qld) %>% .[, year := 2021]
write_rds(g17_combined, file.path("Outputs/g17_combined_2021.rds"))

# State suburb population distribution by age -----------------------------
# 
# pop_profile_chart_fn("Crows N", "nsw")
# pop_profile_chart_fn("Frenchs", "nsw")
# pop_profile_chart_fn("Eatons ", "qld")
# pop_profile_chart_fn("Albany ", "qld")

# Analysis - median -------------------------------------------------------

median_csv_fn("nsw")
median_csv_fn("qld")

check_qld <- setDT(read.csv("Outputs/2021qld_ranked_SAL.csv"))
check_nsw <- setDT(read.csv("Outputs/2021nsw_ranked_SAL.csv"))

nancy_suburb <- c("Arana H", "Bridgem", "Eatons ", "Everton", "The Gap", "McDowal",
                  "Virgini", "Albany ", "Aspley" , "Banyo"  , "Boondal", "Carseld",
                  "Chermsi", "Ferny H", "Geebung", "Keperra", "Zillmer", "Boondoo",
                  "The Gem", "The Gum")

nancy_suburb_full_letters <- check_qld[str_sub(SAL_NAME_2021, 1, 7) %in% nancy_suburb][order(-median_income_bin)][
  , .(SAL_NAME_2021)][[1]]

top_4_nsw <-check_nsw[
  median_income_bin %in% check_nsw[, .(unique(median_income_bin))][1:4][[1]]][
    , .(SAL_NAME_2021)][[1]]

top_4_nsw_7_l <- str_sub(top_4_nsw, 1, 7)

top_5_qld <-check_qld[
  median_income_bin %in% check_qld[, .(unique(median_income_bin))][1:5][[1]]][
    , .(SAL_NAME_2021)][[1]]

top_5_qld_7_l <- str_sub(top_5_qld, 1, 7)


# Total population --------------------------------------------------------

# Ignore columns by age
tot_cols <- names(g17_combined)[
  str_sub(names(g17_combined), -3, -1) == "Tot"]

tot_cols_w_name <- c("SAL_NAME_2021", "state", tot_cols)

all_ages_selected_ssc <- g17_combined[, ..tot_cols_w_name] %>% 
  melt(id.var = c("SAL_NAME_2021", "state"),
       value.name = "population_count",
       variable.name = "income_bin") %>% 
  .[, gender := str_sub(income_bin, 1, 1)] %>% 
  .[str_sub(income_bin, -7, -1) != "Tot_Tot" & str_sub(income_bin, -7, -1) != "_ns_Tot" ] %>% 
  .[, .(tot_by_gender = sum(population_count))
    , .(SAL_NAME_2021, state, gender)] 

all_ages_selected_ssc_both_g <- all_ages_selected_ssc[gender == "P"]

# DQ
sample <- all_ages_selected_ssc_both_g[, lapply(.SD, sum), by = "state", .SDcols = c("tot_by_gender")]
total <- g17_combined[, lapply(.SD, sum), by = "state", .SDcols = c("P_Tot_Tot")]

sample[total, on = .(state)][, diff:= (P_Tot_Tot-tot_by_gender) * 100 /P_Tot_Tot][]

# check_qld[str_sub(SAL_NAME_2021, 1, 7) %in% nancy_suburb][order(-median_income_bin)] %>% view()
qld_top_5 <- check_qld[
  all_ages_selected_ssc_both_g
  , on = .(SAL_NAME_2021)
  , tot_by_gender := i.tot_by_gender][
    SAL_NAME_2021 %in% top_5_qld][
      order(-median_income_bin)]

overview_qld <- qld_top_5[, .N, median_income_bin]
qld_top_5[, lapply(.SD, sum), .SDcols = c("tot_by_gender")]

nsw_top_4 <- check_nsw[
  all_ages_selected_ssc_both_g
  , on = .(SAL_NAME_2021)
  , tot_by_gender := i.tot_by_gender][
    SAL_NAME_2021 %in% top_4_nsw][
      order(-median_income_bin)]

overview_nsw <- nsw_top_4[, .N, median_income_bin]
nsw_top_4[, lapply(.SD, sum), .SDcols = c("tot_by_gender")]

check_nsw[str_sub(SAL_NAME_2021, 1, 7) %in% "Crows N"]
check_nsw[str_sub(SAL_NAME_2021, 1, 7) %in% "Frenchs"]

# Export charts ----------------------------------------------------------------

pdf("Outputs/selected_qld_2021.pdf")

list <- list()
for (i in seq_along(nancy_suburb_full_letters)) {
  # i = 1
  
  plot <- pop_profile_chart_fn(locality_full = nancy_suburb_full_letters[i], state_var = "qld") 
 list <- list(list, plot)
}
list

dev.off()


pdf("Outputs/top_5_qld_2021.pdf")

list <- list()
for (i in seq_along(top_5_qld)) {
  plot <- pop_profile_chart_fn(top_5_qld[i], "qld") 
  list <- list(list, plot)
}
list

dev.off()


pdf("Outputs/top_4_nsw_2021.pdf")

list <- list()
for (i in seq_along(top_4_nsw)) {
  plot <- pop_profile_chart_fn(top_4_nsw[i], "nsw") 
  list <- list(list, plot)
}
list

dev.off()

