# Data source -------------------------------------------------------------
# https://www.abs.gov.au/census/find-census-data/datapacks?release=2016&product=GCP&geography=SA2&header=S

# Setup -------------------------------------------------------------------
rm(list = ls())
library(data.table)
library(tidyverse)
source("Pop_prof_fn.r")
source("Median_fn.r")

# Read in data ------------------------------------------------------------
ssc_mapping <- setDT(read.csv(("Inputs/SSC_2016_AUST.csv")))
ssc_mapping[, SSC_CODE_2016 := paste0("SSC", SSC_CODE_2016)]

g17a_nsw        <- setDT(read.csv("Inputs/2016Census_G17A_NSW_SSC.csv"))
g17b_nsw        <- setDT(read.csv("Inputs/2016Census_G17B_NSW_SSC.csv"))
g17c_nsw        <- setDT(read.csv("Inputs/2016Census_G17C_NSW_SSC.csv"))
g17_combined_nsw     <- merge(g17a_nsw, g17b_nsw, by = "SSC_CODE_2016") %>% merge(g17c_nsw, by = "SSC_CODE_2016") 
g17_combined_nsw[ssc_mapping, on = .(SSC_CODE_2016), SSC_NAME_2016 := i.SSC_NAME_2016][, state := "nsw"]

g17a_qld        <- setDT(read.csv("Inputs/2016Census_G17A_QLD_SSC.csv"))
g17b_qld        <- setDT(read.csv("Inputs/2016Census_G17B_QLD_SSC.csv"))
g17c_qld        <- setDT(read.csv("Inputs/2016Census_G17C_QLD_SSC.csv"))
g17_combined_qld     <- merge(g17a_qld, g17b_qld, by = "SSC_CODE_2016") %>% merge(g17c_qld, by = "SSC_CODE_2016") 
g17_combined_qld[ssc_mapping, on = .(SSC_CODE_2016), SSC_NAME_2016 := i.SSC_NAME_2016]
g17_combined_qld[ssc_mapping, on = .(SSC_CODE_2016), SSC_NAME_2016 := i.SSC_NAME_2016][, state := "qld"]

g17_combined <- rbind(g17_combined_nsw, g17_combined_qld) %>% .[, year := 2016]
write_rds(g17_combined, file.path("Outputs/g17_combined_2016.rds"))

# State suburb population distribution by age -----------------------------
# 
# pop_profile_chart_fn("Crows N", "nsw")
# pop_profile_chart_fn("Frenchs", "nsw")
# pop_profile_chart_fn("Eatons ", "qld")
# pop_profile_chart_fn("Albany ", "qld")

# Analysis - median -------------------------------------------------------

median_csv_fn("nsw", year = "2016", locality_name = "SSC")
median_csv_fn("qld", year = "2016", locality_name = "SSC")

check_qld <- setDT(read.csv("Outputs/2016qld_ranked_SSC.csv"))
check_nsw <- setDT(read.csv("Outputs/2016nsw_ranked_SSC.csv"))

nancy_suburb <- c("Arana H", "Bridgem", "Eatons ", "Everton", "The Gap", "McDowal",
                  "Virgini", "Albany ", "Aspley" , "Banyo"  , "Boondal", "Carseld",
                  "Chermsi", "Ferny H", "Geebung", "Keperra", "Zillmer", "Boondoo",
                  "The Gem", "The Gum")

nancy_suburb_full_letters <- check_qld[str_sub(SSC_NAME_2016, 1, 7) %in% nancy_suburb][order(-median_income_bin)][
  , .(SSC_NAME_2016)][[1]]

top_4_nsw <-check_nsw[
  median_income_bin %in% check_nsw[, .(unique(median_income_bin))][1:4][[1]]][
    , .(SSC_NAME_2016)][[1]]

top_4_nsw_7_l <- str_sub(top_4_nsw, 1, 7)

top_6_qld <-check_qld[
  median_income_bin %in% check_qld[, .(unique(median_income_bin))][1:6][[1]]][
    , .(SSC_NAME_2016)][[1]]

top_6_qld_7_l <- str_sub(top_6_qld, 1, 7)

# check_qld[str_sub(SSC_NAME_2016, 1, 7) %in% nancy_suburb][order(-median_income_bin)] %>% view()
qld_top_6 <- check_qld[str_sub(SSC_NAME_2016, 1, 7) %in% top_6_qld_7_l][order(-median_income_bin)]
qld_top_6[, .N, median_income_bin]
nsw_top_4 <- check_nsw[str_sub(SSC_NAME_2016, 1, 7) %in% top_4_nsw_7_l][order(-median_income_bin)]
nsw_top_4[, .N, median_income_bin]
check_nsw[str_sub(SSC_NAME_2016, 1, 7) %in% "Crows N"]
check_nsw[str_sub(SSC_NAME_2016, 1, 7) %in% "Frenchs"]

# Export charts ----------------------------------------------------------------

pdf("Outputs/selected_qld_2016.pdf")

list <- list()
for (i in seq_along(nancy_suburb_full_letters)) {
  print(nancy_suburb_full_letters[i])
  plot <- pop_profile_chart_fn(nancy_suburb_full_letters[i], "qld", year = "2016", locality_name = "SSC") 
 list <- list(list, plot)
}
list

dev.off()


pdf("Outputs/top_6_qld_2016.pdf")

list <- list()
for (i in seq_along(top_6_qld)) {
  plot <- pop_profile_chart_fn(top_6_qld[i], "qld", year = "2016", locality_name = "SSC") 
  list <- list(list, plot)
}
list

dev.off()


pdf("Outputs/top_4_nsw_2016.pdf")

list <- list()
for (i in seq_along(top_4_nsw)) {
  plot <- pop_profile_chart_fn(top_4_nsw[i], "nsw", year = "2016", locality_name = "SSC") 
  list <- list(list, plot)
}
list

dev.off()
   