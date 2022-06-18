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

g17_combined <- rbind(g17_combined_nsw, g17_combined_qld)

# State suburb population distribution by age -----------------------------
# 
# pop_profile_chart_fn("Crows N", "nsw")
# pop_profile_chart_fn("Frenchs", "nsw")
# pop_profile_chart_fn("Eatons ", "qld")
# pop_profile_chart_fn("Albany ", "qld")

# Analysis - median -------------------------------------------------------

median_csv_fn("nsw")
median_csv_fn("qld")

check_qld <- setDT(read.csv("Outputs/qld_ranked_ssc.csv"))
check_nsw <- setDT(read.csv("Outputs/nsw_ranked_ssc.csv"))

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

# Export charts ----------------------------------------------------------------

pdf("Outputs/selected_qld.pdf")

list <- list()
for (i in seq_along(nancy_suburb_full_letters)) {
  plot <- pop_profile_chart_fn(nancy_suburb_full_letters[i], "qld") 
 list <- list(list, plot)
}
list

dev.off()


pdf("Outputs/top_6_qld.pdf")

list <- list()
for (i in seq_along(top_6_qld)) {
  plot <- pop_profile_chart_fn(top_6_qld[i], "qld") 
  list <- list(list, plot)
}
list

dev.off()


pdf("Outputs/top_4_nsw.pdf")

list <- list()
for (i in seq_along(top_4_nsw)) {
  plot <- pop_profile_chart_fn(top_4_nsw[i], "nsw") 
  list <- list(list, plot)
}
list

dev.off()
