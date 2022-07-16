rm(list = ls())
library(data.table)
library(tidyverse)
library(readxl)

# Read in data ------------------------------------------------------------

g17_combined_2016 <- readRDS("Outputs/g17_combined_2016.rds") %>% setDT() %>% .[, suburb_name := SSC_NAME_2016]
g17_combined_2021 <- readRDS("Outputs/g17_combined_2021.rds") %>% setDT() %>% .[, suburb_name := SAL_NAME_2021]

check_qld_2021 <- setDT(read.csv("Outputs/2021qld_ranked_SAL.csv")) %>% 
  .[, `:=`(
    year = 2021,
    state = "qld",
    suburb_name = SAL_NAME_2021)]

check_nsw_2021 <- setDT(read.csv("Outputs/2021nsw_ranked_SAL.csv")) %>% 
  .[, `:=`(
    year = 2021,
    state = "nsw",
    suburb_name = SAL_NAME_2021)]

check_qld_2016 <- setDT(read.csv("Outputs/2016qld_ranked_SSC.csv")) %>% 
  .[, `:=`(
    year = 2016,
    state = "qld",
    suburb_name = SSC_NAME_2016)]

check_nsw_2016 <- setDT(read.csv("Outputs/2016nsw_ranked_SSC.csv")) %>% 
  .[, `:=`(
    year = 2016,
    state = "nsw",
    suburb_name = SSC_NAME_2016)]

# Total population --------------------------------------------------------

# Icalculate total population by suburb for 2016
tot_cols_16 <- names(g17_combined_2016)[
  str_sub(names(g17_combined_2016), -3, -1) == "Tot"]

tot_cols_w_name <- c("suburb_name", "state", tot_cols_16)

all_ages_selected_ssc_16 <- g17_combined_2016[, ..tot_cols_w_name] %>% 
  melt(id.var = c( "suburb_name", "state"),
       value.name = "population_count",
       variable.name = "income_bin") %>% 
  .[, gender := str_sub(income_bin, 1, 1)] %>% 
  .[str_sub(income_bin, -7, -1) != "Tot_Tot" & str_sub(income_bin, -7, -1) != "_ns_Tot" ] %>% 
  .[, .(tot_by_gender = sum(population_count))
    , .( suburb_name, state, gender)] 

all_ages_selected_ssc_16_both_g <- all_ages_selected_ssc_16[gender == "P"]

# calculate total population by suburb for 2021
tot_cols_16 <- names(g17_combined_2021)[
  str_sub(names(g17_combined_2021), -3, -1) == "Tot"]

tot_cols_w_name <- c("suburb_name", "state", tot_cols_16)

all_ages_selected_ssc_21 <- g17_combined_2021[, ..tot_cols_w_name] %>% 
  melt(id.var = c( "suburb_name", "state"),
       value.name = "population_count",
       variable.name = "income_bin") %>% 
  .[, gender := str_sub(income_bin, 1, 1)] %>% 
  .[str_sub(income_bin, -7, -1) != "Tot_Tot" & str_sub(income_bin, -7, -1) != "_ns_Tot" ] %>% 
  .[, .(tot_by_gender = sum(population_count))
    , .( suburb_name, state, gender)] 

all_ages_selected_ssc_21_both_g <- all_ages_selected_ssc_21[gender == "P"]

# DQ -----------------------------
sample21 <- all_ages_selected_ssc_21_both_g[, lapply(.SD, sum), by = "state", .SDcols = c("tot_by_gender")]
total21 <- g17_combined_2021[, lapply(.SD, sum), by = "state", .SDcols = c("P_Tot_Tot")]
sample21[total21, on = .(state)][, diff:= (P_Tot_Tot-tot_by_gender) * 100 /P_Tot_Tot][]

sample16 <- all_ages_selected_ssc_16_both_g[, lapply(.SD, sum), by = "state", .SDcols = c("tot_by_gender")]
total16 <- g17_combined_2016[, lapply(.SD, sum), by = "state", .SDcols = c("P_Tot_Tot")]
sample16[total16, on = .(state)][, diff:= (P_Tot_Tot-tot_by_gender) * 100 /P_Tot_Tot][]


# Define top suburbs ------------------------------------------------------

top_nsw_2021 <-check_nsw_2021[
  median_income_bin %in% check_nsw_2021[, .(unique(median_income_bin))][1:5][[1]]][
    , .(suburb_name)][[1]]

top_qld_2021 <-check_qld_2021[
  median_income_bin %in% check_qld_2021[, .(unique(median_income_bin))][1:5][[1]]][
    , .(suburb_name)][[1]]


top_nsw_2016 <-check_nsw_2016[
  median_income_bin %in% check_nsw_2016[, .(unique(median_income_bin))][1:5][[1]]][
    , .(suburb_name)][[1]]

top_qld_2016 <-check_qld_2016[
  median_income_bin %in% check_qld_2016[, .(unique(median_income_bin))][1:7][[1]]][
    , .(suburb_name)][[1]]

setdiff(top_nsw_2016, top_nsw_2021)
setdiff(top_qld_2016, top_qld_2021)
setdiff(top_qld_2021, top_qld_2016)


# Details on top bands and count of suburbs/people qld ------------------------

qld_top_2021 <- check_qld_2021[
  all_ages_selected_ssc_21_both_g
  , on = .(suburb_name)
  , tot_by_gender := i.tot_by_gender][
    suburb_name %in% top_qld_2021][
      order(-median_income_bin)]

overview_qld_21 <- qld_top_2021[, .N, median_income_bin]
overview_qld_21
qld_top_2021[, lapply(.SD, sum), .SDcols = c("tot_by_gender")]

qld_top_2016 <- check_qld_2016[
  all_ages_selected_ssc_16_both_g
  , on = .(suburb_name)
  , tot_by_gender := i.tot_by_gender][
    suburb_name %in% top_qld_2016][
      order(-median_income_bin)]

overview_qld_16 <- qld_top_2016[, .N, median_income_bin]
overview_qld_16
qld_top_2016[, lapply(.SD, sum), .SDcols = c("tot_by_gender")]

# Details on top bands and count of suburbs/people nsw ------------------------

nsw_top_2021 <- check_nsw_2021[
  all_ages_selected_ssc_21_both_g
  , on = .(suburb_name)
  , tot_by_gender := i.tot_by_gender][
    suburb_name %in% top_nsw_2021][
      order(-median_income_bin)]

overview_nsw_2021 <- nsw_top_2021[, .N, median_income_bin]
overview_nsw_2021
nsw_top_2021[, lapply(.SD, sum), .SDcols = c("tot_by_gender")]

nsw_top_2016 <- check_nsw_2016[
  all_ages_selected_ssc_16_both_g
  , on = .(suburb_name)
  , tot_by_gender := i.tot_by_gender][
    suburb_name %in% top_nsw_2016][
      order(-median_income_bin)]

overview_nsw_2016 <- nsw_top_2016[, .N, median_income_bin]
overview_nsw_2016
nsw_top_2016[, lapply(.SD, sum), .SDcols = c("tot_by_gender")]

check_nsw_2021[str_sub(suburb_name, 1, 7) %in% "Crows N"]
check_nsw_2021[str_sub(suburb_name, 1, 7) %in% "Frenchs"]
