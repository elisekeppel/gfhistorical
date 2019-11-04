devtools::load_all("../gfdata")
library(dplyr)
library(tidyverse)
library(tidyr)
library(readxl)
source("R/gfcatch.R")

load("data/ph3dat.rda")
load("data/orfhistory.rda")
load("data/rrfhistory.rda")
spp = '442'

# use gfdata to get all rockfish catch from gffos merged catch table
# need to source gfcatch.R
gfmc_rf_catch <- get_gfmc_rf_catch()

# glance at overall catch using gfplot
gfmc_rf_catch %>% filter(species_code == spp) %>% gfplot::tidy_catch() %>% gfplot::plot_catch()
# sum(gfmc_rf_catch$landed_kg==0)

# Calculate average wt per pc for each species by fishery and area (keep only
# avg wt's falling within 90% CI of all avg wts (by species) to avoid bloated
# catch estimates from misaligned catch wt and ct)
avg_wt <- get_avg_wt(gfmc_rf_catch)

# Apply avg wt (by spp, fid and major) to records with pc's recorded only
# to estiamte catch, and create best catch with reported kg when available, else
# estimated kg
# (apply to both landed_pcs and discarded_pcs)
catch <- est_catch_by_pieces(gfmc_rf_catch, avg_wt = avg_wt)
# sum(catch$best_landed_kg==0)

# Get modern catch (landings) for trusted years
mod_catch <- get_mod_catch(catch)
mod_catch_sum <- get_mod_catch_sum(mod_catch)

# Subset modern catch by reference years for calculating ratios
ref_catch <- get_ref_catch(catch)
ratios <- get_ratios(prom = 'orf')

#-------------------------------------------------------------------------------
# Historic catch
#-------------------------------------------------------------------------------

# create orfhistory
# US historic catch from Rowan Haigh "Catch-Historical.xlx" spreadsheet
# calculate ratios of US ORF catch by area
# calculate historic catch by year using ratios

# in US_historical_catch.R
us_orf_catch <- get_orf_history() # this is the source = Stewart data from Rowan's orfhistory object

sum_orfhistory <- orfhistory %>%
  filter(source %in% c("Leaman80",
    "Ketchen76", "Ketchen80", "Obradovich", "Yamanaka", "Stewart")) %>%
 # mutate(landings = catch/2.20462) %>%
  group_by(source) %>%
  summarise(n())

sum_orf <- orf %>%
  filter(source %in% c("leaman80",
    "ketchen76", "ketchen80", "obradovich", "yamanaka", "stewart")) %>%
  # mutate(landings = catch/2.20462) %>%
  group_by(source) %>%
  summarise(n())

### CHECK UNITS - MY SUMMARY SEEMS OFF BY A FACTOR OF 1000 FROM ROWAN'S (THINK MY UNITS KG, ALREADY CHECKED. CHECK ROWAN'S)
#----------------------------------------------------------------------
sum_orfhistory <- orfhistory %>%
  filter(source == "Yamanaka") %>%
  # mutate(landings = catch/2.20462) %>%
  # group_by(year, spp, major, nation, source, action, fishery) %>%
  summarise(first_year = min(year), last_year = max(year), sum(catch), n())

sum_orf <- orf %>%
  filter(source == "yamanaka") %>%
  # mutate(landings = catch/2.20462) %>%
  # group_by(year, spp, major, nation, source, action, fishery) %>%
  summarise(first_year = min(year), last_year = max(year), sum(landings), n())

sum_orfhistory <- orfhistory %>%
  filter(source == "Yamanaka") %>%
  # mutate(landings = catch/2.20462) %>%
  group_by(major) %>%
  summarise(first_year = min(year), last_year = max(year), sum(catch), n())

sum_orf <- orf %>%
  filter(source == "yamanaka") %>%
  # mutate(landings = catch/2.20462) %>%
  group_by(major) %>%
  summarise(first_year = min(year), last_year = max(year), sum(landings), n())

### TO DO NEXT : check units for yamanaka (tonnes in spreadsheet?), obradovich (tonnes in spreadsheet?) (ALL sources)

# (first_year = min(year), last_year = max(year), sum(catch),
# year, spp, major, nation, source, action, fishery
#
# sum_yamanaka <- orf_yamanaka %>%
#   #group_by(spp, major, region, nation, action, fishery) %>%
#   summarise(first_year = min(year), last_year = max(year), sum(landings), n())

# bring in pacharv3 data


# bring in US data available by year, fishery, area (POP?)



#-------------------------------------------------------------------------------



# LATER: bring in "modern" discards by fishery, year
# bring in "historic" discards (calculate ratio from trusted years)

# deal with foreign catch

# merge all data sources together
