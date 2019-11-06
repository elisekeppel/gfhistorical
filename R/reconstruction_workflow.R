devtools::load_all("../gfdata")
library(dplyr)
library(tidyverse)
library(tidyr)
library(readxl)
#source("R/gfcatch.R")

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
# Now subset modern catch for species of interest
mod_catch_sum <- get_mod_catch_sum(mod_catch)


# Subset modern catch by reference years for calculating ratios
ref_catch <- get_ref_catch(catch)
ratios <- get_ratios(prom = 'orf')
# These ratios can be applied to historic ORF, POP or TRF landings to
#   estimate historic RRF catch

#-------------------------------------------------------------------------------
# Historic catch
#-------------------------------------------------------------------------------

# Create orf_history from Rowan Haigh's "Catch-Historical.xlx" spreadsheet (orfhistory.R).
# Spreadsheet contains historic catch values from various sources.
# Code converts to consistent units (kg) and organizes by major, fishery, year and nation.
# For US catch, code calculates ratios of US ORF catch by area (from Ketchen76, Stewart)
#   and calculates historic catch by year using ratios.
# Some data are addititve while others are redundant (of which the max will be used).
# Contains historic Cdn, US and foreign catch of ORF.
# Also contains some POP (396) records.
orf_history <- get_orf_history()

# TO DO: currently orf_history includes other majors than 1, 3:9
# TO DO: re-code gfcatch (pre and post 1954) & pacharv3 for orf_history (in 1-orfhistory.R)
# for now, use RH orfhistory gfcatch & pacharv3
# see Haigh and Yamanaka 2011 Appendix A for RH queries

# TO DO: currently includes other major areas (0, 2, 10, 67, 68) and regions ("3B", "6", "3A", "4A", ...)
# likely want to remove these as not in Cdn waters
# not sure if RH removed at any point in his code

# TO DO: check if catch = (landings + discards) or landings only reported in orf_history? orfhistory?
# TO DO: decide on consistent converstion factor b/w lbs and kg (Yamanaka uses 2.205/2.20459, Ketchen/Stewart use 2.20462/2.204623...)
# for now keeping as close to RH as possible

#-------------------------------------------------------------------------------



# LATER: bring in "modern" discards by fishery, year
# bring in "historic" discards (calculate ratio from trusted years)

# deal with foreign catch

# apply ratios to historic catch

# have modern and historic catch in same format

# merge all data sources together
