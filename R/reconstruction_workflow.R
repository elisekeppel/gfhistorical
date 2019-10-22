load_all("../gfdata")
library(dplyr)

load("data/ph3dat.rda")
load("data/orfhistory.rda")
load("data/rrfhistory.rda")
spp = '442'

# use gfdata to get all rockfish catch from gffos
gfmc_rf_catch <- get_gfmc_rf_catch()

# glance at overall catch using gfplot
gfmc_rf_catch %>% filter(species_code == spp) %>% gfplot::tidy_catch() %>% gfplot::plot_catch()

# Calculate average wt per pc for each species by fishery and area
# Apply to records with pc's recorded only
avg_wt <- get_avg_wt(gfmc_rf_catch)
catch <- est_catch_by_pieces(gfmc_rf_catch, avg_wt = avg_wt)

# Get modern catch for trusted years
mod_catch <- get_mod_catch(catch)
mod_catch_sum <- get_mod_catch_sum(mod_catch)

# Subset modern catch by reference years for calculating ratios
ref_catch <- get_ref_catch(catch)
ratios <- get_ratios(prom = 'orf')




