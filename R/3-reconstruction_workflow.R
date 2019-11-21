
spp = '403'

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

# Get modern catch (landings) for trusted years (currently using PBS default for first trusted year for each fishery)
mod_catch <- get_mod_catch(catch,
  trawl_yr = 1996,
  halibut_yr = 2000,
  sable_yr = 2007,
  dogling_yr = 2007,
  hl_yr = 1986)
# Now subset modern catch for species of interest by year, fid and major
mod_catch_sum <- get_mod_catch_sum(mod_catch)


# Subset catch by reference years for calculating ratios ---> TO DO: these should maybe be adjusted by the 'trusted' modern catch above??
ref_catch <- get_ref_catch(catch,
  rrf = 442,
  trawl_ref_yrs = 1997:2005,
  halibut_ref_yrs = 1997:2005,
  sable_ref_yrs = 1997:2005,
  dogling_ref_yrs = 1997:2005,
  hlrock_ref_yrs = 1997:2005)
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
orf_history <- get_orf_history() # extra nations have been filtered out (need to filter majors for only BC waters, nations for CA and US catches, TO DO: discards not yet considered)

#-------------------------------------------------------------------------------
# combine historic data sources for final annual catch by fishery, major area
#-------------------------------------------------------------------------------

# select maximum catch from redundant data sources for each year, fishery, major, nation
# TO DO: do we need to keep/use the other majors?
orf_history_max <- orf_history %>%
  filter(spp == 391, major %in% c(1,3,4,5,6,7,8,9,0), action == 'max') %>% #filter out pop records TO DO: some of these may be necessary to bring in from before pop was sep'd out
  group_by(year, major, nation, fishery, source) %>%
  summarise(catch_kg = sum(catch)) %>%
  ungroup() %>%
  group_by(year, major, nation, fishery) %>%
  slice(which.max(catch_kg)) %>%
  ungroup()

# roll up catch from additive data sources by
orf_history_add <- orf_history %>%
  filter(spp == 391, major %in% c(1,3,4,5,6,7,8,9,0), action == 'add') %>%
  group_by(year, major, nation, fishery, source) %>%
  summarise(catch_kg = sum(catch)) %>%
  ungroup()

# combine max and additive historic data sources for complete orf history
orf_history_complete <- rbind(orf_history_max, orf_history_add) %>%
  group_by(year, major, fishery) %>%
  summarise(orf_kg = sum(catch_kg))



# apply ratios for rrf to orf
# first estimate fid 2,4,5 catch (halibut, dogfish/lingcod and rf catch) by applying beta to h&l fishery catch
hl <- orf_history_complete %>%
  filter(fishery == "h&l") %>%
  right_join(ratios %>% select(fid, major, beta) %>% filter(fid %in% c(2,4,5))) %>%
  mutate(orf_kg = orf_kg * beta)

# bring expanded hl fishery data back into orf_hisroty
rrf_history <- orf_history_complete %>%
  filter(!fishery == "h&l") %>%
  mutate(fid =
      case_when(
        fishery == "trawl" ~ 1,
        fishery == "trap" ~ 3,
        fishery == "combined" ~ 9)) %>%
  union(hl %>% select(-beta)) %>%
  left_join(ratios %>% select(-beta)) %>%
  mutate(est_catch_kg =
      case_when(
        major == 0 ~ orf_kg * gamma * alpha,
        !major == 0 ~ orf_kg * gamma)) %>%
  ungroup()


#-------------------------------------------------------------------------------
# combine historic and modern catch
#-------------------------------------------------------------------------------

# arrange modern and historical catch data
mod_catch_input <- mod_catch_sum %>%
  select(year, fid, major, catch_kg = landed_kg)
hist_catch_input <- rrf_history %>%
  select(year, fid, major, catch_kg = est_catch_kg)

# combine
d <- rbind(mod_catch_input, hist_catch_input) %>%
  mutate(fishery =
      case_when(
        fid == 1 ~ "Trawl",
        fid == 2 ~ "Halibut",
        fid == 3 ~ "Sablefish",
        fid == 4 ~ "Dogfish/Lingcod",
        fid == 5 ~ "HL_Rockfish",
        fid == 9 ~ "Combined"
      )) %>%
  mutate(major_stat_area =
      case_when(
        major == 0 ~ "unknown",
        major == 1 ~ "4B",
        major == 3 ~ "3C",
        major == 4 ~ "3D",
        major == 5 ~ "5A",
        major == 6 ~ "5B",
        major == 7 ~ "5C",
        major == 8 ~ "5D",
        major == 9 ~ "5E"
      ))
# take a peek

c <- rev(unique(d$major_stat_area))
pal <- RColorBrewer::brewer.pal(n = length(c), name = "Set1")
year_range <- c(min(d$year), max(d$year))


d %>%
  filter(!is.na(major_stat_area)) %>%
  ggplot(aes(year, catch_kg, colour = major_stat_area, fill = major_stat_area)) +
  geom_col() +
  theme_pbs() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_fill_manual(values = pal, drop = FALSE, breaks = c,
    labels = c) +
  scale_colour_manual(values = pal, drop = FALSE, breaks = c,
    labels = c) +
  scale_x_discrete(breaks = seq(1920, year_range[2], 5)) +
  facet_wrap(vars(fishery), scales = "free")
#-------------------------------------------------------------------------------
# STILL TO DO FOR ORFHISTORY
#-------------------------------------------------------------------------------
# TO DO: re-code gfcatch (pre and post 1954) & pacharv3 for orf_history (in 1-orfhistory.R)
# for now, use RH orfhistory gfcatch & pacharv3
# see Haigh and Yamanaka 2011 Appendix A for RH queries

# TO DO: currently includes other major areas (0, 2, 10, 67, 68) and regions ("3B", "6", "3A", "4A", ...)
# likely want to remove these as not in Cdn waters
# not sure if RH removed at any point in his code

# TO DO: Why are pacharvhl, pacharvsable, others included in orfhistory? no old years?

# TO DO: could recalculate Lynne's and Ketchen/Stewart values with consistent use of conversion factors/rounding
# TO DO: check if catch = (landings + discards) or landings only reported in orf_history? orfhistory?
# TO DO: decide on consistent converstion factor b/w lbs and kg (Yamanaka uses 2.205/2.20459, Ketchen/Stewart use 2.20462/2.204623...)
# for now keeping as close to RH as possible
# TO DO: for 1918 to 1949, ORF = TRF (POP not yet reported separately)
# need to use regression of ORF against TRF landings 1953-1995 data excluding
# anomalous 1966 data point and transform data into log2 space to reduce positive skew (RH pg. 5)

#-------------------------------------------------------------------------------
# Apply ratios to orfhistory
#-------------------------------------------------------------------------------

# Apply ratios to historic catch

# 1. filter orf_history for major in (1, 3:9) & orf(/pop/trf) - currently using orf only

# 2. apply beta to catch for each major where fishery is h&l, then apply gamma
# 3. apply gamma to catch for each combo of fid and major (except major = 0)
# 4. apply alpha and gamma to catch for each fid where major = 0

# TO DO:
# 5. apply ?lambda? to catch where fid = 9, then also gamma (and alpha if major = 0)
# 6. bring in "modern" discards by fishery, year
# bring in "historic" and modern discards (calculate ratio from trusted years)
# deal with foreign catch (allow for inclusion in historic catch)
