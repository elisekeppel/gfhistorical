#-----------------------------------------------------------------------------
# 1. Extract catch data for all species of rockfish by year, fishery and major
#    area from GF_MERGED_CATCH
#-----------------------------------------------------------------------------
library(gfdata)
library(dplyr)
# arguments for future function:
spp <- 403
foreign <- FALSE
major <- c("01", "03", "04", "05", "06", "07", "08", "09")
refyears <- 1997:2005

# set some variables
trf <- gfdata::run_sql("GFFOS", "SELECT SPECIES_CODE
  FROM GFFOS.dbo.SPECIES
  WHERE SPECIES_COMMON_NAME LIKE '%ROCKFISH%' OR SPECIES_SCIENTIFIC_NAME LIKE '%SEBASTES%'")
trf <- trf$SPECIES_CODE
orf <- setdiff(trf, 396)

#-----------------------------------------------------------------------------
# get all catch records
#-----------------------------------------------------------------------------
gfmc_catch_data <- here::here("data/all_gfmc_rf_catch.rds")
if (file.exists(gfmc_catch_data)) {
  dat <- readRDS(gfmc_catch_data)
  } else {
    dat <- gfdata:::get_catch(trf)
    saveRDS(dat, "data/all_gfmc_rf_catch.rds")
  }

d <- dat

d$fishery_sector <- tolower(d$fishery_sector)
d <- d %>%
  select(database_name, fishery_sector, major_stat_area_code,
    major_stat_area_name, year, trip_id, fishing_event_id, gear, best_depth,
    species_code, species_common_name, landed_kg:discarded_pcs) %>%
  filter(major_stat_area_code %in% major) %>%
  mutate_if(is.numeric, list(~replace(., is.na(.),0))) %>%
  filter(!(landed_kg == 0 & landed_pcs == 0 & discarded_kg == 0 & discarded_pcs == 0))

# group fishery sectors
d <- d %>% mutate(
  fishery_sector = case_when(
    fishery_sector %in% c("rockfish inside", "rockfish outside", "zn", "k/zn") ~ "hlrock",
    fishery_sector %in% c("halibut", "k/l", "halibut and sablefish") ~ "halibut",
    fishery_sector %in% c("spiny dogfish", "lingcod", "schedule ii") ~ "dogling",
    fishery_sector %in% c("groundfish trawl") ~ "trawl",
    fishery_sector %in% c("sablefish") ~ "sable",
    fishery_sector %in% c("foreign") ~ "foreign"
  ))

# TO DO: need to work out what to do with foreign catch (will differ based on species and fishery)
if(!foreign){
  d <- d %>% filter(!fishery_sector %in% "foreign")
}
fishery <- unique(d$fishery_sector)


#-----------------------------------------------------------------------------
# estimate catch weights for catch counts
#-----------------------------------------------------------------------------
# calculate average wt/pc by year, fishery_sector and major area, and by
# fishery sector and major area, for each species to apply to piece-only data
# TO DO: should this be restricted to certain years?
avg_wt <- d %>%
  select(year, fishery_sector, major_stat_area_code, species_code, landed_kg, landed_pcs) %>%
  filter(landed_kg > 0 & landed_pcs > 0) %>%
  group_by(fishery_sector, major_stat_area_code, species_code) %>%
  mutate(landed_kg_per_pc = mean(landed_kg/landed_pcs)) %>%
  ungroup() %>%
  group_by(year, fishery_sector, major_stat_area_code, species_code, landed_kg_per_pc) %>%
  summarise(landed_kg_per_pc_annual = mean(landed_kg/landed_pcs)) %>%
  ungroup() %>%
  mutate(landed_kg_per_pc_annual = ifelse(is.na(landed_kg_per_pc_annual), 0, landed_kg_per_pc_annual)) %>%
  mutate(landed_kg_per_pc = ifelse(is.na(landed_kg_per_pc), 0, landed_kg_per_pc))

# apply avg_wt to landing and discard records only reporting pieces and not kg
d <- d %>%
  left_join(avg_wt, by = c("year", "fishery_sector", "major_stat_area_code", "species_code")) %>%
  mutate(
    est_landed_kg = ifelse(landed_kg == 0, ifelse(
      !landed_kg_per_pc_annual == 0, landed_pcs * landed_kg_per_pc_annual, landed_pcs * landed_kg_per_pc), landed_kg),
    est_discarded_kg = ifelse(discarded_kg == 0, ifelse(
      !landed_kg_per_pc_annual == 0, discarded_pcs * landed_kg_per_pc_annual, discarded_pcs * landed_kg_per_pc), discarded_kg)) %>%
  mutate(best_landed_kg = ifelse(!landed_kg == 0, landed_kg, est_landed_kg)) %>%
  mutate(best_discarded_kg = ifelse(!discarded_kg == 0, discarded_kg, est_discarded_kg))

#-----------------------------------------------------------------------------
# modern catch summary by sector, year, major area and species/group
#-----------------------------------------------------------------------------
# create modern catch based on trusted modern years of data by fishery (these are
# defaults from PBStools - should be discussed and adjusted for individual
# species or species groups)
hl_yr <- 1986
halibut_yr <- 2000
dogling_yr <- 2007
trawl_yr <- 1996
sable_yr <- 2007

modern_catch <- d %>% filter(
  (fishery_sector == "hlrock" & year >= hl_yr) |
    (fishery_sector == "halibut" & year >= halibut_yr) |
    (fishery_sector == "dogling" & year >= dogling_yr) |
    (fishery_sector == "trawl" & year >= trawl_yr) |
    (fishery_sector == "sable" & year >= sable_yr)
)


modern_catch_sum <- modern_catch %>%
  group_by(year, fishery_sector, major_stat_area_code, species_code) %>%
  mutate(trf = sum(best_landed_kg + best_discarded_kg),
    pop = sum(ifelse(species_code == 396, best_landed_kg + best_discarded_kg, 0))) %>%
  ungroup


#-----------------------------------------------------------------------------
# 2. calculate ratio of RRF to ORF for each fishery and area
# from modern_catch data (trusted years)
#-----------------------------------------------------------------------------

# calculate numerator for ratio of RRF to ORF (these ref years are
# defaults from PBStools)

# ref years for each fishery
# TO DO: shouldn't these align with (be after) trusted
# years for modern_catch, above? (and differ by fishery?)
hlrock_ref_yrs <- 1997:2005
halibut_ref_yrs <- 1997:2005
dogling_ref_yrs <- 1997:2005
trawl_ref_yrs <- 1997:2005
sable_ref_yrs <- 1997:2005

# reference catch - total catch over all reference years by major area
# and fishery
ref_catch <- d %>%
  filter(
    (fishery_sector == "hlrock" & year %in% hlrock_ref_yrs) |
      (fishery_sector == "halibut" & year %in% halibut_ref_yrs) |
      (fishery_sector == "dogling" & year %in% dogling_ref_yrs) |
      (fishery_sector == "trawl" & year %in% trawl_ref_yrs) |
      (fishery_sector == "sable" & year %in% sable_ref_yrs)
  ) %>%
  group_by(fishery_sector, major_stat_area_code, major_stat_area_name) %>%
  summarise(landed_kg = sum(best_landed_kg), discarded_kg = sum(discarded_kg))

# TO DO: should depth be included in this?
#----------------------------------------------------------------------------------------OK TO HERE

#TO DO: define and calculate TRF, ORF, POP and RRF catches
#TO DO: FUNCTIONALIZE!!!

# calculate denominator (ORF caught per area and fishery for trusted
# years of each fishery)


# TO DO: check Rowan's .rda file orfhistory.rda (might need to build orf from
# the big, nasty spreadsheets... or at least code the steps done to from some
# original documented historical catch file(s))

# list of rockfish species excluding POP
# TO DO: should we also exclude spp of interest in ORF?
orf_spp <- function(){
  d <- run_sql("GFFOS","SELECT SPECIES_CODE
    FROM SPECIES
    WHERE SPECIES_COMMON_NAME LIKE ('%ROCKFISH%')
    OR SPECIES_SCIENTIFIC_NAME LIKE ('%SEBASTES%')")
  names(d) <- tolower(names(d))
  d <- d %>% filter(species_code > 300) %>%
    filter(!species_code %in% c(396))
  d <- d$species_code
}

orf_catch <- gfdata:::get_historical_catch(orf_spp())

orf_catch$fishery_sector <- tolower(orf_catch$fishery_sector)

orf_catch <- orf_catch %>% mutate(
  fishery_sector = case_when(
    fishery_sector %in% c("rockfish inside", "rockfish outside", "zn", "k/zn") ~ "hlrock",
    fishery_sector %in% c("halibut", "k/l", "halibut and sablefish") ~ "halibut",
    fishery_sector %in% c("spiny dogfish", "lingcod", "schedule ii") ~ "dogling",
    fishery_sector %in% c("groundfish trawl") ~ "trawl",
    fishery_sector %in% c("sablefish") ~ "sable",
    fishery_sector %in% c("foreign") ~ "foreign"  # need to work out what to do with foreign catch (will differ based on species and fishery)
  )) %>%
  filter(major_stat_area_code %in% major)


orf_catch_by_area_and_fishery <- orf_catch %>%
  filter(
    (fishery_sector == "hlrock" & year %in% hlrock_ref_yrs) |
      (fishery_sector == "halibut" & year %in% halibut_ref_yrs) |
      (fishery_sector == "dogling" & year %in% dogling_ref_yrs) |
      (fishery_sector == "trawl" & year %in% trawl_ref_yrs) |
      (fishery_sector == "sable" & year %in% sable_ref_yrs)
  ) %>%
  group_by(fishery_sector, major_stat_area_code, major_stat_area_name) %>%
  summarise(landed_kg = sum(landed_kg), discarded_kg = sum(discarded_kg))


# calculate gamma ratios for each fishery and major area
for(i in fishery){
  for(j in major){
  x <- modern_catch_by_area_and_fishery %>%
    filter(fishery_sector == i & major_stat_area_code == j)
  y <- orf_catch_by_area_and_fishery %>%
    filter(fishery_sector == i & major_stat_area_code == j)
  assign(paste0("gamma_", i, "_area_", j), x$landed_kg/y$landed_kg)
}}



#-----------------------------------------------------------------------------
# need historical catch for ORF - Rowan has data object orfhistory in
# PBSdata, but we may wish to update this to reflect any database updates
# ** can select years other than trusted years from orf_catch for each fishery
#-----------------------------------------------------------------------------

# ** check values of gamma - many zeros


#
# 3. need modern catch for targetted species by fishery and discards of spp of
#   interest to calculate discard ratio for application to earlier years
#
# 4. need to get historical us cacth for ORF
