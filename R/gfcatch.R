#-----------------------------------------------------------------------------
# 1. Extract catch data for all species of rockfish by year, fishery and major
#    area from GF_MERGED_CATCH
#-----------------------------------------------------------------------------
library(gfdata)
library(dplyr)
# arguments for future function:
rrf <- 403



# set some variables
trf <- gfdata::run_sql("GFFOS", "SELECT SPECIES_CODE
  FROM GFFOS.dbo.SPECIES
  WHERE SPECIES_COMMON_NAME LIKE '%ROCKFISH%' OR SPECIES_SCIENTIFIC_NAME LIKE '%SEBASTES%'")
trf <- trf$SPECIES_CODE
orf <- setdiff(trf, 396)

#-----------------------------------------------------------------------------
# get all catch records
#-----------------------------------------------------------------------------
#' Title
#'
#' @param foreign
#' @param major
#'
#' @return
#' @export
#'
#' @examples
get_gfmc_rf_catch <- function(foreign = FALSE,
  major = c("01", "03", "04", "05", "06", "07", "08", "09")) {
  gfmc_catch_data <- here::here("data/all_gfmc_rf_catch.rds")
  if (file.exists(gfmc_catch_data)) {
  d <- readRDS(gfmc_catch_data)
  } else {
    d <- gfdata:::get_catch(trf) %>%
      select(database_name, fishery_sector, major_stat_area_code,
        major_stat_area_name, year, trip_id, fishing_event_id, gear, best_depth,
        species_code, species_common_name, landed_kg:discarded_pcs) %>%
      filter(major_stat_area_code %in% major) %>%
      mutate_if(is.numeric, list(~replace(., is.na(.),0))) %>%
      filter(!(landed_kg == 0 & landed_pcs == 0 & discarded_kg == 0 & discarded_pcs == 0))

    # group fishery sectors
    d$fishery_sector <- tolower(d$fishery_sector)
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
    saveRDS(d, "data/all_gfmc_rf_catch.rds")
  }
}

# gfmc_rf_catch <- get_gfmc_rf_catch()
# gfmc_rf_catch %>% filter(species_code == spp) %>% tidy_catch() %>% plot_catch()
# fishery <- unique(gfmc_rf_catch$fishery_sector)

#-----------------------------------------------------------------------------
# estimate catch weights for catch count records
#-----------------------------------------------------------------------------
# calculate average wt/pc by year, fishery_sector and major area, and by
# fishery sector and major area, for each species to apply to piece-only data
# TO DO: should this be restricted to certain years?
#' Title
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
get_avg_wt <- function(dat = gfmc_rf_catch){
  dat %>%
    select(year, fishery_sector, major_stat_area_code, species_code, landed_kg, landed_pcs) %>%
    filter(landed_kg > 0 & landed_pcs > 0) %>%
    group_by(fishery_sector, major_stat_area_code, species_code) %>%
    mutate(landed_kg_per_pc = mean(landed_kg/landed_pcs)) %>%
    ungroup() %>%
    group_by(year, fishery_sector, major_stat_area_code, species_code, landed_kg_per_pc) %>%
    summarise(landed_kg_per_pc_annual = mean(landed_kg/landed_pcs)) %>%
    ungroup() %>%
    mutate_if(is.numeric, list(~replace(., is.na(.),0)))
}

# avg_wt <- get_avg_wt(gfmc_rf_catch)

# apply avg_wt to landing and discard records only reporting pieces and not kg
#' Title
#'
#' @param dat
#' @param avg_wt
#'
#' @return
#' @export
#'
#' @examples
est_catch_by_pieces <- function(dat = gfmc_rf_catch, avg_wt = avg_wt){
  dat %>%
    left_join(avg_wt, by = c("year", "fishery_sector", "major_stat_area_code", "species_code")) %>%
    mutate_if(is.numeric, list(~replace(., is.na(.),0))) %>%
    mutate(
      est_landed_kg = ifelse(landed_kg == 0, ifelse(
        !landed_kg_per_pc_annual == 0, landed_pcs * landed_kg_per_pc_annual, landed_pcs * landed_kg_per_pc), landed_kg),
      best_landed_kg = ifelse(!landed_kg == 0, landed_kg, est_landed_kg),
      est_discarded_kg = ifelse(discarded_kg == 0, ifelse(
        !landed_kg_per_pc_annual == 0, discarded_pcs * landed_kg_per_pc_annual, discarded_pcs * landed_kg_per_pc), discarded_kg),
      best_discarded_kg = ifelse(!discarded_kg == 0, discarded_kg, est_discarded_kg),
      best_catch = best_landed_kg + best_discarded_kg
    )
}

# catch <- est_catch_by_pieces(gfmc_rf_catch)
#-----------------------------------------------------------------------------
# modern catch summary by sector, year, major area and species/group
#-----------------------------------------------------------------------------
# create modern catch based on trusted modern years of data by fishery (these are
# defaults from PBStools - should be discussed and adjusted for individual
# species or species groups)

#' Get modern rockfish catch - data for trusted years by each fishery
#'
#' @param dat
#' @param hl_yr
#' @param halibut_yr
#' @param dogling_yr
#' @param trawl_yr
#' @param sable_yr
#'
#' @return
#' @export
#'
#' @examples
get_mod_catch <- function(dat = catch,
  hl_yr = 1986,
  halibut_yr = 2000,
  dogling_yr = 2007,
  trawl_yr = 1996,
  sable_yr = 2007){
  dat %>% filter(
    (fishery_sector == "hlrock" & year >= hl_yr) |
      (fishery_sector == "halibut" & year >= halibut_yr) |
      (fishery_sector == "dogling" & year >= dogling_yr) |
      (fishery_sector == "trawl" & year >= trawl_yr) |
      (fishery_sector == "sable" & year >= sable_yr)
  )
}

# mod_catch <- get_mod_catch(catch)

#' Get modern catch summary
#'
#' All rockfish catch summarised by year, fishery sector and major area for
#' trusted years of modern data
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
get_mod_catch_sum <- function(dat = mod_catch){
  dat %>%
    group_by(year, fishery_sector, major_stat_area_code) %>%
    summarise(rrf_kg = sum(ifelse(species_code == rrf, best_catch, 0)),
      orf_kg = sum(ifelse(!species_code == 396, best_catch, 0)),
      pop_kg = sum(ifelse(species_code == 396, best_catch, 0)),
      trf_kg = sum(best_catch)) %>%
    ungroup()
}

# mod_catch_sum <- get_mod_catch_sum(mod_catch)

#-----------------------------------------------------------------------------
# 2. calculate ratio of RRF to ORF for each fishery and area
# (from data for reference years by fishery)
#-----------------------------------------------------------------------------

# reference catch - total catch over all reference years by major area
# and fishery (these ref years are defaults from PBStools)

# TO DO: shouldn't these align with (be after) trusted
# years for modern_catch, above? (and differ by fishery?)

#' Get reference catch
#' Reference catch from modern years to derive ratios of RRF to species/species
#' groups (eg. POP, TRF (total rockfish), ORF (other rockfish than POP)) to
#' apply to historical catch of species/species groups and estimate historical
#' catches of RRF.
#'
#' @param dat
#' @param hlrock_ref_yrs
#' @param halibut_ref_yrs
#' @param dogling_ref_yrs
#' @param trawl_ref_yrs
#' @param sable_ref_yrs
#'
#' @return
#' @export
#'
#' @examples
get_ref_catch <- function(dat = catch,
  hlrock_ref_yrs = 1997:2005,
  halibut_ref_yrs = 1997:2005,
  dogling_ref_yrs = 1997:2005,
  trawl_ref_yrs = 1997:2005,
  sable_ref_yrs = 1997:2005) {
  dat %>% filter(
    (fishery_sector == "hlrock" & year %in% hlrock_ref_yrs) |
      (fishery_sector == "halibut" & year %in% halibut_ref_yrs) |
      (fishery_sector == "dogling" & year %in% dogling_ref_yrs) |
      (fishery_sector == "trawl" & year %in% trawl_ref_yrs) |
      (fishery_sector == "sable" & year %in% sable_ref_yrs)
  ) %>%
    group_by(fishery_sector, major_stat_area_code) %>%
    summarise(rrf_kg = sum(ifelse(species_code == rrf, best_catch, 0)),
      orf_kg = sum(ifelse(!species_code == 396, best_catch, 0)),
      pop_kg = sum(ifelse(species_code == 396, best_catch, 0)),
      trf_kg = sum(best_catch)) %>%
    ungroup()
}

# ref_catch <- get_ref_catch(catch)

# TO DO: should we weight by depth? locality?

# calculate ratio of RRF to prominent historical group (ORF/TRF/POP) in each
# area by fishery sector

#' Get RRF prop
#' Get proportion of RRF catch (landings + discards) to catch of prominent
#' historical group (POP = Pacific Ocean perch, TRF = total rockfish, ORF =
#' other rockfish than POP) by fishery sector and major area for specified
#' reference years. Gamma in Rowan's reconstruction algorithm.
#'
#' @param dat
#' @param prom Prominent historical group (POP = Pacific Ocean perch, TRF =
#' total rockfish, ORF = other rockfish than POP)
#'
#' @export
#'
#' @examples
get_rrf_prop <- function(dat = ref_catch, prom = 'orf') {
  if(ref == 'orf'){
    dat %>%
      group_by(fishery_sector, major_stat_area_code) %>%
      mutate(gamma = rrf_kg/orf_kg)
  } else if(ref == 'pop'){
    dat %>%
      group_by(fishery_sector, major_stat_area_code) %>%
      mutate(gamma = rrf_kg/pop_kg)
  } else if(ref == 'trf'){
    dat %>%
      group_by(fishery_sector, major_stat_area_code) %>%
      mutate(gamma = rrf_kg/trf_kg)
  }
}

# rrf_prop <- get_rrf_ratio(ref = 'orf')
# #----------------------------------------------------------------------------- GOOD TO HERE

# TO DO: for historical catch data without unknown major area, calculate ratio of RRF
# in each major area:all areas from reference years and apply to total catch
# in historical data.


# TO DO: check Rowan's .rda file orfhistory.rda (might need to build orf from
# the big, nasty spreadsheets... or at least code the steps done to from some
# original documented historical catch file(s))
#
# # calculate gamma ratios for each fishery and major area
# for(i in fishery){
#   for(j in major){
#   x <- modern_catch_by_area_and_fishery %>%
#     filter(fishery_sector == i & major_stat_area_code == j)
#   y <- orf_catch_by_area_and_fishery %>%
#     filter(fishery_sector == i & major_stat_area_code == j)
#   assign(paste0("gamma_", i, "_area_", j), x$landed_kg/y$landed_kg)
# }}
#
#
#
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
