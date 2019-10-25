# This is to get the historical rockfish catch data (us & can)
# Author: Elise Keppel
# Created: September 2019

get_orf_history <- function(){
  # import raw US catch data from Catch-Historical.xls spreadsheet from Rowan Haigh
  ketchen76 <- read_xls("inst/extdata/Catch-Historical.xls", sheet = "Ketchen76", range = "A4:G614")

  stewart <- read_xls("inst/extdata/Catch-Historical.xls",
    sheet = "Stewart",
    range = "I4:O36")

  yamanaka <- read_xls("inst/extdata/Catch-Historical.xls",
    sheet = "Yamanaka",
    range = "BJ5:BR38")

  obradovich <- read_xls("inst/extdata/Catch-Historical.xls",
    sheet = "Obradovich",
    range = "A5:K81")

  ketchen80 <- read_xls("inst/extdata/Catch-Historical.xls",
    sheet = "Ketchen80",
    range = "A3:G113")

  leaman80 <- read_xls("inst/extdata/Catch-Historical.xls",
    sheet = "Leaman80",
    range = "A3:G67")
#-----------------------------------------------------------------------------------
  # bring in raw orf landings
  names(yamanaka) <- c('year', 1, 3, 4, 5, 6, 7, 8, 9)
  orf_yamanaka <- yamanaka %>%
    gather('1', '3', '4', '5', '6', '7', '8', '9', key = major, value = catch) %>%
    mutate(
      landings = as.numeric(catch),
      spp = '391',
      nation = 'CA',
      units = "kg",
      source = "yamanaka",
      fishery = "combined",
      action = "add",
      region = case_when(
        major == 1 ~ '4B',
        major == 2 ~ '4A',
        major == 3 ~ '3C',
        major == 4 ~ '3D',
        major == 5 ~ '5A',
        major == 6 ~ '5B',
        major == 7 ~ '5C',
        major == 8 ~ '5D',
        major == 9 ~ '5E')
      ) %>%
    select(-catch)

  orf_ketchen76 <- ketchen76 %>%
    mutate(landings = catch/2.20462,
      units = "kg",
      source = "ketchen76",
      fishery = "trawl",
      action = "max") %>%
    select(-catch)

  orf_obradovich <- obradovich %>%
    gather('1', '2', '3', '4', '5', '6', '7', '8', '9', key = major, value = catch) %>%
    mutate(
      landings = as.numeric(catch)*1000,
      spp = '391',
      nation = 'CA',
      units = "kg",
      source = "obradovich",
      fishery = "trawl",
      action = "max",
      region = case_when(
        major == 1 ~ '4B',
        major == 2 ~ '4A',
        major == 3 ~ '3C',
        major == 4 ~ '3D',
        major == 5 ~ '5A',
        major == 6 ~ '5B',
        major == 7 ~ '5C',
        major == 8 ~ '5D',
        major == 9 ~ '5E')) %>%
    select(-c(catch, gear))

  orf_ketchen80 <- ketchen80 %>%
    mutate(landings = catch*1000,
      units = "kg",
      source = "ketchen80",
      fishery = "trawl",
      action = "add") %>%
    select(-catch)

orf_leaman80 <- leaman80 %>%
  mutate(landings = catch*1000,
    units = "kg",
    source = "leaman80",
    fishery = "trawl",
    action = "add") %>%
  select(-catch)

#----------------------------------------------------------------------------------
 # estimate US ORF landings by major areas 3CD5ABCD for 1930-1964 using ratios calculated from ketchen76 1950-1953 area landings data and US total landings data

 # calculate total US ORF landings by areas 3CD5ABCD (5E not recorded) 1950-1953
  bc_us_landings <- ketchen76 %>% filter(nation == 'US', major %in% c(3,4,5,6,7,8,9)) %>%
    select(-nation, -region) %>%
    # filter for species 391 = ORF
    filter(year %in% c(1950:1953), spp %in% c(391)) %>%
    # convert from lbs to metric tons
    group_by(major) %>%
    summarise(catch = sum(catch)/2.20462) %>% # convert from lbs to kg
    mutate(units = "kg")
  sum_bc_us_landings <- as.numeric(bc_us_landings %>%
      summarise(catch = sum(catch)))

  # calculate total WA ORF landings from 1950-1953

  all_wa_landings <- stewart %>% select(1,6) %>%
    rename(year = "Year", wa_orf_lbs = "WA Rockfishes (lbs)") %>%
    mutate(wa_orf_kg = wa_orf_lbs/2.20462) # convert from lbs to kg
  sum_all_wa_landings <- all_wa_landings %>%
    filter(year %in% c(1950:1953)) %>%
    summarise(catch = sum(wa_orf_kg)) %>%
    as.numeric()

  # calculate ratio of US ORF landings that came from Canadian waters 1950-1953
  bc_us <- as.numeric(sum_bc_us_landings/sum_all_wa_landings) # 0.7148079

  # apply bc/wa ratio to annual total wa landings 1930-1964
  hist_bc_us_landings <- all_wa_landings %>%
    mutate(bc_wa_orf_kg = wa_orf_kg*bc_us)

  # calculate ratio of US ORF landings in each Canadian fishing area vs. total US catch in BC 1950-1953
  area_ratios <- bc_us_landings %>%
    mutate(bc_wa_area = catch/sum_bc_us_landings)

  # apply area ratios to total us catch in BC to obtain us catch by bc fishing area for 1930-1949

  bc_us_landings_by_area <- hist_bc_us_landings %>% select(-wa_orf_lbs, -wa_orf_kg) %>%
      mutate('3' = bc_wa_orf_kg*area_ratios$bc_wa_area[which(area_ratios$major == 3)]) %>%
      mutate('4' = bc_wa_orf_kg*area_ratios$bc_wa_area[which(area_ratios$major == 4)]) %>%
      mutate('5' = bc_wa_orf_kg*area_ratios$bc_wa_area[which(area_ratios$major == 5)]) %>%
      mutate('6' = bc_wa_orf_kg*area_ratios$bc_wa_area[which(area_ratios$major == 6)]) %>%
      mutate('7' = bc_wa_orf_kg*area_ratios$bc_wa_area[which(area_ratios$major == 7)]) %>%
      mutate('8' = bc_wa_orf_kg*area_ratios$bc_wa_area[which(area_ratios$major == 8)]) %>%
      select(-bc_wa_orf_kg) %>%
      gather('3', '4', '5', '6', '7', '8', key = major, value = landings) %>%
      mutate(spp = '391', nation = 'US', units = "kg", source = "stewart", fishery = "trawl", action = "max",
        region = case_when(
          major == 1 ~ '4B',
          major == 2 ~ '4A',
          major == 3 ~ '3C',
          major == 4 ~ '3D',
          major == 5 ~ '5A',
          major == 6 ~ '5B',
          major == 7 ~ '5C',
          major == 8 ~ '5D',
          major == 9 ~ '5E'))

  orf <- rbind(orf_yamanaka, orf_ketchen76, orf_obradovich, orf_ketchen80, orf_leaman80, bc_us_landings_by_area)

  }


get_pop_history <-

get_orf_1950_1975 <- function(){
  ketchen76 <- read_xls("inst/extdata/Catch-Historical.xls", sheet = "Ketchen76", range = "A4:G614")
  ketchen76 %>% mutate(catch = catch/2.20462, units = "kg")
  }
