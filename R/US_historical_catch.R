# This is to get the historical rockfish catch data (us & can)
# Author: Elise Keppel
# Created: September 2019

# estimate 1930-1949 US landings in areas 3C-5D using total WA landings
# (Stewart) and ratio of total WA landings (Stewart) : area landings (Ketchen76)
# from 1950-1953


get_US_ORF_catch <- function(unit = "kg"){
  # import raw US catch data from Catch-Historical.xls spreadsheet from Rowan Haigh
  ketchen76 <- read_xls("inst/extdata/Catch-Historical.xls", sheet = "Ketchen76", range = "A4:G614")

  # calculate total US ORF landings from areas 3CD5ABCD (5E not recorded) 1950-1953
  bc_us_landings <- ketchen76 %>% filter(nation == 'US', major %in% c(3,4,5,6,7,8,9)) %>%
    select(-nation, -region) %>%
    # filter for species 391 = ORF
    filter(year %in% c(1950:1953), spp %in% c(391, 396)) %>%
    # convert from lbs to metric tons
    group_by(major) %>%
    summarise(catch = sum(catch)/2204.62)
  sum_bc_us_landings <- as.numeric(bc_us_landings %>%
      summarise(catch = sum(catch)))

  # calculate total WA ORF landings from 1950-1953
  all_wa_landings <- read_xls("inst/extdata/Catch-Historical.xls", sheet = "Stewart", range = "I4:O36") %>% select(1,6,7) %>%
    rename(year = "Year",
      wa_total_catch_lbs = "WA Rockfishes (lbs)",
      wa_total_catch_mt = "mt...7")
  sum_all_wa_landings <- all_wa_landings %>%
    filter(year %in% c(1950:1953)) %>%
    summarise(catch = sum(wa_total_catch_mt)) %>%
    as.numeric()

  # calculate ratio of US ORF landings that came from Canadian waters 1950-1953
  bc_wa <- as.numeric(sum_bc_us_landings/sum_all_wa_landings) # 0.7148079

  # apply bc/wa ratio to annual total wa landings 1930-1964
  hist_bc_us_landings <- all_wa_landings %>%
    mutate(bc_wa_catch_lbs = wa_total_catch_lbs*bc_wa) %>%
    mutate(bc_wa_catch_mt = wa_total_catch_mt*bc_wa)

  # calculate ratio of US ORF landings in each Canadian fishing area vs. total US catch in BC 1950-1953
  area_ratios <- bc_us_landings %>%
    mutate(bc_wa_area = catch/sum_bc_us_landings)

  # apply area ratios to total us catch in BC to obtain us catch by bc fishing area for 1930-1949

  if(unit == "kg"){
    bc_us_landings_by_area_mt <- hist_bc_us_landings %>% select(-wa_total_catch_lbs, -bc_wa_catch_lbs) %>%
      filter(year %in% 1930:1949) %>%
      mutate('3C' = bc_wa_catch_mt*1000*area_ratios$bc_wa_area[which(area_ratios$major == 3)]) %>%
      mutate('3D' = bc_wa_catch_mt*1000*area_ratios$bc_wa_area[which(area_ratios$major == 4)]) %>%
      mutate('5A' = bc_wa_catch_mt*1000*area_ratios$bc_wa_area[which(area_ratios$major == 5)]) %>%
      mutate('5B' = bc_wa_catch_mt*1000*area_ratios$bc_wa_area[which(area_ratios$major == 6)]) %>%
      mutate('5C' = bc_wa_catch_mt*1000*area_ratios$bc_wa_area[which(area_ratios$major == 7)]) %>%
      mutate('5D' = bc_wa_catch_mt*1000*area_ratios$bc_wa_area[which(area_ratios$major == 8)]) %>%
      select(-c(wa_total_catch_mt, bc_wa_catch_mt)) %>%
      gather('3C', '3D', '5A', '5B', '5C', '5D', key = area, value = catch_mt)
  } else {
    bc_us_landings_by_area_lbs <- hist_bc_us_landings %>% select(-wa_total_catch_mt, -bc_wa_catch_mt) %>%
      filter(year %in% 1930:1949) %>%
      mutate('3C' = bc_wa_catch_lbs*area_ratios$bc_wa_area[which(area_ratios$major == 3)]) %>%
      mutate('3D' = bc_wa_catch_lbs*area_ratios$bc_wa_area[which(area_ratios$major == 4)]) %>%
      mutate('5A' = bc_wa_catch_lbs*area_ratios$bc_wa_area[which(area_ratios$major == 5)]) %>%
      mutate('5B' = bc_wa_catch_lbs*area_ratios$bc_wa_area[which(area_ratios$major == 6)]) %>%
      mutate('5C' = bc_wa_catch_lbs*area_ratios$bc_wa_area[which(area_ratios$major == 7)]) %>%
      mutate('5D' = bc_wa_catch_lbs*area_ratios$bc_wa_area[which(area_ratios$major == 8)]) %>%
      select(-c(wa_total_catch_lbs, bc_wa_catch_lbs)) %>%
      gather('3C', '3D', '5A', '5B', '5C', '5D', key = area, value = catch_lbs)
  }
}


get_US_POP_catch <- function() {
  ketchen76 <- read_xls("inst/extdata/Catch-Historical.xls", sheet = "Ketchen76", range = "A4:G614")

  # calculate total US ORF landings from areas 3CD5ABCD (5E not recorded) 1950-1953
  bc_us_landings <- ketchen76 %>% filter(nation == 'US', major %in% c(3,4,5,6,7,8,9)) %>%
    select(-nation, -region) %>%
    # filter for species 391 = ORF
    filter(year %in% c(1950:1953), spp %in% c(396)) %>%
    # convert from lbs to metric tons
    group_by(major) %>%
    summarise(catch = sum(catch)/2204.62)
  sum_bc_us_landings <- as.numeric(bc_us_landings %>%
      summarise(catch = sum(catch)))
}
