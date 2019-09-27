# This is to get the historical rockfish catch data (us & can)

get_US_ORF_catch <- function(){
  # import raw US catch data from Catch-Historical.xls spreadsheet from Rowan Haigh
  ketchen <- read.csv("inst/extdata/Ketchen1976.csv")

  # calculate total US ORF landings from areas 3CD5ABCD (5E not recorded) 1950-1953
  bc_us_landings <- ketchen %>% filter(nation == 'US', major %in% c(3,4,5,6,7,8,9)) %>%
    select(-nation, -region) %>%
    # filter for species 391 = ORF
    filter(year %in% c(1950:1953), spp == '391') %>%
    # convert from lbs to metric tons
    group_by(major) %>%
    summarise(catch = sum(catch)/2204.62)
  sum_bc_us_landings <- as.numeric(bc_us_landings %>%
      summarise(catch = sum(catch)))

  # calculate total WA ORF landings from 1950-1953
  stewart <- read.csv("inst/extdata/Stewart.csv")
  all_wa_landings <- stewart %>% select(1,6,7) %>%
    rename(wa_total_catch_lbs = WA.Rockfishes..lbs.) %>%
    rename(wa_total_catch_mt = mt.2)
  sum_all_wa_landings <- all_wa_landings %>%
    filter(Year %in% c(1950:1953)) %>%
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

  # apply area ratios to total us catch in BC to obtain us catch by bc fishing area for 1930-1964
  #??? why wouldn't we use the real us catch by area that we have for 1950-1964 from Ketchen??
  bc_us_landings_by_area_lbs <- hist_bc_us_landings %>% select(-wa_total_catch_mt, -bc_wa_catch_mt) %>%
    mutate('3C' = bc_wa_catch_lbs*area_ratios$bc_wa_area[which(area_ratios$major == 3)]) %>%
    mutate('3D' = bc_wa_catch_lbs*area_ratios$bc_wa_area[which(area_ratios$major == 4)]) %>%
    mutate('5A' = bc_wa_catch_lbs*area_ratios$bc_wa_area[which(area_ratios$major == 5)]) %>%
    mutate('5B' = bc_wa_catch_lbs*area_ratios$bc_wa_area[which(area_ratios$major == 6)]) %>%
    mutate('5C' = bc_wa_catch_lbs*area_ratios$bc_wa_area[which(area_ratios$major == 7)]) %>%
    mutate('5D' = bc_wa_catch_lbs*area_ratios$bc_wa_area[which(area_ratios$major == 8)])

  bc_us_landings_by_area_mt <- hist_bc_us_landings %>% select(-wa_total_catch_lbs, -bc_wa_catch_lbs) %>%
    mutate('3C' = bc_wa_catch_mt*area_ratios$bc_wa_area[which(area_ratios$major == 3)]) %>%
    mutate('3D' = bc_wa_catch_mt*area_ratios$bc_wa_area[which(area_ratios$major == 4)]) %>%
    mutate('5A' = bc_wa_catch_mt*area_ratios$bc_wa_area[which(area_ratios$major == 5)]) %>%
    mutate('5B' = bc_wa_catch_mt*area_ratios$bc_wa_area[which(area_ratios$major == 6)]) %>%
    mutate('5C' = bc_wa_catch_mt*area_ratios$bc_wa_area[which(area_ratios$major == 7)]) %>%
    mutate('5D' = bc_wa_catch_mt*area_ratios$bc_wa_area[which(area_ratios$major == 8)])

  bc_us_landings_by_area_lbs <- bc_us_landings_by_area_lbs %>%
    gather('3C', '3D', '5A', '5B', '5C', '5D', key = area, value = catch_lbs, -wa_total_catch_lbs, -bc_wa_catch_lbs)

  bc_us_landings_by_area_mt <- bc_us_landings_by_area_mt %>%
    gather('3C', '3D', '5A', '5B', '5C', '5D', key = area, value = catch_mt, -wa_total_catch_mt, -bc_wa_catch_mt)

}



US_ORF_catch <- get_US_ORF_catch()
