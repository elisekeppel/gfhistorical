# # Script for comparing between Rowan's orfhistory and my recreation
# # My values appear to be slightly lower for Ketchen76, Yamanaka and Stewart
# #   following conversion from lbs to kg using /2.20462
# sum_orfhistory <- orfhistory %>%
#   filter(source %in% c("Leaman80",
#     "Ketchen76", "Ketchen80", "Obradovich", "Yamanaka", "Stewart")) %>%
#   # mutate(landings = catch/2.20462) %>%
#   group_by(source) %>%
#   summarise(n())
#
# sum_orf <- orf_history %>%
#   filter(source %in% c("leaman80",
#     "ketchen76", "ketchen80", "obradovich", "yamanaka", "stewart")) %>%
#   # mutate(landings = catch/2.20462) %>%
#   group_by(source) %>%
#   summarise(n())
#
# #----------------------------------------------------------------------
# sum_orfhistory <- orfhistory %>%
#   # filter(source == "Ketchen76") %>%
#   # mutate(landings = catch/2.20462) %>%
#   # group_by(year, spp, major, nation, source, action, fishery) %>%
#   group_by(year) %>%
#   summarise(first_year = min(year), last_year = max(year), sum(catch), n())
#
# sum_orf <- orf_history %>%
#   # filter(source == "ketchen76") %>%
#   # mutate(landings = catch/2.20462) %>%
#   # group_by(year, spp, major, nation, source, action, fishery) %>%
#   group_by(year) %>%
#   summarise(first_year = min(year), last_year = max(year), sum(landings, na.rm = TRUE), n())
#
# sum_orfhistory <- orfhistory %>%
#   filter(source == "Yamanaka") %>%
#   # mutate(landings = catch/2.20462) %>%
#   group_by(major) %>%
#   summarise(first_year = min(year), last_year = max(year), sum(catch), n())
#
# sum_orf <- orf %>%
#   filter(source == "yamanaka") %>%
#   # mutate(landings = catch/2.20462) %>%
#   group_by(major) %>%
#   summarise(first_year = min(year), last_year = max(year), sum(landings), n())
