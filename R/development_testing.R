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

test <- orf_history %>% filter(source == "GFCatch") %>%
  summarise(catch = sum(catch))
test2 <- orfhistory %>% filter(source == "GFCatch") %>%
  summarise(catch = sum(catch))



unique(orf_history$major)


# compare catch from orfhistory and orf_history
d <-orfhistory %>%
  #filter(source %in% c("Yamanaka", "Stewart", "Ketchen76", "Ketchen80", "Leaman80", "Obradovich")) %>%
  group_by(source) %>% summarise(sum(catch))
d2 <- orf_history %>% # 2.20456
  #filter(source %in% c("yamanaka2", "stewart", "ketchen76", "ketchen80", "leaman80", "obradovich")) %>%
      group_by(source) %>% summarise(sum(catch))
d3 <- cbind(d, d2)

# compare RH orfhistory records (43527) with EK orf_history records (41826)
x3 <- orfhistory %>%
  filter(source %in% c("Yamanaka", "Stewart")) %>%
  group_by(major, source) %>% summarise(sum(catch))
y3 <- orf_history %>%
  filter(source %in% c("yamanaka", "stewart")) %>%
  group_by(major, source) %>% summarise(sum(catch))
z3 <- cbind(x3,y3)
names(z3) <- c('major', 'source', 'RH_catch', 'x', 'y', 'EK_catch')
z3 <- z3 %>% select(-c(x,y)) %>% group_by(major) %>% mutate(RH_catch - EK_catch)

x1 <- orfhistory %>%
  filter(source %in% c("Yamanaka", "Stewart")) %>%
  group_by(year, source) %>% summarise(sum(catch))
y1 <- orf_history %>%
  filter(source %in% c("yamanaka", "stewart")) %>%
  group_by(year, source) %>% summarise(sum(catch))
z1 <- cbind(x1,y1)
names(z1) <- c('year', 'RH_catch', 'EK', 'EK_catch')
z1 <- z1 %>% select(-EK) %>% group_by(year) %>% mutate(RH_catch - EK_catch)

x2 <- orfhistory %>%
  filter(source %in% c("Yamanaka", "Stewart")) %>%
  group_by(source) %>% summarise(sum(catch))
y2 <- orf_history %>%
  filter(source %in% c("yamanaka", "stewart")) %>%
  group_by(source) %>% summarise(sum(catch))
z2 <- cbind(x2,y2)
names(z2) <- c('source', 'RH_catch', 'EK', 'EK_catch')
z2 <- z2 %>% select(-EK) %>% group_by(source) %>% mutate(RH_catch - EK_catch)



Yamanaka <- orfhistory %>% filter(source == "Yamanaka", major %in% c(3,4))
yamanaka <- orf_history %>% filter(source == "yamanaka", major %in% c(3,4))





