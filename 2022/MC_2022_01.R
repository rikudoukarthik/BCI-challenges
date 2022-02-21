library(tidyverse)
library(magrittr)

load("2022/ebd_IN_relJan-2022_JAN.RData")

data0 <- data 
datas <- data %>% filter(CATEGORY %in% c("species","issf"))



###### monthly stats ###

totbdr <- n_distinct(data0$OBSERVER.ID)
totobs <- length(data0$COMMON.NAME)
totlists <- n_distinct(data0$SAMPLING.EVENT.IDENTIFIER)
totspecs <- n_distinct(datas$COMMON.NAME)
# complete lists
totclists <- data0 %>% filter(ALL.SPECIES.REPORTED == 1) %$% 
  n_distinct(SAMPLING.EVENT.IDENTIFIER)
# unique lists with media
totmedia <- data0 %>% 
  group_by(GROUP.ID) %>% 
  filter(any(HAS.MEDIA == 1)) %>% ungroup() %$%
  n_distinct(GROUP.ID)



###### monthly challenge (January) winners/results ###


# basic eligible list filter
data1 <- data0 %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()


# shared with AWC
data2 <- data1 %>% 
  group_by(GROUP.ID) %>% 
  filter(any(OBSERVER.ID == "obsr1196810")) %>%
  ungroup() %>% 
  filter(!OBSERVER.ID == "obsr1196810")


# checking for only wetland locations
wetlands <- c("pond","Pond","lake","Lake","beach","Beach","river","River"," Dam",
              "reservoir","Reservoir","canal","Canal","jheel","Jheel","kere","Kere",
              "wetland","Wetland","mangrove","Mangrove","creek","Creek","jetty",
              "Jetty","marsh","Marsh")
awc <- c("AWC", "awc", "Asian Waterbird Census", "ensus", "aterbird")

data3 <- data2 %>%
  filter(str_detect(LOCALITY, paste(wetlands, collapse = "|")) | 
           str_detect(TRIP.COMMENTS, paste(wetlands, collapse = "|")) |
           str_detect(TRIP.COMMENTS, paste(awc, collapse = "|")))


# at least 3 wetlands
data4 <- data3 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.WLISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            NO.W = n_distinct(LOCALITY)) %>% 
  filter(NO.W >= 3)



eBird_users <- read.delim("ebd_users_relDec-2021.txt", sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA))
eBird_users <- eBird_users %>% 
  transmute(OBSERVER.ID = observer_id,
            FULL.NAME = paste(first_name, last_name, sep = " "))

# list of group accounts to be filtered
groupaccs <- read_csv("ebd_users_GA_relDec-2021.csv")  
groupaccs <- groupaccs %>% 
  mutate(CATEGORY = case_when(GA.1 == 1 ~ "GA.1", GA.2 == 1 ~ "GA.2", TRUE ~ "NG"))
filtGA <- groupaccs %>% 
  # both categories need to be filtered because this is birder-related 
  filter(CATEGORY == "GA.1" | CATEGORY == "GA.2") %>% 
  select(OBSERVER.ID)



data5 <- data4 %>% 
  left_join(eBird_users) %>% 
  anti_join(filtGA) %>% 
  arrange(desc(NO.W))

# # verifying
# x <- data2 %>% 
#   filter(OBSERVER.ID %in% data5$OBSERVER.ID) %>% 
#   distinct(OBSERVER.ID, LOCALITY, TRIP.COMMENTS) %>% 
#   left_join(data5) %>% 
#   arrange(OBSERVER.ID, FULL.NAME)

write_csv(data5, "2022/MC_results_2022_01.csv", na = "")



# random selection 
a <- read_csv("2022/MC_results_2022_01.csv")
a <- a %>% filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(10)
a %>% filter(OBSERVER.ID == sample(a$OBSERVER.ID, 1))

# winner Karan Matalia