library(tidyverse)

load("ebd_IN_relOct-2021_OCT.RData")

data0 <- data 
datas <- data %>% filter(CATEGORY %in% c("species","issf"))



###### monthly stats ###

totbdr <- length(unique(data0$OBSERVER.ID))
totobs <- length(data0$COMMON.NAME)
totlists <- length(unique(data0$SAMPLING.EVENT.IDENTIFIER))
totspecs <- length(unique(datas$COMMON.NAME))
# complete lists
clists <- data0 %>% filter(ALL.SPECIES.REPORTED == 1)
totclists <- length(unique(clists$SAMPLING.EVENT.IDENTIFIER))
# lists with media
media <- data0 %>% group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(any(HAS.MEDIA == 1)) %>% ungroup()
totmedia <- length(unique(media$SAMPLING.EVENT.IDENTIFIER))



###### monthly challenge (October) winners/results ###

# basic eligible list filter
data1 <- data0 %>% filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% filter(!any(OBSERVATION.COUNT == "X")) %>% ungroup()

# at least 40 total lists in month 
data2 <- data1 %>% distinct(OBSERVER.ID,SAMPLING.EVENT.IDENTIFIER) %>% 
  group_by(OBSERVER.ID) %>% summarise(COUNT = n()) %>% 
  filter(COUNT >= 40)

# dividing into 3 periods
data3a <- data1 %>% filter(DAYM %in% 1:7) %>% 
  distinct(OBSERVER.ID, DAYM, SAMPLING.EVENT.IDENTIFIER)
data3b <- data1 %>% filter(DAYM %in% 8:10) %>% 
  distinct(OBSERVER.ID, DAYM, SAMPLING.EVENT.IDENTIFIER)
data3c <- data1 %>% filter(DAYM %in% 11:31) %>% 
  distinct(OBSERVER.ID, DAYM, SAMPLING.EVENT.IDENTIFIER)

# minimum daily number of lists for the 3 periods
data4a <- data3a %>% group_by(OBSERVER.ID, DAYM) %>% summarise(COUNT = n()) %>% 
  ungroup() %>% group_by(OBSERVER.ID) %>% 
  complete(DAYM = (1:7), fill = list(COUNT = 0)) %>%  
  filter(!any(COUNT == 0)) %>% ungroup()

data4b <- data3b %>% group_by(OBSERVER.ID, DAYM) %>% summarise(COUNT = n()) %>% 
  ungroup() %>% group_by(OBSERVER.ID) %>% 
  complete(DAYM = (8:10), fill = list(COUNT = 0)) %>%  
  filter(!any(COUNT < 4)) %>% ungroup()

data4c <- data3c %>% group_by(OBSERVER.ID, DAYM) %>% summarise(COUNT = n()) %>% 
  ungroup() %>% group_by(OBSERVER.ID) %>% 
  complete(DAYM = (11:31), fill = list(COUNT = 0)) %>%  
  filter(!any(COUNT == 0)) %>% ungroup()

# minimum number of total lists for the 3 periods
data5a <- data4a %>% group_by(OBSERVER.ID) %>% summarise(COUNT = sum(COUNT)) %>% 
  filter(COUNT >= 7)
data5b <- data4b %>% group_by(OBSERVER.ID) %>% summarise(COUNT = sum(COUNT)) %>%
  filter(COUNT >= 12)
data5c <- data4c %>% group_by(OBSERVER.ID) %>% summarise(COUNT = sum(COUNT)) %>%
  filter(COUNT >= 21)

data6a <- data5a %>% filter(OBSERVER.ID %in% data5b$OBSERVER.ID) %>% select(OBSERVER.ID)
data6b <- data6a %>% filter(OBSERVER.ID %in% data5c$OBSERVER.ID)


eBird.users <- read.delim("ebd_users_relJun-2021.txt", sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA))
names(eBird.users) <- c("OBSERVER.ID","FIRST.NAME","LAST.NAME")
eBird.users <- eBird.users %>% transmute(OBSERVER.ID = OBSERVER.ID,
                                         FULL.NAME = paste(FIRST.NAME, LAST.NAME))


data7 <- left_join(data6b, eBird.users)

write.csv(data7, "MC_results_2021_10.csv", row.names = F)


# random selection 
a <- read.csv("MC_results_2021_10.csv")
a <- a %>% filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(10)
filter(a, OBSERVER.ID==sample(a$OBSERVER.ID, 1))

# winner shyamkumar puravankara