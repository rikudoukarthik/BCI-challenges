library(tidyverse)

load("ebd_IN_relNov-2021_NOV.RData")

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



###### monthly challenge (November) winners/results ###

# basic eligible list filter
data1 <- data0 %>% filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% filter(!any(OBSERVATION.COUNT == "X")) %>% ungroup()

# at least 1 list in a day 
data2 <- data1 %>% distinct(OBSERVER.ID, DAY.M, SAMPLING.EVENT.IDENTIFIER) %>% 
  group_by(OBSERVER.ID, DAY.M) %>% summarise(COUNT = n()) %>% 
  ungroup() %>% group_by(OBSERVER.ID) %>% 
  complete(DAY.M = (1:30), fill = list(COUNT = 0)) %>%  
  filter(!any(COUNT < 1)) %>% ungroup()

# dividing into 3 periods and minimum daily number of lists 
data3a <- data2 %>% filter(DAY.M %in% 1:4) %>% 
  group_by(OBSERVER.ID) %>% filter(!any(COUNT < 1)) %>% 
  ungroup()
  
data3b <- data2 %>% filter(DAY.M %in% 5:12) %>% 
  group_by(OBSERVER.ID) %>% filter(!any(COUNT < 2)) %>% 
  ungroup()

data3c <- data2 %>% filter(DAY.M %in% 13:30) %>% 
  group_by(OBSERVER.ID) %>% filter(!any(COUNT < 1)) %>% 
  ungroup()


data4 <- data3a %>% filter(OBSERVER.ID %in% data3b$OBSERVER.ID) %>% distinct(OBSERVER.ID)

data5 <- data4 %>% filter(OBSERVER.ID %in% data3c$OBSERVER.ID)


eBird.users <- read.delim("ebd_users_relJun-2021.txt", sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA))
names(eBird.users) <- c("OBSERVER.ID","FIRST.NAME","LAST.NAME")
eBird.users <- eBird.users %>% transmute(OBSERVER.ID = OBSERVER.ID,
                                         FULL.NAME = paste(FIRST.NAME, LAST.NAME))


data6 <- left_join(data5, eBird.users)

write.csv(data6, "MC_results_2021_11.csv", row.names = F)


# random selection 
a <- read.csv("MC_results_2021_11.csv")
a <- a %>% filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(10)
filter(a, OBSERVER.ID==sample(a$OBSERVER.ID, 1))

# winner Chitra Shanker