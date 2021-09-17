require(tidyverse)

load("ebd_IN_relAug-2021_AUG.RData")

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
  filter(any(HAS.MEDIA == 1)) %>% ungroup
totmedia <- length(unique(media$SAMPLING.EVENT.IDENTIFIER))



###### monthly challenge (August) winners/results ###

# basic eligible list filter, at least 31 lists
data1 <- data0 %>% filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% filter(!any(OBSERVATION.COUNT == "X")) %>%
  ungroup()

data2 <- data1 %>% distinct(OBSERVER.ID,SAMPLING.EVENT.IDENTIFIER)

data3 <- data2 %>% group_by(OBSERVER.ID) %>% summarise(COUNT = n()) %>% filter(COUNT >= 31)

# with breeding code
data4 <- data1 %>% filter(BREEDING.CODE != "F", BREEDING.CODE != "H") %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% filter(any(!is.na(BREEDING.CODE))) %>% ungroup()

data5 <- data4 %>% distinct(OBSERVER.ID,SAMPLING.EVENT.IDENTIFIER)

data6 <- data5 %>% filter(OBSERVER.ID %in% data3$OBSERVER.ID)
data7 <- data6 %>% group_by(OBSERVER.ID) %>% summarise(COUNT = n()) %>% filter(COUNT >= 5)




####### linking unique ID with names of eBird users ### 

eBird.users <- read.delim("ebd_users_relJun-2021.txt", sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA))
names(eBird.users) <- c("OBSERVER.ID","FIRST.NAME","LAST.NAME")
eBird.users <- eBird.users %>% transmute(OBSERVER.ID = OBSERVER.ID,
                                         FULL.NAME = paste(FIRST.NAME, LAST.NAME))

data8 <- left_join(data7, eBird.users)

write.csv(data8, "MC_results_2021_08.csv", row.names = F)


####### random selection ###

a <- read.csv("MC_results_2021_08.csv")
a <- a %>% filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(10)
filter(a, OBSERVER.ID==sample(a$OBSERVER.ID, 1))

# winner ANURANJAN  DHURWEY from ___
