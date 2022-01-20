library(tidyverse)

load("2021/ebd_IN_relSep-2021_SEP.RData")

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



###### monthly challenge (September) winners/results ###

data1 <- data0 %>% filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% filter(!any(OBSERVATION.COUNT == "X")) %>%
  ungroup()

data2 <- data1 %>% filter(DAYM %in% 1:15) %>% distinct(OBSERVER.ID,SAMPLING.EVENT.IDENTIFIER)
data3 <- data1 %>% filter(DAYM %in% 16:30) %>% distinct(OBSERVER.ID,SAMPLING.EVENT.IDENTIFIER)

data4 <- data2 %>% group_by(OBSERVER.ID) %>% summarise(COUNT = n()) %>% filter(COUNT >= 20)
data5 <- data3 %>% group_by(OBSERVER.ID) %>% summarise(COUNT = n()) %>% filter(COUNT >= 20)

data6 <- data4 %>% filter(OBSERVER.ID %in% data5$OBSERVER.ID) %>% select(OBSERVER.ID)


eBird.users <- read.delim("ebd_users_relJun-2021.txt", sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA))
names(eBird.users) <- c("OBSERVER.ID","FIRST.NAME","LAST.NAME")
eBird.users <- eBird.users %>% transmute(OBSERVER.ID = OBSERVER.ID,
                                         FULL.NAME = paste(FIRST.NAME, LAST.NAME))

data7 <- left_join(data6, eBird.users)

write.csv(data7, "2021/MC_results_2021_09.csv", row.names = F)


# random selection 
a <- read.csv("2021/MC_results_2021_09.csv")
a <- a %>% filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(10)
filter(a, OBSERVER.ID==sample(a$OBSERVER.ID, 1))

# winner Shashikiran Ganesh