require(lubridate)
require(tidyverse)

preimp <- c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY","LAST.EDITED.DATE",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","HAS.MEDIA","BREEDING.CODE",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER",
           "TRIP.COMMENTS")

rawpath <- "2021/ebd_IN_202107_202107_relJul-2021.txt"

# reading in only one row to then select only required columns
nms <- read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms <- names(nms)
nms[!(nms %in% preimp)] <- "NULL"
nms[nms %in% preimp] <- NA
# using nms object to filter columns read from the raw data
data <- read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

data <- data %>% mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
                        MONTH = month(OBSERVATION.DATE),
                        DAYM = day(OBSERVATION.DATE))

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



###### monthly challenge (July) winners/results ###

# basic eligible list filter, at least 31 lists
data1 <- data0 %>% filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
         group_by(SAMPLING.EVENT.IDENTIFIER) %>% filter(!any(OBSERVATION.COUNT == "X")) %>%
         ungroup

data2 <- data1 %>% distinct(OBSERVER.ID,SAMPLING.EVENT.IDENTIFIER,TRIP.COMMENTS)

data3 <- data2 %>% group_by(OBSERVER.ID) %>% 
         summarise(COUNT = n()) %>% filter(COUNT >= 31)

# with wetland
data4 <- data2 %>% filter(grepl("etland", TRIP.COMMENTS))

data5 <- data4 %>% group_by(OBSERVER.ID) %>% 
         summarise(WCOUNT = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% filter(WCOUNT >= 7)

data6 <- data3 %>% filter(OBSERVER.ID %in% data5$OBSERVER.ID)
data7 <- left_join(data6,data5)

# sound recordings
sound <- read.csv("2021/ML_2021-08-17T00-41_audio_IN.csv", header = T, stringsAsFactors = F)
sound <- sound %>% distinct(Recordist,eBird.Checklist.ID)
names(sound) <- c("FULL.NAME","SAMPLING.EVENT.IDENTIFIER")

data8 <- data2 %>%  
         filter(SAMPLING.EVENT.IDENTIFIER %in% sound$SAMPLING.EVENT.IDENTIFIER) %>% 
         group_by(OBSERVER.ID) %>% 
         summarise(SCOUNT = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% filter(SCOUNT >= 7)

data9 <-  data8 %>% filter(OBSERVER.ID %in% data7$OBSERVER.ID)
data10 <- left_join(data9,data7)



####### linking unique ID with names of eBird users ### 

eBird.users <- read.delim("ebd_users_relJun-2021.txt", sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA))
names(eBird.users) <- c("OBSERVER.ID","FIRST.NAME","LAST.NAME")
eBird.users <- eBird.users %>% transmute(OBSERVER.ID = OBSERVER.ID,
                                         FULL.NAME = paste(FIRST.NAME, LAST.NAME, sep = " "))

data11 <- left_join(data10, eBird.users)

write.csv(data11, "2021/MC_results_2021_07.csv", row.names = F)


####### random selection ###

a <- read.csv("2021/MC_results_2021_07.csv")
a <- a %>% filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(10)
filter(a, OBSERVER.ID==sample(a$OBSERVER.ID, 1))

# winner Anuj Saikia from ___
