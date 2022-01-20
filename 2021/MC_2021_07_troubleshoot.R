require(lubridate)
require(tidyverse)

preimp <- c("CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
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

eBird.users <- read.delim("ebd_users_relJun-2021.txt", sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA))
names(eBird.users) <- c("OBSERVER.ID","FIRST.NAME","LAST.NAME")
eBird.users <- eBird.users %>% transmute(OBSERVER.ID = OBSERVER.ID,
                                         FULL.NAME = paste(FIRST.NAME, LAST.NAME, sep = " "))



###### Debashis Chowdhury ####

dataDC <- left_join(data, eBird.users) %>% filter(FULL.NAME == "Debashis Chowdhury") %>% 
          arrange(SAMPLING.EVENT.IDENTIFIER)

write.csv(dataDC, "Troubleshooting/monthly-challenge-results-2021-07_DebCho.csv", row.names = F)

## no checklist comments in data download (probably added later)



###### Kalpana Jayaraman ####

dataKJ <- left_join(data, eBird.users) %>% filter(FULL.NAME == "kalpana jayaraman") %>% 
          arrange(SAMPLING.EVENT.IDENTIFIER)

# basic eligible list filter, at least 31 lists
data1 <- dataKJ %>% filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
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
  summarise(SCOUNT = n_distinct(SAMPLING.EVENT.IDENTIFIER)) 

data9 <-  data8 %>% filter(OBSERVER.ID %in% data7$OBSERVER.ID)
data10 <- left_join(data9,data7)

write.csv(dataKJ, "Troubleshooting/monthly-challenge-results-2021-07_KalJay.csv", row.names = F)
write.csv(data10, "Troubleshooting/monthly-challenge-results-2021-07_KalJay_summary.csv", row.names = F)

## only one audio in July



###### Ashok Agarwal ####

dataAA <- left_join(data, eBird.users) %>% filter(FULL.NAME == "Ashok Agarwal") %>% 
          arrange(SAMPLING.EVENT.IDENTIFIER)

write.csv(dataAA, "Troubleshooting/monthly-challenge-results-2021-07_AshAga.csv", row.names = F)

## no checklist comments in data download

###### Rahul Singh ####

dataRS <- left_join(data, eBird.users) %>% filter(FULL.NAME == "Rahul Singh") %>% 
  arrange(SAMPLING.EVENT.IDENTIFIER)

eBird.users %>% filter(FULL.NAME == "Rahul Singh")
data %>% filter(OBSERVER.ID %in% c("obsr1972797","obsr2495022"))
data %>% filter(OBSERVER.ID == "obsr1972797" | OBSERVER.ID == "obsr2495022")


###### Checklist comments issue ####

load("2021/ebd_IN_relAug-2021_AUG.RData")

# data <- data %>% filter(LOCALITY == "Kousapara, Kanjikode, Palakkad")
# my observer.ID is obsr841592

data <- data %>% filter(OBSERVER.ID == "obsr841592")

# I selected the hide option after 21st and before 31st (lists from both dates have comments). 
# I unchecked it this morning, but the Aug release had already been made available yesterday.
# So, it seems that the hidden comments do not appear in the raw data download, even for 
# oneself. And it only depends on whether or not the option in preferences is checked 
# (or not) at the time of preparation of the dataset (even older comments might be hidden).

