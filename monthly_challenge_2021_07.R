require(lubridate)
require(tidyverse)

preimp = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY","LAST.EDITED.DATE",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","HAS.MEDIA","BREEDING.CODE",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

rawpath = "ebd_IN_202106_202106_relJun-2021.txt"

nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

data = data %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         daym = day(OBSERVATION.DATE))

data0 = data
datas = data %>% filter(CATEGORY %in% c("species","issf"))

totobs = length(data0$COMMON.NAME)
totlists = length(unique(data0$SAMPLING.EVENT.IDENTIFIER))
totbir = length(unique(data0$OBSERVER.ID))
totspecs = length(unique(datas$COMMON.NAME))
media = data0 %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% filter(any(HAS.MEDIA == 1)) %>%
  ungroup
media = length(unique(media$SAMPLING.EVENT.IDENTIFIER))


nol = data0 %>% filter(ALL.SPECIES.REPORTED == 1)
n = length(unique(nol$SAMPLING.EVENT.IDENTIFIER))


############# to check for the basic no Xs, at least 30 lists from a single location

data1 = data0 %>% filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14.5) 

data2 = data1 %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% filter(!any(OBSERVATION.COUNT == "X")) %>%
  ungroup

data3 = data2 %>%
  distinct(OBSERVER.ID,LOCALITY,SAMPLING.EVENT.IDENTIFIER)

data4 = data3 %>%
  group_by(OBSERVER.ID,LOCALITY) %>% summarize(count = n())

data5 = data4 %>% filter(count >= 30)

####### linking unique ID with names of eBird users ### 

data5$obs.id.num <- gsub("[[:alpha:]]", "", data5$OBSERVER.ID)

eBird.users = read.delim("ebd_users_relFeb-2021.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                         na.strings = c(""," ",NA))
names(eBird.users) = c("USER_ID","FIRST_NAME","LAST_NAME")
eBird.users = eBird.users %>% mutate(FULL.NAME = paste(FIRST_NAME, LAST_NAME, sep = " "))
eBird.users$obs.id.num = gsub("[[:alpha:]]", "", eBird.users$USER_ID)

data5 = left_join(data5, eBird.users)

write.csv(data5, "june-2021-challenge-results.csv", row.names = F)


##########

a = read.csv("june-2021-challenge-results.csv")
a = a %>% filter(FULL.NAME != "MetalClicks Ajay Ashok")
set.seed(n)
sample(a$FULL.NAME, 1)

#winner Claudia Pinheiro from Avadi, Tamil Nadu