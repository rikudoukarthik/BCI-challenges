library(lubridate)
library(tidyverse)
library(magrittr)

# place file in directory
load("ebd_IN_relDec-2021.RData")


# list of group accounts to be filtered
groupaccs <- read.csv("ebd_users_GA_relSep-2021.csv", 
                      na.strings = c(""," ",NA), quote = "", header = T, 
                      nrows = 401)  # excluding empty cells
groupaccs <- groupaccs %>% 
  mutate(CATEGORY = case_when(GA.1 == 1 ~ "GA.1", GA.2 == 1 ~ "GA.2", TRUE ~ "NG"))
filtGA <- groupaccs %>% filter(CATEGORY == "GA.1") %>% select(OBSERVER.ID)


# filtering for 2021, removing group account data
data0 <- data %>% 
  filter(YEAR == 2021) %>% 
  anti_join(filtGA) # removing data from group accounts
rm(data)

datas <- data0 %>% filter(CATEGORY %in% c("species","issf"))


# basic eligible list filter
data1 <- data0 %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()


# user information
eBird.users <- read.delim("ebd_users_relDec-2021.txt", sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA))
names(eBird.users) <- c("OBSERVER.ID","FIRST.NAME","LAST.NAME")
eBird.users <- eBird.users %>% transmute(OBSERVER.ID = OBSERVER.ID,
                                         FULL.NAME = paste(FIRST.NAME, LAST.NAME))


###### 2021 stats ####

totbdr <- n_distinct(data0$OBSERVER.ID)
totobs <- length(data0$COMMON.NAME)
totlists <- n_distinct(data0$SAMPLING.EVENT.IDENTIFIER)
totspecs <- n_distinct(datas$COMMON.NAME)
# complete lists
totclists <- data0 %>% 
  filter(ALL.SPECIES.REPORTED == 1) %$% 
  n_distinct(SAMPLING.EVENT.IDENTIFIER)
# unique lists with media
totmedia <- data0 %>% 
  group_by(GROUP.ID) %>% 
  filter(any(HAS.MEDIA == 1)) %>% ungroup() %$%
  n_distinct(GROUP.ID)



###### prolific eBirder (>=480 eligible lists) ####

data2 <- data1 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 480) %>%
  ungroup()

# prolific eBirders
data3 <- left_join(data2, eBird.users) %>% 
  arrange(desc(NO.LISTS))
write.csv(data3, "2021/YC_2021_results_cat_prolific.csv", row.names = F)

# random selection 
a <- read.csv("2021/YC_2021_results_cat_prolific.csv")
a <- a %>% filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(20211)
prolific <- a %>% filter(OBSERVER.ID == sample(a$OBSERVER.ID, 1))

# winner Raj Guhan


###### consistent eBirder (>=1 eligible list each day) ####

met_week <- function(dates) {
  require(lubridate)
  normal_year <- c((0:363 %/% 7 + 1), 52)
  leap_year   <- c(normal_year[1:59], 9, normal_year[60:365])
  year_day    <- yday(dates)
  return(ifelse(leap_year(dates), leap_year[year_day], normal_year[year_day])) 
}

data2 <- data1 %>% 
  mutate(DAY.Y = yday(OBSERVATION.DATE), 
         WEEK.Y = met_week(OBSERVATION.DATE),
         M.YEAR = if_else(DAY.Y <= 151, YEAR-1, YEAR), # from 1st June to 31st May
         WEEK.MY = if_else(WEEK.Y > 21, WEEK.Y-21, 52-(21-WEEK.Y))) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(N.DAYS = n_distinct(DAY.Y)) %>% 
  filter(N.DAYS >= 365)

# consistent eBirders
data3 <- left_join(data2, eBird.users)
write.csv(data3, "2021/YC_2021_results_cat_consistent.csv", row.names = F)

# random selection 
a <- read.csv("2021/YC_2021_results_cat_consistent.csv")
a <- a %>% filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(20212)
consistent <- a %>% filter(OBSERVER.ID == sample(a$OBSERVER.ID, 1))

# winner Steffin Babu


###### adventurous eBirder (>=4 eligible lists from >=20 districts) ####

data2 <- data1 %>% 
  group_by(OBSERVER.ID, COUNTY) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 4) %>% 
  ungroup() %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(N.DIST = n_distinct(COUNTY)) %>% 
  filter(N.DIST >= 20)

# adventurous eBirders
data3 <- left_join(data2, eBird.users)
write.csv(data3, "2021/YC_2021_results_cat_adventurous.csv", row.names = F)

# random selection 
a <- read.csv("2021/YC_2021_results_cat_adventurous.csv")
a <- a %>% 
  filter(!FULL.NAME %in% c("MetalClicks Ajay Ashok", "Ashwin Viswanathan")) # removes NAs too
set.seed(20213)
adventuruous <- a %>% filter(OBSERVER.ID == sample(a$OBSERVER.ID, 1))

# winner Vivek Menon


###### faithful eBirder (>=180 eligible lists from a single location) ####

data2 <- data1 %>% 
  group_by(OBSERVER.ID, LOCALITY) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 180)

# faithful eBirders
data3 <- left_join(data2, eBird.users)
write.csv(data3, "2021/YC_2021_results_cat_faithful.csv", row.names = F)

# random selection 
a <- read.csv("2021/YC_2021_results_cat_faithful.csv")
a <- a %>% 
  filter(!FULL.NAME %in% c("MetalClicks Ajay Ashok")) # removes NAs too
set.seed(20214)
faithful <- a %>% filter(OBSERVER.ID == sample(a$OBSERVER.ID, 1)) %>% slice(1)

# winner Janardhan Uppada


###### dedicated eBirder (>=500 hours of birding) ####

data2 <- data0 %>% 
  filter(ALL.SPECIES.REPORTED == 1, !is.na(DURATION.MINUTES)) 

data3 <- data2 %>%
  distinct(OBSERVER.ID, SAMPLING.EVENT.IDENTIFIER, DURATION.MINUTES) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(B.TIME.M = sum(DURATION.MINUTES),
            B.TIME.H = round(B.TIME.M/60, 1),
            B.TIME.D = round(B.TIME.H/24, 1)) %>% 
  filter(B.TIME.H >= 500)

# dedicated eBirders
data4 <- left_join(data3, eBird.users) %>% 
  arrange(desc(B.TIME.H))
write.csv(data4, "2021/YC_2021_results_cat_dedicated.csv", row.names = F)

# random selection 
a <- read.csv("2021/YC_2021_results_cat_dedicated.csv")
a <- a %>% 
  filter(!FULL.NAME %in% c("MetalClicks Ajay Ashok")) # removes NAs too
set.seed(20215)
dedicated <- a %>% filter( OBSERVER.ID == sample(a$OBSERVER.ID, 1)) 

# winner Chaiti Banerjee


###### eBirder of the Year (eBirder of the Month >=9 months in 2021) ####

# selection of final excludes the category winners
YC_cat_winners <- bind_rows(prolific, consistent, adventuruous, faithful, dedicated) %>% 
  distinct(OBSERVER.ID, FULL.NAME)


MC_csv_names <- list.files(path = "2021/",
                           pattern = "MC_results_2021_",
                           full.names = T)

data_y <- MC_csv_names %>% 
  lapply(read_csv) %>% 
  bind_rows(.id = "MONTH") %>% 
  group_by(OBSERVER.ID, FULL.NAME) %>% 
  summarise(N.MONTHS = n_distinct(MONTH)) %>% 
  filter(N.MONTHS >= 9) %>% 
  arrange(desc(N.MONTHS))

# eBirders of the Year
write.csv(data_y, "2021/YC_2021_results_eBoY.csv", row.names = F)

# winner Praveen Bennur


###### troubleshooting ####

# finding days when observers missed submitting an eligible list (for consistent eBirder)

x <- data1 %>% 
  filter(OBSERVER.ID == "obsr358210") %>% 
  complete(OBSERVATION.DATE = seq.Date(as_date("2021-01-01"), as_date("2021-12-31"),
                                       length.out = 365),
           OBSERVER.ID = "obsr358210") %>% 
  filter(is.na(SAMPLING.EVENT.IDENTIFIER)) %>% 
  select(OBSERVER.ID, OBSERVATION.DATE)

y <- data1 %>% 
  filter(OBSERVER.ID == "obsr603959") %>% 
  complete(OBSERVATION.DATE = seq.Date(as_date("2021-01-01"), as_date("2021-12-31"),
                                       length.out = 365),
           OBSERVER.ID = "obsr603959") %>% 
  filter(is.na(SAMPLING.EVENT.IDENTIFIER)) %>% 
  select(OBSERVER.ID, OBSERVATION.DATE)

z <- data1 %>% 
  filter(OBSERVER.ID == "obsr701947") %>% 
  complete(OBSERVATION.DATE = seq.Date(as_date("2021-01-01"), as_date("2021-12-31"),
                                       length.out = 365),
           OBSERVER.ID = "obsr701947") %>% 
  filter(is.na(SAMPLING.EVENT.IDENTIFIER)) %>% 
  select(OBSERVER.ID, OBSERVATION.DATE)

a <- data1 %>% 
  filter(OBSERVER.ID == "obsr689831") %>% 
  complete(OBSERVATION.DATE = seq.Date(as_date("2021-01-01"), as_date("2021-12-31"),
                                       length.out = 365),
           OBSERVER.ID = "obsr689831") %>% 
  filter(is.na(SAMPLING.EVENT.IDENTIFIER)) %>% 
  select(OBSERVER.ID, OBSERVATION.DATE)

b <- data1 %>% 
  filter(OBSERVER.ID == "obsr1365122") %>% 
  complete(OBSERVATION.DATE = seq.Date(as_date("2021-01-01"), as_date("2021-12-31"),
                                       length.out = 365),
           OBSERVER.ID = "obsr1365122") %>% 
  filter(is.na(SAMPLING.EVENT.IDENTIFIER)) %>% 
  select(OBSERVER.ID, OBSERVATION.DATE)

missed <- rbind(x, y, z, a, b) %>% 
  left_join(eBird.users) %>% 
  mutate(MISSED.DATE = OBSERVATION.DATE,
         OBSERVATION.DATE = NULL)
write_csv(missed, "2021/YC_2021_missed_consistent.csv")

rm(x, y, z, a, b)
