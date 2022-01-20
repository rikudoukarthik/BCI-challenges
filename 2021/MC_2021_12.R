library(tidyverse)
library(magrittr)

load("2021/ebd_IN_relDec-2021_DEC.RData")

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



###### monthly challenge (December) winners/results ###


# basic eligible list filter
data1 <- data0 %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()


# at least 40 lists in the month
data2 <- data1 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 40) %>%
  ungroup()


# at least 5 media lists

media_csv_names <- list.files(path = "2021/MC_2021_12_media/",
                              pattern = "*.csv",
                              full.names = T)

data_m <- lapply(media_csv_names, read_csv) %>% 
  bind_rows() 

data3 <- data_m %>%
  distinct(`eBird Checklist ID`) %>% 
  mutate(SAMPLING.EVENT.IDENTIFIER = `eBird Checklist ID`,
         `eBird Checklist ID` = NULL)

# # total lists with media 
totmedia <- data3 %$% n_distinct(SAMPLING.EVENT.IDENTIFIER)

data4 <- data1 %>% 
  distinct(OBSERVER.ID, SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(SAMPLING.EVENT.IDENTIFIER %in% data3$SAMPLING.EVENT.IDENTIFIER) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(M.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  ungroup() %>% 
  filter(M.LISTS >= 5)

# at least 5 of 40 with media
data5 <- data2 %>% inner_join(data4)


eBird.users <- read.delim("ebd_users_relJun-2021.txt", sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA))
names(eBird.users) <- c("OBSERVER.ID","FIRST.NAME","LAST.NAME")
eBird.users <- eBird.users %>% transmute(OBSERVER.ID = OBSERVER.ID,
                                         FULL.NAME = paste(FIRST.NAME, LAST.NAME))



data6 <- left_join(data5, eBird.users) %>% 
  mutate(PROP.MLISTS = M.LISTS/NO.LISTS) %>% 
  arrange(desc(PROP.MLISTS))

write.csv(data6, "2021/MC_results_2021_12.csv", row.names = F)



# random selection 
a <- read.csv("2021/MC_results_2021_12.csv")
a <- a %>% filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(10)
filter(a, OBSERVER.ID==sample(a$OBSERVER.ID, 1))

# winner Ramesh Shenai