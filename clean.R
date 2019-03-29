if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if(!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)
if(!require(naniar)) install.packages("naniar")
library(naniar) # gg_miss
if(!require(stringdist)) install.packages("stringdist")
library(stringdist) # soundex?
if(!require(phonics)) install.packages("phonics")
library(phonics) # soundex?
if(!require(lexicon)) install.packages("lexicon")
library(lexicon) # common_names
if(!require(xlsx)) install.packages("xlsx")
library(xlsx)

# clear environment
rm(list=ls())

# IMPORT DATA
setwd("D:/Code/R/historical.MAL")
# setwd("I:/Code-CAD/R/historical.MAL")
x <- read_csv("data/History.csv")
glimpse(x)

# VARIABLE LEVELS
# x.levels <- cbind(colnames(x),
#                   (as.data.frame(sapply(x,function(x) length(unique(x)))))
# )
# colnames(x.levels) <- c("var","levels")
# row.names(x.levels) <- NULL
# x.levels[order(-x.levels[,2]),]

# FUNCTION to get levels by column
get_levels <- function(df, col){
  x.levels <- cbind(colnames(df),
                    (as.data.frame(sapply(df,function(x) length(unique(x)))))
  )
  colnames(x.levels) <- c("var","levels")
  row.names(x.levels) <- NULL
  levels <- x.levels[order(-x.levels[,2]),]
  return(levels[col,])
}

get_levels(x)

# MISSING DATA
gg_miss_var(x, show_pct = T)
gg_miss_which(x)

# RENAME VARIABLES
# names <- colnames(x) %>% 
#   tolower()
names <- tolower(colnames(x))
names <- gsub("  ", " ", names) 
names <- gsub(" ", "\\.", names) 
names[c(12,14)] <- c("alloy.lbs", "sand.lbs")
colnames(x) <- names
colnames(x)

########################
# REQUEST

# request should be sequential, we'll simple make request = to row number
# which(duplicated(x$request)==TRUE)
# as.data.frame(t(x[3609:3611,]))
x <- x %>% 
  mutate(request = seq(1:nrow(x)))
# confirm
# which(duplicated(x$request)==TRUE)
# get_levels(x,1)

###########################
# ID
x <- x %>% 
  select(-id)

###########################
# CONVERT TO DATES
x <- x %>% 
  mutate(date.received  = as.Date(x$date.received,  "%m/%d/%Y")) %>% 
  mutate(date.poured    = as.Date(x$date.poured,    "%m/%d/%Y")) %>% 
  mutate(date.completed = as.Date(x$date.completed, "%m/%d/%Y"))

# check summary
summary(x[c(3,2,4)])

# filter for anything too far in the future
wrong.dates <- x %>% 
  filter(date.received  > "2020-01-01" |
         date.poured    > "2020-01-01" | 
         date.completed > "2020-01-01")

# list wrong dates
as.data.frame(wrong.dates)[,c(1,3,2,4)]

# manually fix
x$date.poured[1103]    <- as.Date("2002-04-30")
x$date.completed[1198] <- as.Date("2002-08-22")
x$date.received[1889]  <- as.Date("2005-11-17")
x$date.poured[2735]    <- as.Date("2002-06-10")
x$date.received[2740]  <- as.Date("2010-06-15")
x$date.received[3106]  <- as.Date("2012-03-20")
x$date.received[3149]  <- as.Date("2012-06-19")
x$date.received[3341]  <- as.Date("2013-11-01")
x$date.completed[3582] <- as.Date("2016-04-05")
x$date.poured[3606]    <- as.Date("2017-08-24")

# dates now seem to be in a normal range
summary(x[c(3,2,4)])

# LEAD TIME
# function to calculate times based on date values
## preprocessing  = time between receiving and pouring
## postprocessing = time between pouring and completed
## lead time      = pre + post-processing
calc_lead <- function(){
  preprocessing.time  <- as.numeric(x$date.poured-x$date.received)
  postprocessing.time <- as.numeric(x$date.completed-x$date.poured)
  lead.time           <- preprocessing.time + postprocessing.time
  x.temp <- as_tibble(cbind(x,
                            preprocessing.time,
                            postprocessing.time,
                            lead.time))
  return(x.temp)
}

summary(calc_lead()[c(19:21)])

# list large positive values
wrong.dates <- calc_lead() %>% 
  filter(preprocessing.time  > 400 |
         postprocessing.time > 400)
wrong.dates[c(1,3,2,4)]

# manually fix
x$date.received[310]  <- as.Date("1999-10-20")
x$date.poured[930]    <- as.Date("2001-08-30")
x$date.poured[1091]   <- as.Date("2002-04-22")
x$date.received[1616] <- as.Date("2004-04-13")
x$date.poured[1668]   <- as.Date("2004-08-04")
x$date.poured[1877]   <- as.Date("2005-11-01")
x$date.received[2229] <- as.Date("2007-01-07")
x$date.poured[2735]   <- as.Date("2010-06-10")
x$date.poured[3133]   <- as.Date("2012-05-17")

# max values look better now
summary(calc_lead()[c(19:21)])

# fix large negative values
wrong.dates <- calc_lead() %>% 
  filter(preprocessing.time  < 0 | 
         postprocessing.time < 0)

wrong.dates[c(1,3,2,4)]

# 234+ rows, definitely not doing this manually
# remove all rows listed, 
# calculate average lead times on remaining dataset,
# insert dates into removed dataset, merge 
x.anti <- anti_join(calc_lead(), wrong.dates, c("request"))
# dim(calc_lead())[[1]] - dim(x.anti)[[1]] == dim(wrong.dates)[[1]]

# anti looks good
summary(x.anti[c(19:21)])

# wrongs look bad
summary(wrong.dates[c(19:21)])

# see some NA values from the calculations
# backtrace to check dates
summary(wrong.dates[c(3,2,4)])

# have NA values in date.completed
# if NA change completed date to received + 9
summary(x.anti[c(19:21)])
# preprocessing.time postprocessing.time   lead.time     
# Median :  6.000    Median :  3.000     Median :  9.00  

for (i in 1:dim(wrong.dates)[1]){
  if (is.na(wrong.dates$date.completed[[i]]) == TRUE){
    wrong.dates$date.completed[[i]] <- wrong.dates$date.received[[i]] + 9
  }
}

# no more NA's
summary(wrong.dates[c(3,2,4)])

# now we fix errors in chronology causing negative time calculations
summary(wrong.dates[c(19:21)])

# start with received coming before poured
wrong.dates$date.received > wrong.dates$date.poured
wrong.dates[c(2,3,6,7),c(3,2,4)]

# then completed coming before poured
wrong.dates$date.poured > wrong.dates$date.completed
wrong.dates$date.completed < wrong.dates$date.poured
wrong.dates[c(1,4,5,8),c(3,2,4)]

for (i in 1:dim(wrong.dates)[1]){
  # preprocessing time = poured - received; median = 6
  if (wrong.dates$date.received[[i]] > wrong.dates$date.poured[[i]]){
    wrong.dates$date.received[[i]] <- wrong.dates$date.poured[[i]] - 6
  }
  # postprocessing time = completed - poured; median = 3
  if (wrong.dates$date.completed[[i]] < wrong.dates$date.poured[[i]]){
    wrong.dates$date.completed[[i]] <- wrong.dates$date.poured[[i]] + 3
  }
}

# confirm chronology
wrong.dates$date.received <= wrong.dates$date.poured
wrong.dates$date.poured <= wrong.dates$date.completed

# wrong.dates df has correct date values, need to join to original
x[c(13,40,41,191),]
wrong.dates[c(1:4),]

# test loop
# test.df <- tibble("row"=numeric())
# counter=1
# for (i in 1:nrow(x)){
#   if (x$request[[i]] == wrong.dates$request[[counter]] && counter<245){
#     test.df[counter, 1] <- x$request[[i]]
#     counter=counter+1
#   }
# }
# another test loop
# y <- x
# counter=1
# for (i in 1:nrow(x)){
#     if (y$request[[i]] == wrong.dates$request[[counter]] && counter < 244){
#       y[i,c(2,3,4)] <- wrong.dates[counter,c(2,3,4)]
#       counter=counter+1
#     }
# }
# x[c(13,40,41,191),]
# wrong.dates[c(1:4),]
# y[c(13,40,41,191),]

# change date values on original dataframe and recalculate
# loop produces an error when counter > nrows, still fine though
counter=1
for (i in 1:nrow(x)){
  if (counter == nrow(wrong.dates)+1){break}
  if (x$request[[i]] == wrong.dates$request[[counter]]){
    x[i,c(2,3,4)] <- wrong.dates[counter,c(2,3,4)]
    counter=counter+1
  }
}
# confirm
x[c(13,40,41,191,3573),]
wrong.dates[c(1:4,244),]

# date summary looks okay now... except for NA's
summary(calc_lead()[c(19:21)])

summary(x)[,c(3,2,4)]

# fill NA values using date.poured to calculate received and completed
# guess on single NA
x %>% filter(is.na(date.poured))
(x[3608:3612,])
(x$date.poured[3611] - x$date.poured[3609]) / 2 # 144 days between dates
# just assign the middle date
x$date.poured[3610] <- x$date.poured[3609] - 72

# now date.poured has no NA's
summary(x)[,c(3,2,4)]

# convert all NA date.received to date.poured-6
x.rec <- x %>% 
  filter(is.na(date.received)) %>% 
  mutate(date.received = date.poured - 6)

# merge back into original df
counter=1
for (i in 1:nrow(x)){
  if (x$request[[i]] == x.rec$request[[counter]]){
    x[i,c(2,3,4)] <- x.rec[counter,c(2,3,4)]
    counter=counter+1
  }
}

# convert all NA date.completed to date.poured+6
x.com <- x %>% 
  filter(is.na(date.completed)) %>% 
  mutate(date.completed = date.poured + 3)

# merge back into original df
counter=1
for (i in 1:nrow(x)){
  if (x$request[[i]] == x.com$request[[counter]]){
    x[i,c(2,3,4)] <- x.com[counter,c(2,3,4)]
    counter=counter+1
  }
}

# NOW we have zero NA values
summary(calc_lead()[c(19:21)])

# assign our new values
x <- calc_lead()

#######################
# NOTES.ML
## seems fine
x$notes.ml[!is.na(x$notes.ml)]
which(is.na(x$notes.ml)==T)

##########################
# SPECIAL.PROJECTS

# list non-NA values in special projects
x$special.projects[!is.na(x$special.projects)]

# find rownums of non-NA vlaues
which(!is.na(x$special.projects)==T)
spec.rows <- which(!is.na(x$special.projects)==T)

# check notes.ml of same rownums
x$notes.ml[spec.rows]

# concatenate the columns
x[spec.rows,] <- x[spec.rows,] %>%
  mutate(notes.ml = paste(notes.ml, special.projects, sep="--"))

# confirm
x$notes.ml[spec.rows]

# remove column
# x <- x %>% 
#   select(-special.projects)

########
# REQUESTED BY

# remove double spaces, commas, periods, caps
x.requestor <- x %>%
  mutate(requested.by = str_replace_all(requested.by, '\\  ', '')) %>% 
  mutate(requested.by = str_replace_all(requested.by, '\\,', '')) %>% 
  mutate(requested.by = str_replace_all(requested.by, '\\.', '')) %>% 
  mutate(requested.by = str_to_lower(requested.by)) %>% 
  select(request, requested.by) %>% 
  mutate(sound = soundex(requested.by,clean=F))

# have some problem values at [1,], [125,]: NA and blank entries
unique(x.requestor$sound)
  
# find problem rows: 1,540,3484
x.requestor %>% 
  filter(sound == "" | is.na(sound))

# check surrounding rows
x[c(1:3,539:542,3483:3485),c(1,5,22)]

# replace NA/number values with next name in line
x$requested.by[c(1,540,3484)] <- x$requested.by[c(2,541,3485)]

x <- x %>%
  mutate(requested.by = str_replace_all(requested.by, '\\  ', '')) %>% 
  mutate(requested.by = str_replace_all(requested.by, '\\,', '')) %>% 
  mutate(requested.by = str_replace_all(requested.by, '\\.', '')) %>% 
  mutate(requested.by = str_to_lower(requested.by)) %>% 
  mutate(sound = soundex(requested.by,clean=F))

length(unique(x$requested.by)) # 204 unique names
length(unique(x$sound))        # 132 unique sounds

unique.names <- unique(x$sound)
# some names are misspelled but have the same sound
# replace any same-sounding with top used name
# replace unique name with most popular unique name filtered by sound
for (i in 1:length(unique.names)){
  # find most popular name of same sounding names
  replacement.name <- x %>% 
    filter(sound == unique.names[[i]]) %>%
    group_by(requested.by) %>% 
    summarise(count=n()) %>% 
    arrange(desc(count))
  replacement.name <- replacement.name[[1]][1]
  # if unique.name == requestor$sound, replace with replacement.name
  x$requested.by[x$sound == unique.names[[i]]] <- 
    replacement.name
}

# 129 unique names now
length(unique(x$requested.by)) 

# but we see mispellings such as adamotvits or lowek
unique(x$requested.by)

# not many options but to skim thru manually
name.levels <- as.data.frame(table(x$requested.by))
name.levels
name.levels %>% arrange(-Freq,Var1)

# notes regarding misspellings
# convert all vits to adamovits
# bald to archibald
# heid to aufderheide
# carr to b carr
# contains yu convert to edward yu
# fountain g to gerry fountaine
# henryc and herry c to henry c
# jigel j, rigelj to rigel j
# all lowes/ lowe to k lowe
# vivas to p vivas
# skolund to skoglund
# showmanc to showman
# yeoman to yeomans

# create some dummy dfs to check filter efficacy
# xx <- dplyr::filter(x, grepl('vits', requested.by)) # m adamovits
# xx <- dplyr::filter(x, grepl('bald', requested.by)) # j archibald
# xx <- dplyr::filter(x, grepl('heid', requested.by)) # r aufderheide
# xx <- dplyr::filter(x, grepl('carr', requested.by)) # b carr
# xx <- dplyr::filter(x, grepl('yu', requested.by)) # e yu
# xx <- dplyr::filter(x, grepl('tain', requested.by)) # g fountaine
# xx <- dplyr::filter(x, grepl('herr', requested.by)) # henry c
# xx <- dplyr::filter(x, grepl('igel', requested.by)) # j rigel
# xx <- dplyr::filter(x, grepl('lowe', requested.by)) # k lowe
# xx <- dplyr::filter(x, grepl('vivas', requested.by)) # p vivas
# xx <- dplyr::filter(x, grepl('lund', requested.by)) # m skoglund
# xx <- dplyr::filter(x, grepl('showm', requested.by)) # r showman
# xx <- dplyr::filter(x, grepl('yeom', requested.by)) # n yeomans

# another dummy to test replacement
xx <- x
# need to duplicate column to avoid most recent if() overwriting previous 
xx %>% 
  mutate(requested.by.tf = ifelse(requested.by =="clingerman m", "TRUE", "FALSE")) %>% 
  # mutate(requested.by.tf = ifelse(requested.by =="belt p", "TRUE", "FALSE")) %>%
  select(requested.by, requested.by.tf)
# adding new column seems to work
xx %>% 
  mutate(requested.by.tf = requested.by) %>%
  mutate(requested.by.tf = ifelse(grepl('man',requested.by), "CLINGER", requested.by.tf)) %>% 
  mutate(requested.by.tf = ifelse(grepl('belt',requested.by), "BELT", requested.by.tf)) %>%
  select(requested.by, requested.by.tf)
# test df using the filters
xx <- xx %>% 
  mutate(requested.by.tf = requested.by) %>%
  mutate(requested.by.tf = ifelse(grepl('vits',requested.by), "mark adamovits", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('bald',requested.by), "jim archibald", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('heid',requested.by), "ron aufderheide", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('carr',requested.by), "ben carr", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('yu',requested.by), "edward yu", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('tain',requested.by), "gerry fountaine", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('herr',requested.by), "henry c", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('henr',requested.by), "henry c", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('igel',requested.by), "judy rigel", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('lowe',requested.by), "kathy lowe", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('vivas',requested.by), "paula vivas", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('lund',requested.by), "m skoglund", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('showm',requested.by), "ralph showman", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('wang',requested.by), "xianping wang", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('hart',requested.by), "matt hartman", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('yeom',requested.by), "n yeomans", requested.by.tf))
  # select(requested.by, requested.by.tf)

# checking names again, seems like most are alright, only 104 unique now
name.levels <- as.data.frame(table(xx$requested.by.tf))
name.levels
name.levels %>% arrange(-Freq,Var1)
  
# actually do the fixing
x <- x %>% 
  mutate(requested.by.tf = requested.by) %>%
  mutate(requested.by.tf = ifelse(grepl('vits',requested.by), "mark adamovits", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('bald',requested.by), "jim archibald", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('heid',requested.by), "ron aufderheide", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('carr',requested.by), "ben carr", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('yu',requested.by), "edward yu", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('tain',requested.by), "gerry fountaine", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('herr',requested.by), "henry c", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('henr',requested.by), "henry c", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('igel',requested.by), "judy rigel", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('lowe',requested.by), "kathy lowe", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('vivas',requested.by), "paula vivas", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('lund',requested.by), "m skoglund", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('showm',requested.by), "ralph showman", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('wang',requested.by), "xianping wang", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('hart',requested.by), "matt hartman", requested.by.tf)) %>%
  mutate(requested.by.tf = ifelse(grepl('yeom',requested.by), "n yeomans", requested.by.tf)) %>% 
  mutate(requested.by = requested.by.tf) %>% 
  select(-requested.by.tf)

# seems to be good enough
unique(x$requested.by)
length(unique(x$requested.by)) # 106
#########################
# CUSTOMER NAMES

# only 2 missing customer names, replace with ASK
x %>%
  filter(is.na(x$customer.name)==TRUE)
x$customer.name[c(3366,3377)] <- "ASK"

#####################
# ALLOY USED

unique(x$alloy)
length(unique(x$alloy)) # 60

# convert case, remove punctuations
x <- x %>%
  mutate(alloy = str_replace_all(alloy, '\\  ', '')) %>% 
  mutate(alloy = str_replace_all(alloy, '\\,', '')) %>% 
  mutate(alloy = str_replace_all(alloy, '\\.', '')) %>% 
  mutate(alloy = str_to_lower(   alloy))

length(unique(x$alloy)) # 47

x <- x %>% 
  mutate(alloy.new = alloy) %>%
  mutate(alloy.new = str_replace_all(alloy.new, "[:punct:]","none")) %>% 
  mutate(alloy.new = ifelse(grepl('al',alloy), "aluminum", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('di',alloy),      "ductile iron", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('ductile',alloy), "ductile iron", alloy.new)) %>%
  mutate(alloy.new = ifelse(grepl('le iron',alloy), "ductile iron", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('gray',alloy),   "grey iron", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('y iron',alloy), "grey iron", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('cg',alloy), "cgi", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('brass',alloy), "bras", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('s steel',alloy), "stainless", alloy.new)) %>%
  mutate(alloy.new = ifelse(grepl('44',alloy),      "stainless", alloy.new)) %>%
  mutate(alloy.new = ifelse(grepl('ss',alloy),      "stainless", alloy.new)) %>%
  mutate(alloy.new = ifelse(grepl('teel',alloy), "lc steel", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('bras',alloy), "brass", alloy.new)) %>% 
  mutate(alloy.new = ifelse(alloy.new == "0" | 
                              alloy.new == "none" | 
                              alloy.new == "unknown", NA, alloy.new)) %>% 
  mutate(alloy = alloy.new) %>% 
  select(-alloy.new)

# confirm
unique(x$alloy)
length(unique(x$alloy)) # 11

########################
# FURNACE CYCLE
# split into furnace and cycle

# since aluminum uses a different furnace, change all to NA
x$furnace.cycle[x$alloy=="aluminum"] <- NA

# test df
# select first letter to call furnace
xx <- x %>% 
  select(request, furnace.cycle, alloy) %>% 
  filter(alloy != "aluminum") %>% 
  mutate(furnace = str_sub(furnace.cycle,1,1)) %>% 
  mutate(furnace = str_to_lower(furnace)) %>% 
  mutate(cycle = NA) %>% 
  mutate(furnace.name = NA)

# some NA values
xx[is.na(xx$furnace.cycle),]

# if NA, pull value above
for (i in 1:nrow(xx)){
  if (is.na(xx$furnace[[i]])){
    xx$furnace[[i]] <- xx$furnace[[i-1]]
  }
}

# zeros confused with letter o
xx[1543:1576,]
# replace zeros with o's
xx[xx$furnace==0,][4] <- "o"

# M between L's
xx[2679:2684,]
xx[xx$request==3468,][4] <- "m"

# O between N's
xx[2725:2729,]
# switch place
which(xx$request==3517) # 2726
xx[2726,][1] <- 3518
xx[2727,][1] <- 3517

xx <- xx %>% 
  arrange(request)

# increment if furnace before = furnace current
# if not, assign value = 1
cycle.counter=1
for (i in 2:nrow(xx)){
  # first row = 1
  xx$cycle[[1]] <- 1
  # vars 
  before =  i-1
  current = i
  # current != before, start counter over
  if (xx$furnace[[current]] != xx$furnace[[before]]){
    cycle.counter=1
    xx$cycle[[current]] <- cycle.counter
  }
  if (xx$furnace[[current]] == xx$furnace[[before]]){
    cycle.counter=cycle.counter+1
    xx$cycle[[current]] <- cycle.counter
  }
}

data("common_names")
names <- common_names[1:length(common_names)]
set.seed(1111)
names <- sample(names)

name.counter = 0
for (i in 1:nrow(xx)){
  if (xx$cycle[[i]] == 1){
    name.counter=name.counter+1
    xx$furnace.name[[i]] <- names[[name.counter]]
  }
  if (xx$cycle[[i]] != 1){
    xx$furnace.name[[i]] <- names[[name.counter]]
  }
}

x <- full_join(x,xx)

#################################
# CASTING TYPE

# way too many unique
unique(x$casting.type)
length(unique(x$casting.type)) # 274

# remove double spaces, commas, periods
x <- x %>%
  mutate(casting.type1 = str_replace_all(casting.type, '\\  ', ' ')) %>% 
  mutate(casting.type1 = str_replace_all(casting.type, '\\,', ' ')) %>%
  mutate(casting.type1 = str_replace_all(casting.type, '\\.', ' ')) %>%
  mutate(casting.type1 = str_to_lower(   casting.type)) %>% 
  mutate(casting.type = casting.type1) %>% 
  select(-casting.type1)

unique(x$casting.type)
length(unique(x$casting.type)) # 264

# xxx <- xx %>% 
#   filter(grepl('filter', casting.type))
# unique(xxx$casting.type)

x <- x %>% 
  mutate(casting.type1 = casting.type) %>%
  mutate(casting.type1 = ifelse(grepl('cube',casting.type), "shrink cube", casting.type1)) %>% 
  mutate(casting.type1 = ifelse(grepl('ero',casting.type), "erosion wedge", casting.type1)) %>% 
  mutate(casting.type1 = ifelse(grepl('sleeve',casting.type), "sleeves", casting.type1)) %>% 
  mutate(casting.type1 = ifelse(grepl('shake',casting.type), "shakeout tree", casting.type1)) %>% 
  mutate(casting.type1 = ifelse(grepl('page',casting.type), "warpage block", casting.type1)) %>% 
  mutate(casting.type1 = ifelse(grepl('ration',casting.type), "penetration", casting.type1)) %>% 
  mutate(casting.type1 = ifelse(grepl('pene',casting.type), "penetration", casting.type1)) %>%
  mutate(casting.type1 = ifelse(grepl('graphit',casting.type), "graphite step", casting.type1)) %>% 
  mutate(casting.type1 = ifelse(grepl('cone',casting.type), "stepcone", casting.type1)) %>% 
  mutate(casting.type1 = ifelse(grepl('steps',casting.type), "stepcone", casting.type1)) %>% 
  mutate(casting.type1 = ifelse(grepl('graphite',casting.type), "graphite stepcones", casting.type1)) %>% 
  mutate(casting.type1 = ifelse(grepl('fluid',casting.type), "fluidity spiral", casting.type1)) %>% 
  mutate(casting.type1 = ifelse(grepl('buck',casting.type), "belt buckles", casting.type1)) %>% 
  mutate(casting.type1 = ifelse(grepl('gator',casting.type), "gator", casting.type1)) %>% 
  mutate(casting.type1 = ifelse(grepl('soot p',casting.type), "sootplate", casting.type1)) %>% 
  mutate(casting.type1 = ifelse(grepl('gear',casting.type), "gear mold", casting.type1)) %>% 
  mutate(casting.type1 = ifelse(grepl('rotor',casting.type), "brake rotor", casting.type1)) %>% 
  mutate(casting.type1 = ifelse(grepl('pig',casting.type), "pigs", casting.type1)) %>%
  mutate(casting.type1 = ifelse(grepl('impel',casting.type), "di impeller", casting.type1)) %>% 
  mutate(casting.type = casting.type1) %>% 
  select(-casting.type1)

unique(x$casting.type)
length(unique(x$casting.type)) # 120

# xxx <- xx %>%
#   filter(grepl('pig', casting.type1))
# unique(xxx$casting.type1)

# slightly better, not perfect

#################################
# PRODUCT TESTED
unique(x$product.tested)
length(unique(x$product.tested)) # 175

x <- x %>%
  mutate(product.tested1 = str_replace_all(product.tested, '\\ ', ' ')) %>% 
  mutate(product.tested1 = str_replace_all(product.tested, '\\,', ' ')) %>% 
  mutate(product.tested1 = str_replace_all(product.tested, '\\.', ' ')) %>% 
  mutate(product.tested1 = str_to_lower(   product.tested)) %>% 
  mutate(product.tested = product.tested1) %>% 
  select(-product.tested1)

unique(xx$product.tested)
length(unique(xx$product.tested)) # 170
# 
# xxx <- xx %>%
#   filter(grepl('pep s', product.tested))
# unique(xxx$product.tested)
# 
# xx <- xx %>% 
#   mutate(product.tested1 = product.tested) %>%
#   mutate(product.tested1 = ifelse(grepl('pep=',product.tested), "pepset", product.tested1))  
#   mutate(product.tested = product.tested1) %>% 
#   select(-product.tested1)
# 
# unique(xx$product.tested)
# length(unique(xx$product.tested))
  
#################################
# SAND TYPE
unique(x$sand.type)
length(unique(x$sand.type)) # 113

x <- x %>%
  mutate(sand.type1 = str_replace_all(sand.type, '\\ ', ' ')) %>% 
  mutate(sand.type1 = str_replace_all(sand.type, '\\,', ' ')) %>% 
  mutate(sand.type1 = str_replace_all(sand.type, '\\.', ' ')) %>% 
  mutate(sand.type1 = str_to_lower(   sand.type)) %>% 
  mutate(sand.type = sand.type1) %>% 
  select(-sand.type1)

# unique(x$sand.type)
length(unique(x$sand.type)) # 111
# 
# xxx <- xx %>%
#   filter(grepl('410', sand.type))
# unique(xxx$sand.type)

x <- x %>%
  mutate(sand.type1 = sand.type) %>%
  mutate(sand.type1 = ifelse(grepl('w41',sand.type), "w410", sand.type1)) %>% 
  mutate(sand.type = sand.type1) %>%
  select(-sand.type1)

# unique(x$sand.type)
length(unique(x$sand.type)) # 102

################################
get_levels(x)
gg_miss_var(x, show_pct = T)
gg_miss_which(x)
glimpse(x)

y <- x %>% 
  select(request,
         date.received,
         date.poured,
         date.completed,
         requested.by,
         customer.name,
         product.tested,
         casting.type,
         number.of.castings,
         alloy,
         alloy.lbs,
         sand.type,
         sand.lbs,
         total.hours,
         total.cost,
         preprocessing.time,
         postprocessing.time,
         lead.time,
         furnace.name,
         cycle,
         notes.ml) %>% 
  mutate(requested.by=as.factor(requested.by)) %>% 
  mutate(customer.name=as.factor(customer.name)) %>% 
  mutate(product.tested=as.factor(product.tested)) %>% 
  mutate(casting.type=as.factor(casting.type)) %>% 
  mutate(alloy=as.factor(alloy)) %>% 
  mutate(sand.type=as.factor(sand.type)) %>% 
  mutate(furnace.name=as.factor(furnace.name)) 
glimpse(y)

write.xlsx(y, file="/data/cleanedMAL.xlsx", sheetName="Sheet1", 
           col.names=TRUE, row.names=TRUE, append=FALSE)
  