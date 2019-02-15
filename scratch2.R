# done:
#   notes.ml
#   special.projects


if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if(!require(naniar)) install.packages("naniar")
library(naniar) # gg_miss
if(!require(stringdist)) install.packages("stringdist")
library(stringdist) # soundex

# clear environment
rm(list=ls())

# IMPORT DATA
setwd("~/R/historical.MAL")
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
glimpse(x)

# REQUEST
## request should be sequential, we'll simple make request = to row number
which(duplicated(x$request)==TRUE)
as.data.frame(t(x[3609:3611,]))
x <- x %>% 
  mutate(request = seq(1:nrow(x)))
# confirm
which(duplicated(x$request)==TRUE)
get_levels(x,1)

# ID
x <- x %>% 
  select(-id)

# CONVERT TO DATES
x <- x %>% 
  mutate(date.received = as.Date(x$date.received, "%m/%d/%Y")) %>% 
  mutate(date.poured = as.Date(x$date.poured, "%m/%d/%Y")) %>% 
  mutate(date.completed = as.Date(x$date.completed, "%m/%d/%Y"))

# FIND DATE TYPOS
summary(x[c(3,2,4)])
# filter for anything too far in the future
wrong.dates <- x %>% 
  filter(date.received  > "2020-01-01" |
         date.poured    > "2020-01-01" | 
         date.completed > "2020-01-01")
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
# x[3605:3608,]
x$date.poured[3606] <- as.Date("2017-08-24")
# dates now seem to be in a normal range
summary(x[c(3,2,4)])

# CALCULATE LEAD TIME
## create function so that results of editing can be seen quickly
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
# calc_lead()
# check extreme values
# summary(x.temp[c(19:21)])
summary(calc_lead()[c(19:21)])

## fix large values
wrong.dates <- calc_lead() %>% 
  filter(preprocessing.time > 400 |
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

# fix negatives
# must follow: received < poured < completed
wrong.dates2 <- calc_lead() %>% 
  filter(preprocessing.time < 0 | postprocessing.time < 0)
wrong.dates2[c(1,3,2,4)]
# 234+ rows, definitely not doing this manually
# remove all rows listed, 
# calculate average lead times on remaining dataset,
# insert dates into removed dataset, merge 

x.anti <- anti_join(calc_lead(), wrong.dates2, c("request"))
# dim(calc_lead())[[1]] - dim(x.anti)[[1]] == dim(wrong.dates2)[[1]]
# anti looks good
summary(x.anti[c(19:21)])
# wrongs look bad
summary(wrong.dates2[c(19:21)])
# see some NA values from the calculations
# backtrace to check dates
summary(wrong.dates2[c(3,2,4)])
# have NA values in date.completed
# if NA change completed date to received + 9

# > summary(x.anti[c(19:21)])
# preprocessing.time postprocessing.time   lead.time     
# Median :  6.000    Median :  3.000     Median :  9.00  

for (i in 1:dim(wrong.dates2)[1]){
  if (is.na(wrong.dates2$date.completed[[i]]) == TRUE){
    wrong.dates2$date.completed[[i]] <- wrong.dates2$date.received[[i]] + 9
  }
}
# no more NA's
summary(wrong.dates2[c(3,2,4)])

# now we fix errors in chronology causing negative time calculations
summary(wrong.dates2[c(19:21)])

# start with received coming before poured
wrong.dates2$date.received > wrong.dates2$date.poured
wrong.dates2[c(2,3,6,7),c(3,2,4)]

# then completed coming before poured
wrong.dates2$date.poured > wrong.dates2$date.completed
wrong.dates2$date.completed < wrong.dates2$date.poured
wrong.dates2[c(1,4,5,8),c(3,2,4)]

for (i in 1:dim(wrong.dates2)[1]){
  # preprocessing time = poured - received; median = 6
  if (wrong.dates2$date.received[[i]] > wrong.dates2$date.poured[[i]]){
    wrong.dates2$date.received[[i]] <- wrong.dates2$date.poured[[i]] - 6
  }
  # postprocessing time = completed - poured; median = 3
  if (wrong.dates2$date.completed[[i]] < wrong.dates2$date.poured[[i]]){
    wrong.dates2$date.completed[[i]] <- wrong.dates2$date.poured[[i]] + 3
  }
}

# confirm chronology
wrong.dates2$date.received <= wrong.dates2$date.poured
wrong.dates2$date.poured <= wrong.dates2$date.completed

# wrong.dates2 df has correct date values, need to join to original
x[c(13,40,41,191),]
wrong.dates2[c(1:4),]

# test loop
# test.df <- tibble("row"=numeric())
# counter=1
# for (i in 1:nrow(x)){
#   if (x$request[[i]] == wrong.dates2$request[[counter]] && counter<245){
#     test.df[counter, 1] <- x$request[[i]]
#     counter=counter+1
#   }
# }
# another test loop
# y <- x
# counter=1
# for (i in 1:nrow(x)){
#     if (y$request[[i]] == wrong.dates2$request[[counter]] && counter < 244){
#       y[i,c(2,3,4)] <- wrong.dates2[counter,c(2,3,4)]
#       counter=counter+1
#     }
# }
# x[c(13,40,41,191),]
# wrong.dates2[c(1:4),]
# y[c(13,40,41,191),]

# change date values on original dataframe and recalculate
# loop produces an error when counter > nrows, still fine though
counter=1
for (i in 1:nrow(x)){
    if (x$request[[i]] == wrong.dates2$request[[counter]]){
      x[i,c(2,3,4)] <- wrong.dates2[counter,c(2,3,4)]
      counter=counter+1
    }
}
# confirm
x[c(13,40,41,191),]
wrong.dates2[c(1:4),]

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

x.rec <- x %>% 
  filter(is.na(date.received)) %>% 
  mutate(date.received = date.poured - 6)

counter=1
for (i in 1:nrow(x)){
  if (x$request[[i]] == x.rec$request[[counter]]){
    x[i,c(2,3,4)] <- x.rec[counter,c(2,3,4)]
    counter=counter+1
  }
}

x.com <- x %>% 
  filter(is.na(date.completed)) %>% 
  mutate(date.completed = date.poured + 3)

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
# filter for unique sounds
x.requestor <- x %>%
  mutate(requested.by = str_replace_all(requested.by, '\\  ', '')) %>% 
  mutate(requested.by = str_replace_all(requested.by, '\\,', '')) %>% 
  mutate(requested.by = str_replace_all(requested.by, '\\.', '')) %>% 
  mutate(requested.by = str_to_lower(requested.by)) %>% 
  select(request, requested.by) %>% 
  mutate(sound = soundex(requested.by))

# have some problem values at [1,], [125,]
  unique(x.requestor$sound)
  
# problem rows: 1,540,3484
x.requestor %>% 
  filter(sound == "" | is.na(sound))
x[c(1:3,539:542,3483:3485),]
# replace NA/number values with next name in line
x$requested.by[c(1,540,3484)] <- x$requested.by[c(2,541,3485)]

x <- x %>%
  mutate(requested.by = str_replace_all(requested.by, '\\  ', '')) %>% 
  mutate(requested.by = str_replace_all(requested.by, '\\,', '')) %>% 
  mutate(requested.by = str_replace_all(requested.by, '\\.', '')) %>% 
  mutate(requested.by = str_to_lower(requested.by)) %>% 
  mutate(sound = soundex(requested.by))

length(unique(x$requested.by)) # 204 unique names
length(unique(x$sound))        # 134 unique sounds

unique.names <- unique(x$sound)

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

length(unique(x$requested.by)) # 134 unique names now


















unique(x$requested.by)
# alloy.lbs NA values
summary(is.na(x))
x %>%
  filter(is.na(x$alloy.lbs)==TRUE)




      
get_levels(x)
gg_miss_var(x, show_pct = T)
gg_miss_which(x)
glimpse(x)