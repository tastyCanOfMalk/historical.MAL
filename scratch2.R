# done:
#   notes.ml
#   special.projects


if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if(!require(naniar)) install.packages("naniar")
library(naniar) # gg_miss



# IMPORT DATA
# setwd("~/R/historical.MAL")
setwd("I:/Code-CAD/R/historical.MAL")
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

# max values better
summary(calc_lead()[c(19:21)])


# fix negatives
# must follow: received < poured < completed
wrong.dates2 <- calc_lead() %>% 
  filter(preprocessing.time < 0 | postprocessing.time < 0)
wrong.dates2[c(1,3,2,4)]
# definitely not doing this manually
# remove all rows listed, 
# calculate average lead times on remaining dataset,
# insert dates into removed dataset, merge 


x.anti <- anti_join(calc_lead(), wrong.dates2, c("request"))
# dim(calc_lead())[[1]] - dim(x.anti)[[1]] == dim(wrong.dates2)[[1]]
# anti look good
summary(x.anti[c(19:21)])
# wrongs look bad
summary(wrong.dates2[c(19:21)])


# > summary(x.anti[c(19:21)])
# preprocessing.time postprocessing.time   lead.time     
# Min.   :  0.000    Min.   :  0.000     Min.   :  0.00  
# 1st Qu.:  3.000    1st Qu.:  1.000     1st Qu.:  6.00  
# Median :  6.000    Median :  3.000     Median :  9.00  
# Mean   :  8.141    Mean   :  5.218     Mean   : 13.11  
# 3rd Qu.:  8.000    3rd Qu.:  6.000     3rd Qu.: 14.00  
# Max.   :372.000    Max.   :308.000     Max.   :434.00  
# NA's   :43         NA's   :225         NA's   :228     

wrong.dates2$date.received > wrong.dates2$date.poured
wrong.dates2$date.poured > wrong.dates2$date.completed
wrong.dates2$date.received > wrong.dates2$date.completed

wrong.dates2$date.completed 
wrong.dates2[,c(1,3,2,4)]

wrong.dates2[7,]
wrong.dates2$date.received[[2]]
wrong.dates2$date.poured[[2]]
wrong.dates2$date.poured[[2]] - 6

wrong.dates2$date.received[[6]] > wrong.dates2$date.completed[[6]]
wrong.dates2[6,]


# NA values screw up the loop below
wrong.dates2 %>% 
  filter(is.na(date.received) == T | 
           is.na(date.poured) == T | 
           is.na(date.completed) == T)


for (i in 1:nrow(wrong.dates2)){
 if (wrong.dates2$date.received[[i]] > wrong.dates2$date.poured[[i]]){
   wrong.dates2$date.received[[i]] <- wrong.dates2$date.poured[[i]] - 6
 }
 if (wrong.dates2$date.poured[[i]] > wrong.dates2$date.completed[[i]]){
   wrong.dates2$date.completed[[i]] <- wrong.dates2$date.poured[[i]] + 3
 }
 # if (wrong.dates2$date.received[[i]] > wrong.dates2$date.completed[[i]]){
 #   wrong.dates2$date.received[[i]] <- wrong.dates2$date.completed[[i]] - 9
 # }
}



# as.data.frame(wrong.dates)[c(1,3,2,4)]

# check median values of anti-joined dataframe





#######################

x.temp %>% 
  filter(postprocessing.time < 0)


which.min(x.temp$preprocessing.time)


which.min(x.temp$lead.time)
as.data.frame(x.temp[3058,])
# date mislabeled as 2513 instead of 2013
x$date.received[3341] <- as.Date("2013-11-01")



# find NA values dates
which(is.na(x$date.received)==T)
which(is.na(x$date.poured)==T)
which(is.na(x$date.completed)==T)


# DATES
xx <- which(is.na(x$date.received)==T)
x[xx,]


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





      