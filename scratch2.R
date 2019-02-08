# done:
#   notes.ml
#   special.projects


if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if(!require(naniar)) install.packages("naniar")
library(naniar) # gg_miss



# IMPORT DATA
setwd("~/R/historical.MAL")
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
summary(x[c(2:4)])
wrong.dates <- x %>% 
  filter(date.received  > "2019-01-01" |
         date.poured    > "2019-01-01" | 
         date.completed > "2019-01-01")
as.data.frame(wrong.dates)[,1:4]
# manually fix
x$date.poured[1103]    <- as.Date("2002-04-30")
x$date.completed[1198] <- as.Date("2002-08-22")
x$date.poured[2735]    <- as.Date("2002-06-10")
x$date.received[3106]  <- as.Date("2012-03-20")
x$date.received[3149]  <- as.Date("2012-06-19")
x$date.completed[3582] <- as.Date("2016-04-05")
# x[3605:3608,]
x$date.poured[3606] <- as.Date("2017-08-24")
# dates now seem to be in a normal range
summary(x[c(2:4)])

# CALCULATE LEAD TIME
preprocessing.time <- as.numeric(x$date.poured-x$date.received)
postprocessing.time <- as.numeric(x$date.completed-x$date.poured)
lead.time <- preprocessing.time + postprocessing.time
x.temp <- as_tibble(cbind(x,
                          preprocessing.time,
                          postprocessing.time,
                          lead.time))
# check extreme values
summary(x.temp[c(19:21)])

# negative processing times occur when items are input in the incorrect order
# must follow: received < poured < completed

wrong.dates <- x.temp %>% 
  filter(preprocessing.time < 0 |
           postprocessing.time < 0)
wrong.dates <- x.temp %>% 
  filter(preprocessing.time > 200 |
           postprocessing.time > 200)

wrong.dates[c(1,3,2,4,19,20,21)]
# as.data.frame(wrong.dates)[c(1,3,2,4)]

# check median values of anti-joined dataframe
x.anti <- anti_join(x.temp,wrong.dates,c("request"))
summary(x.anti[c(19:21)])



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





      