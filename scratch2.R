
if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if(!require(naniar)) install.packages("naniar")
library(naniar) # gg_miss

# IMPORT DATA
setwd("~/R/historical.MAL")
x <- read_csv("data/History.csv")
glimpse(x)

# VARIABLE LEVELS
x.levels <- cbind(colnames(x),
                  (as.data.frame(sapply(x,function(x) length(unique(x)))))
)
colnames(x.levels) <- c("var","levels")
row.names(x.levels) <- NULL
x.levels[order(-x.levels[,2]),]

# MISSING DATA
gg_miss_var(x, show_pct = T)
gg_miss_which(x)

# start from the left column and fill in to the right

# REQUEST
## request should be sequential, we'll simple make request = to row number
x <- x %>% 
  mutate(Request = seq(1:nrow(x)))
