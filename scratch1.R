# https://stackoverflow.com/questions/16837461/dealing-with-wrong-spelling-when-matching-text-strings-in-r
# https://stackoverflow.com/questions/45990947/how-to-find-a-typo-in-a-data-frame-and-replace-it
# https://cran.r-project.org/web/packages/stringdist/stringdist.pdf

setwd("~/R/historical.MAL")

if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
# if(!require(tibble)) install.packages("tibble")
# library(tibble)
# if(!require(lubridate)) install.packages("lubridate")
# library(lubridate)
# if(!require(Amelia)) install.packages("Amelia")
# library(Amelia) # missmap
# if(!require(visdat)) install.packages("visdat")
# library(visdat) # vis_miss
if(!require(naniar)) install.packages("naniar")
library(naniar) # gg_miss
# if(!require(stringdist)) install.packages("stringdist")
# library(stringdist) 

x <- read_csv("data/History.csv")
glimpse(x)

# x.1 <- x[1:10,]
# glimpse(x.1)
# convert all dates to POSIXct
# mdy(x.1$`Date  Poured`)

# get unique variables levels from each variable
x.levels <- cbind(colnames(x),
                  (as.data.frame(sapply(x,function(x) length(unique(x)))))
)
colnames(x.levels) <- c("var","levels")
row.names(x.levels) <- NULL
x.levels[order(-x.levels[,2]),]

# get proportion of NA/missing data
missmap(x,rank.order = T,y.cex=.2)
vis_miss(x,sort_miss = T)
gg_miss_var(x, show_pct = T)
gg_miss_which(x)

# fill in missing data...
## Some columns won't need filled in:
# * Special projects
# * Notes ML
# * Furnace cycle?


x$`Requested by`[is.na(x$`Requested by`)] <- 0

dist.matrix<-stringdistmatrix(x$`Requested by`,
                              x$`Requested by`,
                              method='jw',p=0.1)
row.names(dist.matrix)<-x$`Requested by`
names(dist.matrix)<-x$`Requested by`
dist.matrix<-as.dist(dist.matrix)
clusts<-hclust(dist.matrix,method="ward.D2")
plot(clusts)
  
length(unique(x$`Requested by`))
xx <- gsub('[[:punct:] ]+',' ',x$`Requested by`) # remove punctuation
length(unique(xx))
xx <- str_trim(xx) # remove extra whitespaces
length(unique(xx))
xx <- tolower(xx)
length(unique(xx))
str_extract(xx, '[^ ]+$')

xx.fac <- as.factor(as.matrix(xx))
xx.fac <- data.frame(xx.fac)
xx.count <- xx.fac %>% 
  count(xx.fac,sort=T)









# http://openrefine.org/download.html
# approx string matching:
# https://cran.r-project.org/doc/contrib/de_Jonge+van_der_Loo-Introduction_to_data_cleaning_with_R.pdf
y <- x %>% 
  mutate(Date.Poured = mdy(x$`Date  Poured`)) %>% 
  mutate(Date.Received = mdy(x$`Date Received`)) %>% 
  mutate(Date.Completed = mdy(x$`Date Completed`))
glimpse(y)  
  