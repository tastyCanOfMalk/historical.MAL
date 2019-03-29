# try CART 
# dummy vars: low, med-l,med,med-h, high
if(!require(rpart)) install.packages("rpart")
library(rpart)
if(!require(rpart.plot)) install.packages("rpart.plot")
library(rpart.plot) # rpart.plot

glimpse(y)
glimpse(y2)

y22 <- y2 %>% 
  select(c(-medianYear,-life,-furnace.name,-castings,-count)) %>% 
  as.matrix()

# create model
fit <- rpart(count~.,y22)
# plot model
rpart.plot(fit,type=2,extra=100)

y22 <- round(prop.table(y22,1),3)*100
y22 <- cbind(y2[2],y22)
y23 <- y22 %>% 
  mutate(level = NA)

for (i in 1:nrow(y23)){
  if(y23$count[[i]]<=25){y23$level[[i]] <- "low"}
  # if(y23$count[[i]]>15 & y23$count[[i]]<=30){y23$level[[i]] <- "mid.low"}
  if(y23$count[[i]]>25 & y23$count[[i]]<=50){y23$level[[i]] <- "mid"}
  # if(y23$count[[i]]>60 & y23$count[[i]]<=75){y23$level[[i]] <- "mid.high"}
  if(y23$count[[i]]>50){y23$level[[i]] <- "high"}
}
glimpse(y23)

y24 <- y23 %>% 
  mutate(level = as.factor(level)) %>% 
  select(-count)

# create model
fit <- rpart(level~.,y24)
# plot model
rpart.plot(fit,type=2,extra=100)

  
