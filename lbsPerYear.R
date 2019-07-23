if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# output lbs poured per year

glimpse(y)

# select columns
yy <- y %>% 
  mutate(year = format(date.received, "%Y")) %>%
  mutate(year = factor(year)) %>% 
  select(year, alloy, alloy.lbs) %>% 
  drop_na()
  
  
yy %>% 
  group_by(year) %>% 
  summarise(total.lbs = sum(alloy.lbs)) 

yy %>% 
  ggplot(aes(x=year, year=alloy.lbs))+
  geom_bar()

# aggregate(yy$alloy.lbs,list(yy$year),sum)

# yy %>% 
#   count(year)
