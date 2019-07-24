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
  
# lbs per year  
yy %>% 
  group_by(year) %>% 
  summarise(total.lbs = sum(alloy.lbs))

yy %>% 
  ggplot(aes(x=year, y=alloy.lbs))+
  geom_bar(stat="identity") + 
  ggtitle("Lbs poured vs year") + 
  xlab("Year") + 
  ylab ("Lbs poured")
  
# lbs per year by alloy
yy %>% 
  group_by(year, alloy) %>% 
  summarise(total.lbs = sum(alloy.lbs))

yy %>% 
  ggplot(aes(x=year, y=alloy.lbs, fill=alloy))+
  geom_bar(stat="identity") + 
  ggtitle("Lbs poured vs year by alloy") + 
  xlab("Year") + 
  ylab ("Lbs poured")

yy %>% 
  filter(alloy != "stainless") %>% 
  filter(alloy != "simo") %>% 
  filter(alloy != "brass") %>% 
  filter(alloy != "white iron") %>% 
  filter(alloy != "copper") %>% 
  ggplot(aes(x=year, y=alloy.lbs))+
  geom_bar(stat="identity") + 
  facet_wrap(~alloy)+
  ggtitle("Lbs poured vs year by alloy") + 
  xlab("Year") + 
  ylab ("Lbs poured") +
  theme(axis.text.x = element_text(angle=-70,vjust=0.5,hjust=0))
