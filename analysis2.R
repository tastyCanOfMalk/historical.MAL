if (!require(reshape2)) install.packages("reshape2")
library(reshape2)

y1 <- y %>% 
  mutate(year=as.numeric(substring(date.poured,1,4))) %>% 
  filter(!is.na(furnace.name)) %>%
  select(furnace.name,alloy,date.poured,year,number.of.castings) %>% 
  group_by(furnace.name)

# method 1 to spread alloys
y2 <- summarise(y1,
          count=n(),
          medianYear=median(year),
          # startDate=min(date.poured),
          # endDate=max(date.poured),
          life=(max(date.poured)-min(date.poured)),
          castings=sum(number.of.castings)) %>% 
  left_join(reshape2::dcast(y1,furnace.name~alloy))

# method 2 to spread alloys
y3 <- summarise(y1,
                count=n(),
                medianYear=median(year),
                life=(max(date.poured)-min(date.poured)),
                uniqueDays=length(unique(date.poured)),
                castings=sum(number.of.castings),
                n = list(enframe(table(alloy))),
) %>%
  unnest %>%
  spread(name, value, fill = 0)


y2.melt <- y2 %>% 
  # arrange(desc(count)) %>% 
  select(-count,-medianYear,-life,-castings) %>% 
  melt()

y2.melt %>% 
  ggplot(aes(x=reorder(furnace.name,value),y=value,fill=variable)) + 
  geom_bar(stat="identity")+ 
  scale_fill_viridis(discrete=TRUE,option=2) +
  coord_flip()
