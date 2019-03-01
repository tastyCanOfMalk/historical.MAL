y %>% 
  filter(!is.na(furnace.name)) %>%
  select(furnace.name,alloy) %>% 
  group_by(furnace.name) %>%
  summarise(n())

yy <- y %>% 
  mutate(year=as.numeric(substring(date.poured,1,4))) %>% 
  filter(!is.na(furnace.name)) %>%
  select(furnace.name,alloy,date.poured,year,year,number.of.castings) %>%
  group_by(furnace.name)
summarise(yy,
          count=n(),
          medianYear=median(year),
          # startDate=min(date.poured),
          # endDate=max(date.poured),
          life=(max(date.poured)-min(date.poured)),
          castings=sum(number.of.castings)) %>% 
          left_join(reshape2::dcast(yy,furnace.name~alloy))
y2 <- summarise(yy,
          count=n(),
          medianYear=median(year),
          startDate=min(date.poured),
          # endDate=max(date.poured),
          life=(max(date.poured)-min(date.poured)),
          uniqueDays=length(unique(date.poured)),
          castings=sum(number.of.castings),
          n = list(enframe(table(alloy))),
          
          ) %>%
  unnest %>%
  spread(name, value, fill = 0)

y2


y %>% 
  filter(!is.na(furnace.name)) %>% 
  mutate(furnace.name=as.factor(furnace.name)) %>% 
  count(furnace.name) %>% 
  arrange(desc(n))

y %>% 
  filter(!is.na(furnace.name)) %>% 
  mutate(alloy=as.factor(alloy)) %>% 
  mutate(all=count(as.factor(alloy)))
  group_by(furnace.name) %>% 
  # mutate(furnace.name=as.factor(furnace.name)) %>% 
  count(alloy) %>% 
  arrange(desc(n))

  