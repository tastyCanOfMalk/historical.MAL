glimpse(y)

if (!require(viridis)) install.packages("viridis")
library(viridis)

# Histogram of pours per month
y %>% 
  mutate(month=as.factor(substring(months.Date(x$date.poured),1,3))) %>% 
  ggplot(aes(x=month,fill=..count..))+
  geom_histogram(stat="count")+
  scale_x_discrete(limits=c("Jan","Feb","Mar","Apr","May","Jun",
                            "Jul","Aug","Sep","Oct","Nov","Dec"))+
  ggtitle("Pours per month")+
  scale_fill_viridis()+
  theme(legend.position = "none")+
  coord_flip()

# Histogram of pours per year
y %>% 
  mutate(year=as.factor(substring(x$date.poured,1,4))) %>% 
  ggplot(aes(x=year,fill=..count..))+
  geom_histogram(stat="count")+
  ggtitle("Pours per year")+
  scale_fill_viridis()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle=45,hjust=1))

# Pours per month faceted by year
y %>% 
  mutate(month=as.factor(substring(months.Date(x$date.poured),1,3))) %>% 
  mutate(year=as.factor(substring(x$date.poured,1,4))) %>% 
  ggplot(aes(x=month,fill=..count..))+
  geom_histogram(stat="count")+
  scale_x_discrete(limits=c("Jan","Feb","Mar","Apr","May","Jun",
                            "Jul","Aug","Sep","Oct","Nov","Dec"))+
  ggtitle("Pours per month")+
  scale_fill_viridis()+
  theme(legend.position = "none")+
  # coord_flip()+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  facet_wrap(year~.)

y %>% 
  filter(!is.na(furnace.name)) %>% 
  ggplot(aes(x=furnace.name,fill=..count..))+
  geom_histogram(stat="count")+
  coord_flip()
