glimpse(y)

if (!require(viridis)) install.packages("viridis")
library(viridis)
if (!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)

# Histogram of pours per month
g1 <- y %>% 
  mutate(month=as.factor(substring(months.Date(x$date.poured),1,3))) %>% 
  ggplot(aes(x=month,fill=..count..))+
  geom_histogram(stat="count")+
  scale_x_discrete(limits=c("Jan","Feb","Mar","Apr","May","Jun",
                            "Jul","Aug","Sep","Oct","Nov","Dec"))+
  ggtitle("Cumulative pours over all months")+
  scale_fill_viridis()+
  theme(legend.position = "none")+
  coord_flip()

# Histogram of pours per year
g2 <- y %>% 
  mutate(year=as.factor(substring(x$date.poured,1,4))) %>% 
  ggplot(aes(x=year,fill=..count..))+
  geom_histogram(stat="count")+
  ggtitle("Pours per year")+
  scale_fill_viridis()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle=45,hjust=1))

grid.arrange(g1,g2,ncol=2)

# Pours per month faceted by year
y %>% 
  mutate(month=as.factor(substring(months.Date(x$date.poured),1,3))) %>% 
  mutate(year=as.factor(substring(x$date.poured,1,4))) %>% 
  ggplot(aes(x=month,fill=..count..))+
  geom_histogram(stat="count")+
  scale_x_discrete(limits=c("Jan","Feb","Mar","Apr","May","Jun",
                            "Jul","Aug","Sep","Oct","Nov","Dec"))+
  ggtitle("Pours per month per year")+
  scale_fill_viridis()+
  theme(legend.position = "none")+
  # coord_flip()+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  facet_wrap(year~.)

# barplot of longest lasting furnaces
y %>% 
  filter(!is.na(furnace.name)) %>% 
  mutate(furnace.name=as.factor(furnace.name)) %>% 
  count(furnace.name) %>% 
  # arrange(desc(n)) %>% 
  filter(n>50) %>% 
  ggplot(aes(x=reorder(furnace.name,n),y=n,fill=n))+
  geom_bar(stat="identity")+
  coord_flip()+
  scale_fill_viridis()+
  theme(legend.position = "none")+
  ggtitle("Longest lasting furnaces, n>50")

# table of longest lasting furnaces
df.furnace <- y %>% 
  filter(!is.na(furnace.name)) %>% 
  mutate(furnace.name=as.factor(furnace.name)) %>% 
  count(furnace.name) %>% 
  arrange(desc(n))

y %>% 
  filter(furnace.name == df.furnace[[1]][1])


# create bew df?
# furnace name, number cycles, number iron/steel pours




