# repeat below but order in terms of date poured rather than count
p1 <- y %>% 
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


# total pours vs lifetime vs pours perday, etc
# lbs of steel (total) used per
# number of castings vs