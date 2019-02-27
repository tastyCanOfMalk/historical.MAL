
# since aluminum furance rarely needs changed, convery all to NA
x$furnace.cycle[x$alloy=="aluminum"] <- NA

xx <- x %>% 
  select(request, furnace.cycle, alloy) %>% 
  filter(alloy != "aluminum") %>% 
  
  # mutate(furnace.cycle=as.factor(furnace.cycle))


install.packages("lexicon")
library(lexicon)

data("common_names")
common_names[1:10]

# assign each furnace lining a random first name
# then increment with a number



gsub("\\d", "",x$furnace.cycle)



xx$furnace.cycle[100:125]
# unique(xx$furnace.cycle)

for (i in 2:nrow(xx)){
  if (xx$furnace.cycle[[i]])
}

xx$furnace.cycle[1:25]

y <- as_tibble(as.factor(unique(xx$furnace.cycle)))

# xx <- dplyr::filter(x, grepl('vits', requested.by)) # m adamovits

# two columns?
# furnace = furnace name
# cycle   = how many times furnace used

