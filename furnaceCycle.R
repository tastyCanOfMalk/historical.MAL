install.packages("lexicon")
library(lexicon)

data("common_names")
common_names[1:10]

# assign each furnace lining a random first name
# then increment with a number

xx <- x %>% 
  select(request, date.received, furnace.cycle)

unique(xx$furnace.cycle)
for (i in 1:nrow(xx)){
  if (xx$furnace.cycle)
}

# two columns?
# furnace = furnace name
# cycle   = how many times furnace used

