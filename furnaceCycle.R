
# since aluminum uses a different furnace, change all to NA
x$furnace.cycle[x$alloy=="aluminum"] <- NA

# test df
# select first letter to call furnace
xx <- x %>% 
  select(request, furnace.cycle, alloy) %>% 
  filter(alloy != "aluminum") %>% 
  mutate(furnace = str_sub(furnace.cycle,1,1)) %>% 
  mutate(cycle = NA) %>% 
  mutate(name = NA)

# some NA values
xx[is.na(xx$furnace.cycle),]

# if NA, pull value above
for (i in 1:nrow(xx)){
  if (is.na(xx$furnace[[i]])){
    xx$furnace[[i]] <- xx$furnace[[i-1]]
  }
}

# increment if furnace before = furnace current
# if not, assign value = 1
cycle.counter=1
for (i in 2:nrow(xx)){
  # first row = 1
  xx$cycle[[1]] <- 1
  # vars 
  before =  i-1
  current = i
  # current != before, start counter over
  if (xx$furnace[[current]] != xx$furnace[[before]]){
    cycle.counter=1
    xx$cycle[[current]] <- cycle.counter
  }
  if (xx$furnace[[current]] == xx$furnace[[before]]){
    cycle.counter=cycle.counter+1
    xx$cycle[[current]] <- cycle.counter
  }
}
# add some personality to the furnace names
if(!require(lexicon)) install.packages("lexicon")
library(lexicon)

data("common_names")
names <- common_names[1:length(common_names)]
names <- sample(names)

name.counter = 0
for (i in 1:nrow(xx)){
  if (xx$cycle[[i]] == 1){
    name.counter=name.counter+1
    xx$name[[i]] <- names[[name.counter]]
  }
  if (xx$cycle[[i]] != 1){
    xx$name[[i]] <- names[[name.counter]]
  }
}

x <- full_join(x,xx)
