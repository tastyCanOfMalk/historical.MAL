
# remove NA

# remove double spaces, commas, periods, caps
# x$requested.by %>%
#   str_replace_all(., '\\  ', '') %>% 
#   str_replace_all(., '\\,', '') %>% 
#   str_replace_all(., '\\.', '') %>% 
#   str_to_lower(.)

# remove double spaces, commas, periods, caps
x.requestor <- x %>%
  mutate(requested.by = str_replace_all(requested.by, '\\  ', '')) %>% 
  mutate(requested.by = str_replace_all(requested.by, '\\,', '')) %>% 
  mutate(requested.by = str_replace_all(requested.by, '\\.', '')) %>% 
  mutate(requested.by = str_to_lower(requested.by)) %>% 
  select(request, requested.by)

x.requestor         

which_na(x.requestor$requested.by)
x[3480:3488,]
# replace NA and number values with next ones
x$requested.by[3484] <- x$requested.by[3485]
x$requested.by[1]    <- x$requested.by[2]
which(x.requestor$requested.by=="0000")
x$requested.by[540] <- x$requested.by[541]

if(!require(stringdist)) install.packages("stringdist")
library(stringdist)

# create column based on soundex
x.requestor <- x.requestor %>% 
  mutate(sound = soundex(requested.by))

x.requestor
length(unique(x.requestor$requested.by)) # 204 unique names
length(unique(x.requestor$sound))        # 134 unique sounds
unique(x.requestor$sound)

x.requestor %>% 
  filter(sound == "S556") %>%
  group_by(requested.by) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))



for (i in 1:length(unique.names)){
  # find most popular name of same sounding names
  replacement.name <- x.requestor %>% 
    filter(sound == unique.names[[i]]) %>%
    group_by(requested.by) %>% 
    summarise(count=n()) %>% 
    arrange(desc(count))
  replacement.name <- replacement.name[[1]][1]
  # if unique.name == requestor$sound, replace with replacement.name
  x.requestor$requested.by[x.requestor$sound == unique.names[[i]]] <- 
    replacement.name
}

######################






xx <- x$requested.by[c(840:865,430:440,110:115,120:126,15:18)]
stringdist(x$requested.by)


stringdistmatrix(xx)



