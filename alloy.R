unique(x$alloy)
length(unique(x$alloy)) # 60

# remove double spaces, commas, periods, caps
# filter for unique sounds

xx <- x %>%
  mutate(alloy = str_replace_all(alloy, '\\  ', '')) %>% 
  mutate(alloy = str_replace_all(alloy, '\\,', '')) %>% 
  mutate(alloy = str_replace_all(alloy, '\\.', '')) %>% 
  mutate(alloy = str_to_lower(   alloy))
  # select(request, alloy)

xx$alloy
unique(xx$alloy)
length(unique(xx$alloy)) # 47

# search aluminums
y <- dplyr::filter(xx, grepl('al', alloy))
unique(y$alloy)
# convert aluminums
y <- xx %>% 
  mutate(alloy.new = alloy) %>%
  mutate(alloy.new = ifelse(grepl('al',alloy), "aluminum", alloy.new))
# confirm
unique(y$alloy.new)
length(unique(y$alloy.new)) # 41

# search
y <- dplyr::filter(xx, grepl('ductile', alloy))
unique(y$alloy)
# convert 
y <- xx %>% 
  mutate(alloy.new = alloy) %>%
  mutate(alloy.new = ifelse(grepl('al',alloy), "aluminum", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('ductile',alloy), "ductile iron", alloy.new))
# confirm
unique(y$alloy.new)
length(unique(y$alloy.new)) # 36

# search
y <- dplyr::filter(xx, grepl('gray', alloy))
unique(y$alloy)
# convert 
y <- xx %>% 
  mutate(alloy.new = alloy) %>%
  mutate(alloy.new = ifelse(grepl('al',alloy), "aluminum", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('ductile',alloy), "ductile iron", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('di',alloy), "ductile iron", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('gray',alloy), "grey iron", alloy.new))
# confirm
unique(y$alloy.new)
length(unique(y$alloy.new)) # 29

# search
y <- dplyr::filter(xx, grepl('cg', alloy))
unique(y$alloy)
# convert 
y <- xx %>% 
  mutate(alloy.new = alloy) %>%
  mutate(alloy.new = ifelse(grepl('al',alloy), "aluminum", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('ductile',alloy), "ductile iron", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('di',alloy), "ductile iron", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('gray',alloy), "grey iron", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('cg',alloy), "cgi", alloy.new))
# confirm
unique(y$alloy.new)
length(unique(y$alloy.new)) # 28

# search
y <- dplyr::filter(xx, grepl("ss", alloy))
unique(y$alloy)
# convert 
y <- xx %>% 
  mutate(alloy.new = alloy) %>%
  mutate(alloy.new = str_replace_all(alloy.new, "[:punct:]","none")) %>% 
  mutate(alloy.new = ifelse(grepl('al',alloy), "aluminum", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('di',alloy),      "ductile iron", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('ductile',alloy), "ductile iron", alloy.new)) %>%
  mutate(alloy.new = ifelse(grepl('le iron',alloy), "ductile iron", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('gray',alloy),   "grey iron", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('y iron',alloy), "grey iron", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('cg',alloy), "cgi", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('brass',alloy), "bras", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('s steel',alloy), "stainless", alloy.new)) %>%
  mutate(alloy.new = ifelse(grepl('44',alloy),      "stainless", alloy.new)) %>%
  mutate(alloy.new = ifelse(grepl('ss',alloy),      "stainless", alloy.new)) %>%
  mutate(alloy.new = ifelse(grepl('teel',alloy), "lc steel", alloy.new)) %>% 
  mutate(alloy.new = ifelse(grepl('bras',alloy), "brass", alloy.new)) %>% 
  mutate(alloy.new = ifelse(alloy.new == "0" | 
                              alloy.new == "none" | 
                              alloy.new == "unknown", NA, alloy.new))
  

# confirm
unique(y$alloy.new)
length(unique(y$alloy.new)) # 11

# yy <- dplyr::filter(y, grepl("ss", alloy.new))
# unique(yy$alloy.new)

yy <- y %>% 
  filter(is.na(alloy.new))

yy <- y[2915:2920,]



