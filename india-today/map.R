library(dplyr) # left join
library(rlist)
library(ggplot2)
library(maps)
library(stringr)
##set directory to the folder where the shapefile is, then input shapefile
setwd("~/Documents/COVID-19-India/india-today/")

Affected <- read.csv("cleaned/ever.Affected.csv")
States = as.factor(c("Andaman", "Nicobar", levels(Affected$State)[-1]))

to_append <- data.frame(
  rank = 1:length(States),
  states = States
)

############################################################################################


# getting world map
map.world <- map_data("world")
india = map.world[which(str_detect(map.world$region, "India")), -5]
india$order = c(1:nrow(india))
india
colnames(india) <- colnames(colnames(map.world)[-6])
map.world_joined <- left_join(india, to_append, by = c('subregion' = 'states'))
map.world_joined <- map.world_joined %>% mutate(fill_flg = ifelse(is.na(rank),F,T))

affected.Today = Affected[, c(1:3, 42)]

ggplot() +
  geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = fill_flg), color = "#252525") +
  geom_point(data = affected.Today, aes(x = Long, y = Lat), color = "white") +
  scale_fill_manual(values = c("#414141","#af0404")) +
  labs(title = "2019-nCoV",
       subtitle = date[i],
       caption = "by @ravi")



usa <- map_data("usa") # we already did this, but we can do it again
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

states <- map_data("state")
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # do this to leave off the color legend









