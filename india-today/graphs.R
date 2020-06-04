# Setting the working directory
setwd("~/Documents/COVID-19-India/india-today")

# visualization on map
library(dplyr)
library(rlist)
library(ggplot2)



## TO PLOT COUNTRIES
# generating List of countries
confTemp <- read.csv("cleaned/time_series_19-covid-Confirmed.csv")

conf <- confTemp[c(33,nrow(confTemp):34, 32:1),]
# View(conf)



###  Steps
# 1. plot bar graph for col1 of cases --->  col2 of table
ggplot(data = conf, aes(x = conf[,1], y = conf[,2])) +
  geom_bar(aes(fill = State), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Cases' Count") + ylab("State") +
  theme_classic()  +
  coord_flip()

# 2. make a fun to plot for col no. n
# 3. Plot till date

#####################################################################
#                             Function                              #
#####################################################################

#visualize_on_barPlot(conf, "PLOTS/maps/pngs/ever", index = 1)

# MAIN : for visualization
visualize_on_barPlot <- function(df, Path, index = 2) {
  get(df) -> Affected
  
  
  ### for testing purpose ###
  #Affected = conf
  
  ##########################################
  
  # storing column names
  columns <- colnames(Affected)
  
  day = 1:length(columns)
  d = as.Date("29/01/2020", format(c("%d/%m/%Y")))
  date = as.character((day + d), format(c("%d/%m/%Y")))
  
  
  i = index # passed index
  for (col in i:length(columns)) {
    ggplot(data = conf, aes(x = conf[,1], y = conf[,col])) +
      geom_bar(aes(fill = State), stat = "identity") +
      theme(legend.position = "none") +
      xlab("Cases' Count") + ylab("State") +
      theme_classic()  +
      coord_flip()
    
    
    pic = paste("pic",i+1, sep = "")
    imageName = paste(pic,"png", sep = ".")
    
    ggsave(path = Path, filename = imageName, width = 16, height = 9, scale = 1)
  }
  
  # 1. plot bar graph for col1 of cases --->  col2 of table
  
  
  
  
  
  
  # head(myList)
  # generating plots, datewise
  i = index
  while(T) {
    if(i>length(myList)){ # length(myList)
      print("Stoppppppp!!!!!!!!!!!!!!!!")
      break
    }
    
    affected.Today = as.data.frame(myList[i])
    row.names(affected.Today) <- NULL
    
    
    # renaming the countries as per their standered name
    Countries <- as.character(affected.Today$Country.Region)
    Countries.levels <- as.character(levels(affected.Today$Country.Region))
    Countries[Countries %in% "United States"] = "USA"
    Countries[Countries %in% "United Kingdom"] = "UK"
    Countries[Countries %in% "North Macedonia"] = "Macedonia"
    Countries.levels[Countries.levels %in% "United States"] = "USA"
    Countries.levels[Countries.levels %in% "United Kingdom"] = "UK"
    Countries.levels[Countries.levels %in% "North Macedonia"] = "Macedonia"
    
    
    if(length(Countries[Countries %in% "Saint Vincent and the Grenadines"]) != 0){
      Countries[Countries %in% "Saint Vincent and the Grenadines"] = "Saint Vincent"
      Countries.levels[Countries.levels %in% "Saint Vincent and the Grenadines"] = "Saint Vincent"
      
      Countries = c(Countries, "Grenadines")
      Countries.levels = c(Countries.levels, "Grenadines")
    }
    if(length(Countries[Countries %in% "Antigua and Barbuda"]) != 0){
      Countries[Countries %in% "Antigua and Barbuda"] = "Antigua"
      Countries.levels[Countries.levels %in% "Antigua and Barbuda"] = "Antigua"
      
      Countries = c(Countries, "Barbuda")
      Countries.levels = c(Countries.levels, "Barbuda")
    }
    if(length(Countries[Countries %in% "Trinidad and Tobago"]) != 0){
      Countries[Countries %in% "Trinidad and Tobago"] = "Trinidad"
      Countries.levels[Countries.levels %in% "Trinidad and Tobago"] = "Trinidad"
      
      Countries = c(Countries, "Tobago")
      Countries.levels = c(Countries.levels, "Tobago")
    }
    
    Countries = factor(c(Countries), levels = c(Countries.levels))
    #################################
    
    
    to_append <- data.frame(
      rank = 1:length(Countries),
      country = Countries
    )
    
    ############################################################################################
    
    
    # getting world map
    map.world <- map_data("world")
    
    
    map.world_joined <- left_join(map.world, to_append, by = c('region' = 'country'))
    map.world_joined <- map.world_joined %>% mutate(fill_flg = ifelse(is.na(rank),F,T))
    
    
    ##################################################
    ## list of countries that can't be plotted
    listIsolated <- function() {
      log = NULL
      for (r in to_append$rank) {
        
        x = map.world[which(map.world$rank == r),"fill_flg"] & T
        l = T
        for (t in 1:length(x)) {
          if(t==1) {
            l = x[t]
          } else {
            l = l & x[t] 
          }
        }
        
        if(length(l) == 0)
          log = c(log, r)
      }
      
      return(to_append[which(to_append$rank %in% log),'country'])
    }
    temp = as.character(listIsolated())
    #print(temp)
    
    # go above and manually add them to plot list
    ##################################################
    
    # countries/locations affected by coronavirus
    ggplot() +
      geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = fill_flg), color = "#252525") +
      geom_point(data = affected.Today, aes(x = Long, y = Lat), color = "white") +
      scale_fill_manual(values = c("#414141","#af0404")) +
      labs(title = "2019-nCoV",
           subtitle = date[i],
           caption = "by @ravi") +
      
      theme(text = element_text(family = "Gill Sans", color = "#FFFFFF")
            ,panel.background = element_rect(fill = "#252525")
            ,plot.background = element_rect(fill = "#252525")
            ,panel.grid = element_blank()
            ,plot.title = element_text(size = 20, face = "bold", color = "#ff0000", hjust = 0.5)
            ,plot.subtitle = element_text(size = 25, family = "Courier", face = "bold", hjust = 0.5)
            ,plot.caption = element_text(size = 15, family = "Bookman", face = "italic", hjust = 0.9)
            ,axis.text = element_blank()
            ,axis.title = element_blank()
            ,axis.ticks = element_blank()
            ,legend.position = "none"
      )
    
    ##########################################################################################
    
    
    pic = paste("pic",i+1, sep = "")
    imageName = paste(pic,"png", sep = ".")
    
    ggsave(path = Path, filename = imageName, width = 16, height = 9, scale = 1)
    i = i+1
  }
}

#####################################################################
# index 90   --->   20th April: 91.png

## Generates map plots from the given index (day number) till today.
newDay = 100
visualize_on_map("ever.Affected", "PLOTS/maps/pngs/ever", index = newDay)    # pass index also to plot map(s) from index-th day
visualize_on_map("highly.Affected", "PLOTS/maps/pngs/highly", index = newDay)  # by default index is 1














