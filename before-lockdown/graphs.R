# Setting the working directory
setwd("~/Documents/COVID-19-India/india-today")

# visualization on map
library(dplyr)
library(rlist)
library(ggplot2)



## TO PLOT COUNTRIES
# generating List of countries
confTemp <- read.csv("cleaned/time_series_19-covid-Confirmed.csv")
deadTemp <- read.csv("cleaned/time_series_19-covid-Deaths.csv")
rcvrTemp <- read.csv("cleaned/time_series_19-covid-Recovered.csv")

conf <- confTemp[c(33,nrow(confTemp):34, 32:1),]
dead <- deadTemp[c(33,nrow(deadTemp):34, 32:1),]
rcvr <- rcvrTemp[c(33,nrow(rcvrTemp):34, 32:1),]
# View(conf)
# View(dead)
# View(rcvr)


#####################################################################
#                             Function                              #
#####################################################################

getBreaks <- function(highestNum) {
  
  breaks = NULL
  
  if(highestNum < 6) {
    breaks = c(0:highestNum)
  }
  else {
    init = 0
    noOfTerms = 6  # to generate 6+1 break points where last one is 'highestNum'
    
    gap = floor(highestNum/(noOfTerms))
    endPoint = gap*noOfTerms
    
    breaks = seq(init, endPoint, gap)
    if(max(breaks) < highestNum)
      breaks = c(breaks, highestNum)
  }
  
  return(breaks) 
}


# MAIN : for visualization
visualize_on_barPlot <- function(df, Path, index = 2) {
  get(df) -> Affected
  
  
  ### for testing purpose ###
  #Affected = conf
  #Path = "../Plots/graphs"
  
  ##########################################
  
  # storing column names
  columns <- colnames(Affected)
  
  day = 1:length(columns)
  d = as.Date("28/01/2020", format(c("%d/%m/%Y")))
      ### 29==> (28+1) is waste as 'State' in colnames
  date = as.character((day + d), format(c("%d/%m/%Y")))
  
  
  i = index # passed index
  for (col in i:length(columns)) {
    
    vector = getBreaks(max(Affected[,col]))
    
    ggplot(data = Affected, aes(x = Affected[,1], y = Affected[,col])) +
      theme_classic()  +
      geom_bar(aes(fill = State), stat = "identity") +
      xlab("State") + ylab("Cases' Count") +
      scale_y_continuous(breaks = vector) +
      labs(title = "COVID-19 in India",
           subtitle = date[col]) +
      
      theme(text = element_text(family = "Gill Sans", color = "#fd0054")
            ,panel.background = element_rect(fill = "#fbf9fa")
            ,plot.background = element_rect(fill = "#fbf9fa")
            ,plot.title = element_text(size = 15, face = "bold", color = "#2b2024", hjust = 0.4)
            ,plot.subtitle = element_text(size = 12, family = "Courier", face = "bold", hjust = 0.4)
            ,axis.text = element_text(size = 8, face = "bold", margin(t = 1, r = 1, b = 1, l = 1, unit = "pt"))
            ,axis.title = element_text(size = 12, face = "bold", color = "#a80038")
            ,axis.ticks = element_blank()
            ,legend.position = "none"
      ) +
      coord_flip()
    
    pic = paste("pic",col, sep = "")
    imageName = paste(pic,"png", sep = ".")
    
    ggsave(path = Path, filename = imageName, width = 11, height = 9, scale = 1)
  }
}

##################################################

#  index = 2
visualize_on_barPlot("dead", "../Plots/graphs/dead", index = 43)  # first death 13th March (44.png)
visualize_on_barPlot("conf", "../Plots/graphs/confirmed", index = 2)        # first case 30th Jan (2.png)
visualize_on_barPlot("rcvr", "../Plots/graphs/recovered", index = 34)        # first recovery 3rd March (35.png)







