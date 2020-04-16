# Setting the working directory
setwd("~/Documents/COVID-19-India/india-today/")

###################################

#####  LIBRARIES  #####
library(stringr)

#####   dataset   #####
data = read.csv("covid_19_india.csv")
position = read.csv("latlong.csv")

data = data[which(str_detect(data$State.UnionTerritory, "Unassigned", negate = T)),]
data$State.UnionTerritory = as.factor(as.character(data$State.UnionTerritory))
#View(data)
#View(position)
###################################


########     FUNCTIONS     ########

findDates <- function(dfName){
  df <- get(dfName)
  
  dates = levels(df$Date)
  newDates = sort(as.Date(dates, "%d/%m/%y")) # format has to be changed
  newDates = format(newDates, "%d/%m/%y")
  
  return(newDates)
}
filter <- function(dfName = "data"){
  df <- get(dfName)
  
  df = df[,-c(1, 3, 5, 6)]
  df$Day = NA
  df <- df[, c(2, 1, 6, 5, 3, 4)]
  
  ######################################
  uniqueDates = findDates(dfName)
  allDates = df$Date
  for (dayNo in seq_along(uniqueDates)) {
    df[which(str_detect(df$Date, uniqueDates[dayNo])),'Day'] = dayNo
  }
  ######################################
  
  ## REMOVED STATE (Pondicherry type)
  saparater = "\n\n\n===============================================\n\n"
  lastDay = max(df$Day)
  
  
  all = levels(df$State.UnionTerritory)
  atLast = df[which(df$Day == lastDay), "State.UnionTerritory"]
  cat(saparater, "Fix Manually :  ", all[!all %in% atLast], saparater)
  
  ######################################
  
  
  return(df)
}

#---------------------------------#

state.spread.daily <- function(dfName, sName) {
  df <- get(dfName)
  df = df[which(str_detect(df$State.UnionTerritory, sName)),]
  row.names(df) <- NULL
  
  return (df)
}

state.timeseries <- function(dfName, sName, colName){
  
  df <- state.spread.daily(dfName, sName)
  
  dates.india <- findDates(dfName)    # finds all dates
  dates.state = df$Date
  
  get(dfName) -> dfName
  col = which(str_detect(colnames(dfName)[seq_along(colnames(dfName))], colName))
  
  temp = data.frame(State = sName)
  
  for(day in dates.india){
    cases = ifelse((day %in% dates.state), df[which(str_detect(df$Date, day)), col], 0)
    temp = cbind(temp, as.data.frame(cases))
  }
  colnames(temp) <- c("State", dates.india)
  
  return(temp)
}
whole.country <- function(colName, dfName = "india.data"){
  get(dfName) -> df
  states = levels(df$State.UnionTerritory)
  
  temp = state.timeseries(dfName, states[1], colName)
  states <- states[-1]
  
  for(st in states){
    temp = rbind(temp, state.timeseries(dfName, st, colName))
  }
  
  return(temp)
}

###################################

sName = "Kerala"
india.data = filter("data")
#View(india.data)

states.summary = state.spread.daily("india.data", sName)
#View(states.summary)

# timeseries
state.conf = state.timeseries("india.data", sName, "Confirmed")
state.cure = state.timeseries("india.data", sName, "Cured")
state.dead = state.timeseries("india.data", sName, "Deaths")
#View(state.conf)
#View(state.cure)
#View(state.dead)

# whole country data
conf = whole.country("Confirmed")
cure = whole.country("Cured")
dead = whole.country("Deaths")
#View(conf)
#View(cure)
#View(dead)


###################################


#includes both Indians as well Forigners
## Closed cases (i.e. Recovered or Death cases)
cases.Closed = cbind(conf[,1],  (dead[,2:ncol(dead)] + cure[,2:ncol(cure)]))
## Active cases 
cases.Active = cbind(conf[,1],  (conf[,2:ncol(conf)] - cases.Closed[,2:ncol(cases.Closed)]))



### Unit scaling ###

ever.Affected = conf
still.Affected = cases.Active

for (i in row.names(ever.Affected)) {
  for (j in 2:ncol(ever.Affected)) {
    if(ever.Affected[i,j] != 0)
      ever.Affected[i,j] = 1
  }
}
for (i in row.names(still.Affected)) {
  for (j in 2:ncol(still.Affected)) {
    if(still.Affected[i,j] != 0)
      still.Affected[i,j] = 1
  }
}

colnames(still.Affected) <- colnames(ever.Affected)

ever.Affected = ever.Affected[order(ever.Affected$State),]
still.Affected = still.Affected[order(still.Affected$State),]

######################## extract lat long
allStates = levels(position$State)
affectedStates = levels(data$State.UnionTerritory)
posToAppend = position[which(length(position$State) == 0),]
for (x in affectedStates) {
  posToAppend = rbind(posToAppend, position[which(str_detect(position$State, x)),])
}
posToAppend = posToAppend[order(posToAppend$State),]


###################################
ever.Affected = cbind(posToAppend, ever.Affected[,c(2:ncol(ever.Affected))])
still.Affected = cbind(posToAppend, still.Affected[,c(2:ncol(still.Affected))])
#View(ever.Affected)
#View(still.Affected)
#############################################################

# writing to csv

## Main files
write.csv(india.data, file = "cleaned/statewise_ready.csv", row.names = FALSE)

write.csv(conf, file = "cleaned/time_series_19-covid-Confirmed.csv", row.names = FALSE)
write.csv(dead, file = "cleaned/time_series_19-covid-Deaths.csv", row.names = FALSE)
write.csv(cure, file = "cleaned/time_series_19-covid-Recovered.csv", row.names = FALSE)

###   For map plot & gif
write.csv(ever.Affected, file = "cleaned/ever.Affected.csv", row.names = FALSE)
write.csv(still.Affected, file = "cleaned/still.Affected.csv", row.names = FALSE)




########################################


ready <- read.csv("cleaned/statewise_ready.csv")

cleaned.Confirmed <- read.csv("cleaned/time_series_19-covid-Confirmed.csv")
cleaned.Deaths <- read.csv("cleaned/time_series_19-covid-Deaths.csv")
cleaned.Recovered <- read.csv("cleaned/time_series_19-covid-Recovered.csv")

cleaned.ever.Affected <- read.csv("cleaned/ever.Affected.csv")
cleaned.still.Affected <- read.csv("cleaned/still.Affected.csv")



#View(ready)

#View(cleaned.Confirmed)
#View(cleaned.Deaths)
#View(cleaned.Recovered)

#View(cleaned.ever.Affected)
#View(cleaned.still.Affected)


##########    ENDS     ###########


