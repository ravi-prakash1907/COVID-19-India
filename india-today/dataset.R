# Setting the working directory
setwd("~/Documents/COVID-19-India/india-today/")


#####  LIBRARIES  #####
library(stringr)


########################################
#data <- read.csv("cleaned/statewise_ready.csv")

Confirmed <- read.csv("cleaned/time_series_19-covid-Confirmed.csv")
Deaths <- read.csv("cleaned/time_series_19-covid-Deaths.csv")
Recovered <- read.csv("cleaned/time_series_19-covid-Recovered.csv")

#View(Confirmed)  
#View(Deaths)               
#View(Recovered)            


#######################################

getPercent <- function(whole, part){
  if(whole == 0) {
    cent = 0.00
  } else {
    cent = (part*100)/whole
    cent = round(cent, 2)  # rounds off the value to hold upto 2 decimal places
  }
  return(cent)
}

# sort
sort.statewise <- function(dfName){
  df <- get(dfName)
  df = df[order(df$State),]
  
  return(df)
}
sort.datewise <- function(dfName){
  df = get(dfName)
  df = df[order(df$Day),]
  
  return(df)
}

country.timeseries  <-  function(dfName) {
  
  temp = get(dfName)            # all states' data of the country
  df = temp[1,]
  
  df[2:ncol(temp)] = apply(   temp[,2:ncol(temp)],
                              2,
                              sum
                           )                              
  
  names <- c("Country", colnames(df[2:ncol(df)]))
  colnames(df) <- names
  df$Country = "India"
  
  row.names(df) <- NULL
  
  return(df)
}

# summarizer
summary.states = function() {
  
  #########################################
  C <- get("Confirmed")
  D <- get("Deaths")
  R <- get("Recovered")
  
  
  c1 = C[,ncol(C)]
  c2 = D[,ncol(D)]
  
  rate.of.death = cbind(c1, c2)
  result = data.frame(cent = NULL)
  
  for (j in 1:nrow(rate.of.death)) { # i=1
    whole = as.integer(rate.of.death[j,1])
    part = as.integer(rate.of.death[j,2])
    res = getPercent(whole, part)
    
    temp = data.frame(cent = res)
    result = rbind(result, temp)
  }
  #########################################
  
  
  allStates <- as.character(C$State)   # list of states in the country
  
  ###### overall data of the country (all states) ######
  df = data.frame(
    States = allStates,
    Confirmed = C[,ncol(C)],
    Deaths = D[,ncol(D)],
    Recovered = R[,ncol(R)],
    "Active Cases" = C[,ncol(C)] - (D[,ncol(D)] + R[,ncol(R)]),
    "Closed Cases" = D[,ncol(D)] + R[,ncol(R)],
    "Death Rate" = result$cent
  )
  
  return(df)
}
india.bulk.summary = function() { # date wise country data
  
  # structure of resulting dataset (initially blank)
  df <- data.frame(
    Country = NULL,
    Day = NULL,           # day no.
    Date = NULL,
    Confirmed = NULL,
    Deaths = NULL,
    Recovered = NULL,
    "Active Cases" = NULL,
    "Closed Cases" = NULL,
    "Death Rate" = NULL
  )
  
  
  for (state in levels(Confirmed$State)) {
    C = Confirmed[which(str_detect(Confirmed$State, state)),]
    D = Deaths[which(str_detect(Deaths$State, state)),]
    R = Recovered[which(str_detect(Recovered$State, state)),]
    
    
    times = ncol(C)-1      # no. of days
    day = 1:times
    d = as.Date("29/01/2020", format(c("%d/%m/%Y")))
    
    date = as.character((day + d), format(c("%d/%m/%Y")))
    date = factor(c(date), levels = date)
    
    #########
    conf = as.integer(C[,2:ncol(C)])
    dead = as.integer(D[,2:ncol(D)])
    recovered = as.integer(R[,2:ncol(R)])
    
    rate.of.death = cbind(conf, dead)
    result = data.frame(cent = NULL)
    
    for (j in 1:nrow(rate.of.death)) { # i=1
      whole = as.integer(rate.of.death[j,1])
      part = as.integer(rate.of.death[j,2])
      res = getPercent(whole, part)
      
      temp = data.frame(cent = res)
      result = rbind(result, temp)
    }
    #########
    
    dataset <- data.frame(
      State = rep(C[1,"State"], times),
      Date = date,
      Day = c(1:times),
      "Confirmed" = conf,
      "Deaths" = dead,
      "Recovered" = recovered,
      "Active Cases" = conf - (dead + recovered),
      "Closed Cases" = dead + recovered,
      "Death Rate" = result$cent
    )
    df = rbind(df, dataset)
  }
  
  return(df)
}

dailyGrowth <- function(){
  
  states <- india.bulk.summary()
  
  dName = levels(as.factor(states$Day))
  df = states[which(states$Day == dName[!dName %in% dName]),]
  dayCount = nlevels(as.factor(states$Day))
  
  for(day in 1:dayCount){
    temp1 = states[which(states$Day == dName[day]),]
    aggr = apply(temp1[,4:ncol(temp1)], 2, sum)
    
    temp2 = temp1[1,]
    temp2[,4:c(ncol(temp2)-1)] = aggr[-length(aggr)]
    temp2[,ncol(temp2)] = getPercent(aggr[1], aggr[2])
    
    
    df = rbind(df, temp2)
  }
  
  name = colnames(df)
  colnames(df) <- c("Country", name[2:length(name)])
  df$Country = rep("India", nrow(df))
  
  ###############################################
  
  #### within how many days, cases double
  doublesIN = NULL
  for(today in df$Day){
    # today = 3
    if((today == 1) || (df[which(df$Day == today), 'Confirmed'] == 1)){
      dIN = 0
    } else {
      half = (df[which(df$Day == today),'Confirmed']/2)
      
      halfDay = df[which(df$confConfirmed == half), 'Day']
      if(length(halfDay) != 0){
        dIN = today-halfDay
      } else {
        lowerBound = max(df[which(df$Confirmed <= half), 'Day'])
        upperBound = min(df[which(df$Confirmed >= half), 'Day'])
        
        if(length(lowerBound) == 0){
          dIN = upperBound
        } else {
          subL = c(half - df[which(df$Day == lowerBound), 'Confirmed'])
          subU = c(df[which(df$Day == upperBound), 'Confirmed']- half)
          
          dIN = today - ifelse((subL<subU), lowerBound, upperBound)
        }
      }
    }
    
    if(length(dIN) > 1)
      dIN = max(dIN)
    
    doublesIN = c(doublesIN, dIN)
  }
  
  ### adding col 
  df$"Doubles In" = doublesIN
  
  
  ######## new cases
  day = df$Day
  cases = df$Confirmed
  
  newCases = NULL
  for(i in day){
    if(i==1)
      newCases = c(newCases, cases[i])
    else{
      val = cases[i]-cases[i-1]
      newCases = c(newCases, val)
    }
  }
  
  ### adding new
  df$"New Cases" = newCases
  ###############################################
  df = df[,c(1:4, 11, 10, 5:9)]
  
  return(df)
}

########################################

india.daily.conf = country.timeseries("Confirmed")
india.daily.dead = country.timeseries("Deaths")
india.daily.cure = country.timeseries("Recovered")
#View(india.daily.conf)
#View(india.daily.dead)
#View(india.daily.cure)

india.summary.statewise = summary.states()
#View(india.summary.statewise)

india.bulk.summary.statewise = india.bulk.summary()
india.bulk.summary.datewise = sort.datewise("india.bulk.summary.statewise")
#View(india.bulk.summary.statewise)
#View(india.bulk.summary.datewise)

india.daily.rise = dailyGrowth()
#View(india.daily.rise)

# india.aggr.summary
india.summary.overall = india.daily.rise[nrow(india.daily.rise),-c(2:3)]
#View(india.summary.overall)



#######################  INDIA  #######################

# daily states' data
write.csv(Confirmed, file = "ready_to_use/India_States_daily_Confirmed.csv", row.names = FALSE)
write.csv(Deaths, file = "ready_to_use/India_States_daily_Deaths.csv", row.names = FALSE)
write.csv(Recovered, file = "ready_to_use/India_States_daily_Recovered.csv", row.names = FALSE)

# daily india' data ---> one row
write.csv(india.daily.conf, file = "ready_to_use/India_Aggregate_daily_Confirmed.csv", row.names = FALSE)
write.csv(india.daily.dead, file = "ready_to_use/India_Aggregate_daily_Deaths.csv", row.names = FALSE)
write.csv(india.daily.cure, file = "ready_to_use/India_Aggregate_daily_Recovered.csv", row.names = FALSE)

# Aggrigate data ---> till-date
write.csv(india.summary.statewise, file = "ready_to_use/India_dataset_stateWise_summary.csv", row.names = FALSE)
write.csv(india.summary.overall, file = "ready_to_use/India_Aggregate_summary.csv", row.names = FALSE)
write.csv(india.daily.rise, file = "ready_to_use/India_Aggregate_dateWise_summary.csv", row.names = FALSE)

# bulk --> every-day, every-state
write.csv(india.bulk.summary.datewise, file = "ready_to_use/India_dataset_dateWise_summary.csv", row.names = FALSE)
write.csv(india.bulk.summary.statewise, file = "ready_to_use/India_dataset_stateWise_summary.csv", row.names = FALSE)












