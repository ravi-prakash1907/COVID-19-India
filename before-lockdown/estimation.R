# Setting the working directory
setwd("~/Documents/COVID-19-India/before-lockdown/")


#####  LIBRARIES  #####
library(stringr)


### datasets
india.summary.overall = read.csv("ready_to_use/India_Aggregate_summary.csv")
india.summary.statewise = read.csv("ready_to_use/India_dataset_stateWise_summary.csv")
india.daily.rise = read.csv("ready_to_use/India_Aggregate_dateWise_summary.csv")

india.bulk.summary.statewise = read.csv("ready_to_use/India_dataset_stateWise_summary.csv")
#####################################################

##### Functions 
appxCases <- function(cent, death){
  count = (deaths*100)/cent
  expactedCount = round(count)
  
  return(expactedCount)
}

magnifire <- function(cases){
  cumulativeCases = cases*2
  
  return(cumulativeCases)
}

reducer <- function(cases){
  cumulativeCases = cases/2
  
  return(cumulativeCases)
}

#####################################################

## incubatio period -> in average case ---> 2-7 days
incubation.Period = 5.2   # avg in world

# Time from symptom onset to death
incub.toDeath = 15

## total time taken from incubation to death --->  10+15 = 25

## on the day, when a case was reported:
# 1) that person was affected some 10 days ago
# 2) along with that one, n more people were affected

## on the day, when a person expired:
# 1) he was affected some 25 days age
# 2) he was hospitalized some 15 days ago
# 3) as per the death persent (d%) till that day,  total probable affected(not even reported too) be : 
# a) (total died that day)/(d%)

## since those 25 days ago probable case:
# 1) Daily cases will be related to those probable cases(25 days ago)
# 2) Double the cases as per the doubling rate bases on that of today

#####################################################

#View(india.daily.rise)

# upto 24th march  --->  last day before lockdown
dRate.India = india.daily.rise$Death.Rate[!india.daily.rise$Death.Rate %in% 0.00] # death rate of days when there was a casualiti in india
dRate.States = india.summary.statewise$Death.Rate[!india.summary.statewise$Death.Rate %in% 0.00] # dRate of states where there was a casuality, till 24th

dRate = sum(mean(dRate.India), mean(dRate.States))/2 # overall deathrate of india till 24th

doublingRate = as.integer(sum(mean(india.daily.rise$Doubles.In), india.summary.overall$Doubles.In)/2)  # average rate of doubling of the cases

#####################################################

dayOfDeath = india.daily.rise[which(str_detect(india.daily.rise$Date, "24/03/2020")),"Day"]

# expacted day of infetion day
dayOfInfection = round(dayOfDeath - (incubation.Period + incub.toDeath))
# estimation since that person was affected
daysSinceInf = c(dayOfInfection:(dayOfDeath+(doublingRate*3)))
conf = c(india.daily.rise$Confirmed[dayOfInfection:nrow(india.daily.rise)], rep(NA, (doublingRate*3)))

daysAnalysed = data.frame(
  Day = daysSinceInf,
  Atual.Cases = conf,
  Expacted.Cases = rep(NA, length(daysSinceInf))
)

#####################################################


deaths = india.daily.rise$Deaths[nrow(india.daily.rise)]     # dec by 1
cases = india.daily.rise$Confirmed[nrow(india.daily.rise)]   # dec by 1


day = 1

repeat {
  if(day == 1)
  {
    daysAnalysed$Expacted.Cases[day] = appxCases(dRate, deaths)
    casesNow = daysAnalysed$Expacted.Cases[day]
  }
  else if(day > length(daysSinceInf))
  {
    break
  }
  else if(day > 1 && (day-doublingRate) < 1)
  {
    
    daysAnalysed$Expacted.Cases[day] = appxCases(dRate, deaths)
    casesNow = daysAnalysed$Expacted.Cases[day]
  }
  else if(day > 1)
  {
    daysAnalysed$Expacted.Cases[day] = magnifire(casesNow)
    casesNow = daysAnalysed$Expacted.Cases[day]
  }
  
  
  for(j in 1:doublingRate)
  {if(day+j <= length(daysSinceInf))
    daysAnalysed$Expacted.Cases[day+j] = casesNow + round(casesNow*j/doublingRate)
  }
  day = day+doublingRate
  
}

###################################################

t1 = cbind(
            Country = "India",
            india.daily.rise[1:(dayOfInfection-1),c('Date', 'Day', 'Confirmed')],
            Expacted = rep(NA, dayOfInfection-1)
          )


totalNAs = nrow(daysAnalysed)+(dayOfInfection-1)-nrow(india.daily.rise)
t2 = cbind( 
            Country = "India",
            Date = c(as.character(india.daily.rise$Date[dayOfInfection:dayOfDeath]), rep(NA, totalNAs)),
            Day = daysAnalysed[,1],
            daysAnalysed[,2:3]
          )


colnames(t1) <- colnames(t2)
daysAnalysed = rbind(t1, t2)


###################################################

# fill expacted places (above)
day = dayOfInfection
while(day >= 1){
  
  if(day == dayOfInfection)
  {
    casesNow = daysAnalysed$Expacted.Cases[day]
  }
  else
  {
    casesNow = round(reducer(casesNow))
    daysAnalysed$Expacted.Cases[day] = casesNow
    
    for(d in 1:doublingRate){
      extra = round(casesNow*d/doublingRate)
      daysAnalysed$Expacted.Cases[day+d] = casesNow+extra
    }
    
    if(day-doublingRate < 1)
    {
      min = round(reducer(casesNow))
      
      for (i in (day):1) {
        extra = round(min*i/doublingRate)
        daysAnalysed$Expacted.Cases[i] = min+extra
      }
      break
    }
  }
  
  day = day-doublingRate
  
}


###################################################


View(daysAnalysed)

# write to a .csv file

