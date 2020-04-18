# Setting the working directory
setwd("~/Documents/COVID-19-India/before-lockdown/")

#####  LIBRARIES  #####
library(stringr)

############################ Loading data

# daily states' data
Confirmed = read.csv("ready_to_use/India_States_daily_Confirmed.csv")
Deaths = read.csv("ready_to_use/India_States_daily_Deaths.csv")
Recovered = read.csv("ready_to_use/India_States_daily_Recovered.csv")

# daily india' data ---> one row
india.daily.conf = read.csv("ready_to_use/India_Aggregate_daily_Confirmed.csv")
india.daily.dead = read.csv("ready_to_use/India_Aggregate_daily_Deaths.csv")
india.daily.cure = read.csv("ready_to_use/India_Aggregate_daily_Recovered.csv")

# Aggrigate data ---> till-date
india.summary.statewise = read.csv("ready_to_use/India_dataset_stateWise_summary.csv")
india.summary.overall = read.csv("ready_to_use/India_Aggregate_summary.csv")
india.daily.rise = read.csv("ready_to_use/India_Aggregate_dateWise_summary.csv")

# bulk --> every-day, every-state
india.bulk.summary.datewise = read.csv("ready_to_use/India_dataset_dateWise_summary.csv")
india.bulk.summary.statewise = read.csv("ready_to_use/India_dataset_stateWise_summary.csv")


# Analysed
Analysed = read.csv("analysis/estimatedCases.csv")

######### Viewing


#View(Confirmed)
#View(Deaths)
#View(Recovered)

#View(india.daily.conf)
#View(india.daily.dead)
#View(india.daily.cure)

#View(india.summary.statewise)
#View(india.summary.overall)
#View(india.daily.rise)

#View(india.bulk.summary.datewise)
#View(india.bulk.summary.statewise)


#View(Analysed)







