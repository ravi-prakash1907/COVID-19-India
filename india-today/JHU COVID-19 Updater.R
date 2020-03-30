## Updating Forked repo  --> COVID-19


# Setting the working directory
setwd("~/Documents/COVID-19-India/india-today/")

###################################

#####  LIBRARIES  #####
library(stringr)

# world data

conf = read.csv("~/Documents/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
dead = read.csv("~/Documents/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
rcvr = read.csv("~/Documents/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

# india data
jhu.india.conf = read.csv("../time_series_data/ready/time_series_19-covid-Confirmed-India.csv")
jhu.india.dead = read.csv("../time_series_data/ready/time_series_19-covid-Deaths-India.csv")
jhu.india.rcvr = read.csv("../time_series_data/ready/time_series_19-covid-Recovered-India.csv")

####################################

# step-1: finding rownum. for India
cNUM = which(str_detect(conf$Country.Region, "India"))
dNUM = which(str_detect(dead$Country.Region, "India"))
rNUM = which(str_detect(rcvr$Country.Region, "India"))

# step-2: removing India from world
conf = conf[which(str_detect(conf$Country.Region, "India", negate = TRUE)),]
dead = dead[which(str_detect(dead$Country.Region, "India", negate = TRUE)),]
rcvr = rcvr[which(str_detect(rcvr$Country.Region, "India", negate = TRUE)),]

#View(conf)
#View(dead)
#View(rcvr)


# step-3: appending whole india
conf = rbind(conf[1:(cNUM-1),], jhu.india.conf, conf[(cNUM):nrow(conf),])
dead = rbind(dead[1:(cNUM-1),], jhu.india.dead, dead[(dNUM):nrow(dead),])
rcvr = rbind(rcvr[1:(cNUM-1),], jhu.india.rcvr, rcvr[(cNUM):nrow(rcvr),])


# step-4: updating the main files
write.csv(conf, file = "~/Documents/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", row.names = FALSE)
write.csv(dead, file = "~/Documents/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", row.names = FALSE)
write.csv(rcvr, file = "~/Documents/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", row.names = FALSE)

# done!!
