## Updating Forked repo  --> COVID-19

# Setting the working directory
setwd("~/Documents/COVID-19-India/india-today/")

###################################

cleaned.Confirmed <- read.csv("cleaned/time_series_19-covid-Confirmed.csv")
cleaned.Deaths <- read.csv("cleaned/time_series_19-covid-Deaths.csv")
cleaned.Recovered <- read.csv("cleaned/time_series_19-covid-Recovered.csv")

#View(cleaned.Confirmed)
#View(cleaned.Deaths)
#View(cleaned.Recovered)

#############################################################
fetched.India.JHU.Conf = read.csv("../time_series_data/jhu_format/time_series_19-covid-jhu-Confirmed.csv")
#View(fetched.India.JHU.Conf)


#cleaned.Confirmed = cleaned.Confirmed[,-ncol(cleaned.Confirmed)]
#cleaned.Deaths = cleaned.Deaths[,-ncol(cleaned.Deaths)]
#cleaned.Recovered = cleaned.Recovered[,-ncol(cleaned.Recovered)]


############### Time series for JHU
posToAppend = cbind(States = posToAppend[,1], Country = c(rep("India", nrow(cleaned.Confirmed))), posToAppend[,2:3])

jhu.india.conf = cbind(posToAppend, cleaned.Confirmed[order(cleaned.Confirmed$State),2:ncol(cleaned.Confirmed)])
jhu.india.dead = cbind(posToAppend, cleaned.Deaths[order(cleaned.Deaths$State),2:ncol(cleaned.Deaths)])
jhu.india.rcvr = cbind(posToAppend, cleaned.Recovered[order(cleaned.Recovered$State),2:ncol(cleaned.Recovered)])

omittedDates = data.frame(  c1 = rep(0, nrow(jhu.india.conf))  )
for(cols in 2:8){
  omittedDates = cbind(omittedDates, omittedDates[,1])
}

jhu.india.conf = cbind(jhu.india.conf[,1:4], omittedDates, jhu.india.conf[,5:ncol(jhu.india.conf)])
jhu.india.dead = cbind(jhu.india.dead[,1:4], omittedDates, jhu.india.dead[,5:ncol(jhu.india.dead)])
jhu.india.rcvr = cbind(jhu.india.rcvr[,1:4], omittedDates, jhu.india.rcvr[,5:ncol(jhu.india.rcvr)])

################################
colnames(jhu.india.conf) <- colnames(fetched.India.JHU.Conf)
colnames(jhu.india.dead) <- colnames(fetched.India.JHU.Conf)
colnames(jhu.india.rcvr) <- colnames(fetched.India.JHU.Conf)

#jhu.india.conf = jhu.india.conf[,-ncol(jhu.india.conf)]
#jhu.india.dead = jhu.india.dead[,-ncol(jhu.india.dead)]
#jhu.india.rcvr = jhu.india.rcvr[,-ncol(jhu.india.rcvr)]


write.csv(jhu.india.conf, file = "../time_series_data/ready/time_series_19-covid-Confirmed-India.csv", row.names = FALSE)
write.csv(jhu.india.dead, file = "../time_series_data/ready/time_series_19-covid-Deaths-India.csv", row.names = FALSE)
write.csv(jhu.india.rcvr, file = "../time_series_data/ready/time_series_19-covid-Recovered-India.csv", row.names = FALSE)
#View(jhu.india.conf)
#View(jhu.india.dead)
#View(jhu.india.rcvr)


#########################################################################################################


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

#source('~/Documents/A-tracking-of-2019-nCoV/COVID-19/datasets.R', echo=TRUE)
#source('~/Documents/A-tracking-of-2019-nCoV/COVID-19/dataCleaner.R', echo=TRUE)
