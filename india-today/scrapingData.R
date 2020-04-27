# Setting the working directory
setwd("~/Documents/COVID-19-India/india-today/")

# General-purpose data wrangling
library(httr)
library(XML)
library(stringr)

#######################################################
#######################################################

# reading from url
url <-'https://www.mohfw.gov.in/'
rawHTML = GET(url)

# extracting & formatting html
content = content(rawHTML, as='text')
parsedHTML = htmlParse(content, asText=T)


######################################################


# working wuth required table in html

tableHeader = xpathSApply(parsedHTML, '//thead//strong', xmlValue)  # header

rows = xpathSApply(parsedHTML, '//tbody/tr', xmlValue)

#########     METHOD 1      ##########
# 
# for (row in rows) {
#   # we'll use regex here
#   xpathSApply(row, '/td', xmlValue)
# }



#########     METHOD 2      ##########
# fetch all datavalues seperatly
values = xpathSApply(parsedHTML, '//tbody/tr/td', xmlValue)
#values

limit = which(str_detect(rows, "Total number of confirmed cases in India"))

sno = NULL
states = NULL
conf = NULL
cured = NULL
dead = NULL

#fetching seperate columns' data
for(row in seq(0, c(limit-2))) {
  rowIndex = row*5
  
  sno = c(sno, as.integer(values[rowIndex + 1]))
  states = c(states, as.character(values[rowIndex + 2]))
  conf = c(conf, as.integer(values[rowIndex + 3]))
  cured = c(cured, as.integer(values[rowIndex + 4]))
  dead = c(dead, as.integer(values[rowIndex + 5]))

}

# joining to the 
todayData = cbind(sno, states, conf, cured, dead)
colnames(todayData) <- tableHeader
# View(todayData)


###############################################

# Saving this dataset
write.csv(todayData, file = "todayData.csv", row.names=F)


###################################################################################

casesToday = read.csv("todayData.csv")
casesToday = casesToday[,c(1,2,4,5,3)]
# View(casesToday)
# str(casesToday)

casesTillYesterday = read.csv("covid_19_india.csv")

##################

#  list of all states
temp = levels(casesTillYesterday$State.UnionTerritory)
allStates = temp[!temp %in% "Unassigned"]
statesToday = levels(casesToday$Name.of.State...UT)

casesYesterday = tail(casesTillYesterday, length(allStates))

##################

# finding missing state in today's data ---> like Nagaland
missing.state = allStates[!allStates %in% statesToday]

# fetching old data of missing state
oldData = casesYesterday[
                         which(str_detect(casesYesterday$State.UnionTerritory, missing.state)),
                         c('Sno', 'State.UnionTerritory', 'Cured', 'Deaths', 'Confirmed')
                        ]

# joining it to todays' data
colnames(oldData) <- colnames(casesToday)
casesToday = rbind(casesToday, oldData)
# View(casesToday)

# Arranging in order of states' name
casesToday = casesToday[order(as.character(casesToday$Name.of.State...UT)),]
casesToday$S..No. = c(1:nrow(casesToday))


##################
# Other info
date = format(Sys.Date()-1, "%d/%m/%y")
time = "10:00 AM"
indians = "-"
foreigners = "-"

casesToday = cbind(
                    casesToday$S..No.,
                    rep(date, times=nrow(casesToday)),
                    rep(time, times=nrow(casesToday)),
                    casesToday$Name.of.State...UT,
                    rep(indians, times=nrow(casesToday)),
                    rep(foreigners, times=nrow(casesToday)),
                    casesToday[,3:5]
                    )

colnames(casesToday) = colnames(casesYesterday)
# View(casesToday)


################################################


# Backing up the previous data
write.csv(casesTillYesterday, file="backup/covid_19_india.csv", row.names = FALSE)

# Updating the actual
lastRowNo = nrow(casesTillYesterday)
casesToday$Sno = casesToday$Sno+lastRowNo

casesTillNow = rbind(casesTillYesterday, casesToday)
View(casesTillNow)
View(casesTillYesterday)


################################################

## Writing the updated data
write.csv(casesTillNow, file="covid_19_india.csv", row.names = FALSE)





















