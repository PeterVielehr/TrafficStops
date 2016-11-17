##################################################################################
##################################################################################
##The following script produces results from Gideon's Army's
##report, "Driving While Black." The report can be found at,
##www.drivingwhileblacknashville.org. This script and the associated data files
##may be used to examine the data or to guide future analysis.
##The MNPD traffic stop database is a public record and can be shared widely.
##Additional data can be requested from MNPD using Form 720 available at,
##http://www.nashville.gov/Police-Department/Administrative-Services/Records.aspx.
##If you have any questions about the data or script, please 
##contact Peter Vielehr at peter.vielehr@gmail.com. Additional information 
##about the research is available at www.petervielehr.com.
##Please note that due to the size of the data, the program is 
##memory intensive and some code may give a warning.
#################################################################################
#################################################################################

#Run these lines to load required packages
wants <- c("RCurl","dplyr","chron")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
library(RCurl)
library(dplyr)
library(chron)


###RUN CODE, DO NOT CHANGE###
githubURL <- "https://github.com/PeterVielehr/TrafficStops/blob/master/TrafficStopData2001.Rda?raw=true"
load(url(githubURL))
closeAllConnections()
githubURL <- "https://github.com/PeterVielehr/TrafficStops/blob/master/TrafficStopData2002.Rda?raw=true"
load(url(githubURL))
closeAllConnections()
githubURL <- "https://github.com/PeterVielehr/TrafficStops/blob/master/TrafficStopData2003.Rda?raw=true"
load(url(githubURL))
closeAllConnections()
githubURL <- "https://github.com/PeterVielehr/TrafficStops/blob/master/TrafficStopData2004.Rda?raw=true"
load(url(githubURL))
closeAllConnections()
githubURL <- "https://github.com/PeterVielehr/TrafficStops/blob/master/TrafficStopData2005.Rda?raw=true"
load(url(githubURL))
closeAllConnections()
githubURL <- "https://github.com/PeterVielehr/TrafficStops/blob/master/TrafficStopData2006.Rda?raw=true"
load(url(githubURL))
closeAllConnections()
githubURL <- "https://github.com/PeterVielehr/TrafficStops/blob/master/TrafficStopData2007.Rda?raw=true"
load(url(githubURL))
closeAllConnections()
githubURL <- "https://github.com/PeterVielehr/TrafficStops/blob/master/TrafficStopData2008.Rda?raw=true"
load(url(githubURL))
closeAllConnections()
githubURL <- "https://github.com/PeterVielehr/TrafficStops/blob/master/TrafficStopData2009.Rda?raw=true"
load(url(githubURL))
closeAllConnections()
githubURL <- "https://github.com/PeterVielehr/TrafficStops/blob/master/TrafficStopData2010.Rda?raw=true"
load(url(githubURL))
closeAllConnections()
githubURL <- "https://github.com/PeterVielehr/TrafficStops/blob/master/TrafficStopData2011.Rda?raw=true"
load(url(githubURL))
closeAllConnections()
githubURL <- "https://github.com/PeterVielehr/TrafficStops/blob/master/TrafficStopData2012.Rda?raw=true"
load(url(githubURL))
closeAllConnections()
githubURL <- "https://github.com/PeterVielehr/TrafficStops/blob/master/TrafficStopData2013.Rda?raw=true"
load(url(githubURL))
closeAllConnections()
githubURL <- "https://github.com/PeterVielehr/TrafficStops/blob/master/TrafficStopData2014.Rda?raw=true"
load(url(githubURL))
closeAllConnections()
githubURL <- "https://github.com/PeterVielehr/TrafficStops/blob/master/TrafficStopData2015.Rda?raw=true"
load(url(githubURL))
closeAllConnections()
githubURL <- "https://github.com/PeterVielehr/TrafficStops/blob/master/TrafficStopData2016.Rda?raw=true"
load(url(githubURL))
closeAllConnections()

#Create a year variable for each dataset#
mnpddata01<-mutate(mnpddata01, year = 2001)
mnpddata02<-mutate(mnpddata02, year = 2002)
mnpddata03<-mutate(mnpddata03, year = 2003)
mnpddata04<-mutate(mnpddata04, year = 2004)
mnpddata05<-mutate(mnpddata05, year = 2005)
mnpddata06<-mutate(mnpddata06, year = 2006)
mnpddata07<-mutate(mnpddata07, year = 2007)
mnpddata08<-mutate(mnpddata08, year = 2008)
mnpddata09<-mutate(mnpddata09, year = 2009)
mnpddata10<-mutate(mnpddata10, year = 2010)
mnpddata11<-mutate(mnpddata11, year = 2011)
mnpddata12<-mutate(mnpddata12, year = 2012)
mnpddata13<-mutate(mnpddata13, year = 2013)
mnpddata14<-mutate(mnpddata14, year = 2014)
mnpddata15<-mutate(mnpddata15, year = 2015)
mnpddata16<-mutate(mnpddata16, year = 2016)

#Make a single dataset for focal years

mnpddata<-rbind( mnpddata11, mnpddata12, mnpddata13, mnpddata14, mnpddata15)

#make race/ethnicity variables to create data subset
hispanic<-ifelse(mnpddata$Suspect.Ethnicity=="H" | mnpddata$Suspect.Ethnicity=="Y", 1, 0)
white<-ifelse(hispanic==1, 0, ifelse(mnpddata$Race=="W", 1, 0))
black<-ifelse(hispanic==1, 0, ifelse(mnpddata$Race=="B", 1, 0))
otherrace<-ifelse(white==0 & black==0 & hispanic==0, 1, 0)

#Subset data to exclude individuals that are not white, black, or Hispanic
mnpddata<-subset(mnpddata, otherrace==0)

#Demographics
hispanic<-ifelse(mnpddata$Suspect.Ethnicity=="H" | mnpddata$Suspect.Ethnicity=="Y", 1, 0)
white<-ifelse(hispanic==1, 0, ifelse(mnpddata$Race=="W", 1, 0))
black<-ifelse(hispanic==1, 0, ifelse(mnpddata$Race=="B", 1, 0))
otherrace<-ifelse(white==0 & black==0 & hispanic==0, 1, 0)
race<-ifelse(white==1, "white", ifelse(black==1, "black", ifelse(hispanic==1, "hispanic", NA)))
age<-ifelse(!is.na(mnpddata$Age.of.Suspect), mnpddata$Age.of.Suspect, NA)
female<-ifelse(is.na(mnpddata$Sex), NA, ifelse(mnpddata$Sex=="F", 1, 0))
year<-mnpddata$year
countyresident<-ifelse(mnpddata$County.Resident=="Y", 1, 0)
madestop<-rep(1, length(mnpddata$Stop.Number))


#Population estimates from American Community Survey (ACS)
blackpop11<-133696
blackpop12<-135492
blackpop13<-137564
blackpop14<-141753
blackpop15<-141447
blackpop<-c(blackpop11, blackpop12, blackpop13, blackpop14, blackpop15)

avgblackpop<-sum(blackpop)/5

whitepop11<-311409
whitepop12<-314462
whitepop13<-320411
whitepop14<-326040
whitepop15<-327772
whitepop<-c(whitepop11, whitepop12, whitepop13, whitepop14, whitepop15)

avgwhitepop<-sum(whitepop)/5

hispanicpop11<-42332
hispanicpop12<-42777
hispanicpop13<-42828
hispanicpop14<-43833
hispanicpop15<-44381
hispanicpop<-c(hispanicpop11, hispanicpop12, hispanicpop13, hispanicpop14, hispanicpop15)

avghisppop<-sum(hispanicpop)/5

totalpop11<-487437
totalpop12<-492721
totalpop13<-500803
totalpop14<-511626
totalpop15<-513630
totalpop<-c(totalpop11, totalpop12, totalpop13, totalpop14, totalpop15)

avgtotalpop<-sum(totalpop)/5

percentblack<-blackpop/totalpop
percentwhite<-whitepop/totalpop
percenthispanic<-hispanicpop/totalpop

avgpoppercent<-c(avgblackpop, avghisppop, avgwhitepop)/avgtotalpop

##Finding #2: Between 2011-2015, MNPD stopped an average of 
##  1,122 per 1,000 black drivers
##  more black drivers than were living in Davidson County

blackstop<-rowsum(black,group = year,  na.rm = T)
whitestop<-rowsum(white,group = year, na.rm = T)
hispstop<-rowsum(hispanic,group = year, na.rm = T)
totalstop<-rowsum(madestop,group = year, na.rm = T)

##Figure 2.2
blackprop<-blackstop/totalstop
blackpropavg<-sum(blackprop)/5
whiteprop<-whitestop/totalstop
whitepropavg<-sum(whiteprop)/5
hispprop<-hispstop/totalstop
hisppropavg<-sum(hispprop)/5

#Difference from population (Figure 2.4)
blackpopdiff<-blackprop-percentblack
avgblackpopdiff<-blackpropavg-avgpoppercent[1]
whitepopdiff<-whiteprop-percentwhite
avgwhitepopdiff<-whitepropavg-avgpoppercent[3]
hisppopdiff<-hispprop-percenthispanic
avghisppopdiff<-hisppropavg-avgpoppercent[2]

#Population Rate per 1,000 (Figure 2.3)
blackrate<-(blackstop/blackpop)*1000
whiterate<-(whitestop/whitepop)*1000
hispanicrate<-(hispstop/hispanicpop)*1000
totalrate<-(totalstop/totalpop)*1000
blacktimesgreaterthanwhite<-blackrate/whiterate

avgpopstoprate<-(mean(totalstop)/avgtotalpop)*1000
avgblackrate<-(mean(blackstop)/avgblackpop)*1000
avgwhiterate<-(mean(whitestop)/avgwhitepop)*1000
avghispanicrate<-(mean(hispstop)/avghisppop)*1000

#Total stops residents only
blackresstops<-rowsum(black[countyresident==1],group = year[countyresident==1], na.rm = T)
whiteresstops<-rowsum(white[countyresident==1],group = year[countyresident==1], na.rm = T)
hispanictesstops<-rowsum(hispanic[countyresident==1], group = year[countyresident==1], na.rm = T)
totalresstops<-rowsum(madestop[countyresident==1], group = year[countyresident==1], na.rm = T)

blackresidentsshare<-blackresstops/totalresstops
whiteresidentsshare<-whiteresstops/totalresstops
hispanicresidentsshare<-hispanictesstops/totalresstops

##Figure 2.5
blackrespopdiff<-blackresidentsshare-percentblack
whiterespopdiff<-whiteresidentsshare-percentwhite
hisresppopdiff<-hispanicresidentsshare-percenthispanic

##Finding #3: Black drivers are up to 5 times more likely
##than white drivers to be stopped multiple times in a
##year, showing a disproportionate burden of policing on
##black communities

#Since MNPD Policy changed in July of 2015, the data must be subsetted based on data
datetime15<-as.character(mnpddata15$Stop.Date.Time)
dtpart15 = t(as.data.frame(strsplit(datetime15,' ')))
row.names(dtpart15) = NULL
dates15<- chron(dates=dtpart15[,1], format='m/d/y')
dates15<-as.Date(dates15)

datetime16<-as.character(mnpddata16$Stop.Date.Time)
dtpart16 = t(as.data.frame(strsplit(datetime16,' ')))
row.names(dtpart16) = NULL
dates16<-chron(dates = dtpart16[,1],  format ='m/d/y')
dates16<-as.Date(dates16)

#Merge datasets
mnpddata15.16<-rbind(mnpddata15, mnpddata16)
dates15.16<-c(dates15, dates16)

#Subset correct time frame
mnpd.15.16.year<-subset(mnpddata15.16, dates15.16> as.Date("2015-8-31") & dates15.16< as.Date("2016-9-1"))
yearsub<-as.character(mnpd.15.16.year$Stop.Date.Time)
dtpart15.16 = t(as.data.frame(strsplit(yearsub,' ')))
row.names(dtpart15.16) = NULL
datessub<-chron(dates = dtpart15.16[,1],  format ='m/d/y')
datessub<-as.Date(datessub)

#Confirm correct date range
range(datessub)

#Create demographic variables on subset and remove other race/ethnic groups
dlnumber<-mnpd.15.16.year$Operator.License
hispanicsub<-ifelse(mnpd.15.16.year$Suspect.Ethnicity=="H", 1, 0)
whitesub<-ifelse(hispanicsub==1, 0, ifelse(mnpd.15.16.year$Race=="W", 1, 0))
blacksub<-ifelse(hispanicsub==1, 0, ifelse(mnpd.15.16.year$Race=="B", 1, 0))
otherracesub<-ifelse(whitesub==0 & blacksub==0 & hispanicsub==0, 1, 0)
mnpd.15.16.year<-subset(mnpd.15.16.year, otherracesub==0)
stopdatesubset<-rep(1, length(mnpd.15.16.year$ID))
indnumstopssubset<-rowsum(stopdatesubset[!is.na(mnpd.15.16.year$Operator.License)], group = as.factor(mnpd.15.16.year$Operator.License[!is.na(mnpd.15.16.year$Operator.License)] ), na.rm = T, reorder = T)
hispanicsub<-ifelse(mnpd.15.16.year$Suspect.Ethnicity=="H", 1, 0)
whitesub<-ifelse(hispanicsub==1, 0, ifelse(mnpd.15.16.year$Race=="W", 1, 0))
blacksub<-ifelse(hispanicsub==1, 0, ifelse(mnpd.15.16.year$Race=="B", 1, 0))

#Sum for each unique Driver's Liscense number
blacksubtotal<-rowsum(blacksub[!is.na(mnpd.15.16.year$Operator.License)], group = mnpd.15.16.year$Operator.License[!is.na(mnpd.15.16.year$Operator.License)], na.rm = T, reorder = T)
whitesubtotal<-rowsum(whitesub[!is.na(mnpd.15.16.year$Operator.License)], group = mnpd.15.16.year$Operator.License[!is.na(mnpd.15.16.year$Operator.License)], na.rm = T, reorder = T)
hispanicsubtotal<-rowsum(hispanicsub[!is.na(mnpd.15.16.year$Operator.License)], group = mnpd.15.16.year$Operator.License[!is.na(mnpd.15.16.year$Operator.License)], na.rm = T, reorder = T)

table(blacksubtotal)
table(whitesubtotal)
table(hispanicsubtotal)

stoppedonce<-sum(indnumstopssubset[indnumstopssubset==1])
stopped2to5<-length(indnumstopssubset[indnumstopssubset>=2 & indnumstopssubset<=5])
stopped6to10<-length(indnumstopssubset[indnumstopssubset>=6 & indnumstopssubset<=10])
stoppedgt10<-length(indnumstopssubset[indnumstopssubset>10])
totalstop<-length(indnumstopssubset[indnumstopssubset>0])

blackstoppedonce<-sum(blacksubtotal[blacksubtotal==1])
blackstopped2to5<-length(blacksubtotal[blacksubtotal>=2 & blacksubtotal<=5])
blackstopped6to10<-length(blacksubtotal[blacksubtotal>=6 & blacksubtotal<=10])
blackstoppedgt10<-length(blacksubtotal[blacksubtotal>10])
blacktotalstop<-length(blacksubtotal[blacksubtotal>0])
blackstoptotal<-sum(blacksubtotal[blacksubtotal>=1])
blackproponce<-blackstoppedonce/blacktotalstop
blackprop2to5<-blackstopped2to5/blacktotalstop
blackprop6to10<-blackstopped6to10/blacktotalstop
blackpropgt10<-blackstoppedgt10/blacktotalstop

whitestoppedonce<-sum(whitesubtotal[whitesubtotal==1])
whitestopped2to5<-length(whitesubtotal[whitesubtotal>=2 & whitesubtotal<=5])
whitestopped6to10<-length(whitesubtotal[whitesubtotal>=6 & whitesubtotal<=10])
whitestoppedgt10<-length(whitesubtotal[whitesubtotal>10])
whitetotalstop<-length(whitesubtotal[whitesubtotal>0])
whiteproponce<-whitestoppedonce/whitetotalstop
whiteprop2to5<-whitestopped2to5/whitetotalstop
whiteprop6to10<-whitestopped6to10/whitetotalstop
whitepropgt10<-whitestoppedgt10/whitetotalstop

hispanicstoppedonce<-sum(hispanicsubtotal[hispanicsubtotal==1])
hispanicstopped2to5<-length(hispanicsubtotal[hispanicsubtotal>=2 & hispanicsubtotal<=5])
hispanicstopped6to10<-length(hispanicsubtotal[hispanicsubtotal>=6 & hispanicsubtotal<=10])
hispanicstoppedgt10<-length(hispanicsubtotal[hispanicsubtotal>10])
hispanictotalstop<-length(hispanicsubtotal[hispanicsubtotal>0])
hispanicproponce<-hispanicstoppedonce/hispanictotalstop
hispanicprop2to5<-hispanicstopped2to5/hispanictotalstop
hispanicprop6to10<-hispanicstopped6to10/hispanictotalstop
hispanicpropgt10<-hispanicstoppedgt10/hispanictotalstop

##Figures 3.1 and 3.2
blackstoppedonce/blackpop15*1000
blackstopped2to5/blackpop15*1000
blackstopped6to10/blackpop15*1000
blackstoppedgt10/blackpop15*1000
blacktotalstop/blackpop15*1000

whitestoppedonce/whitepop15*1000
whitestopped2to5/whitepop15*1000
whitestopped6to10/whitepop15*1000
whitestoppedgt10/whitepop15*1000
whitetotalstop/whitepop15*1000

hispanicstoppedonce/hispanicpop15*1000
hispanicstopped2to5/hispanicpop15*1000
hispanicstopped6to10/hispanicpop15*1000
hispanicstoppedgt10/hispanicpop15*1000
hispanictotalstop/hispanicpop15*1000

##########################################################
##Finding #5: Given the disproportionate rates of traffic
##stops of black drivers across all reasons for initiating
##a stop, it seems that, for MNPD, “driving while black”
##constitutes a de facto reason for initiating a stop
##########################################################

#Vehicle Equiptment Violation (VEV)
vevstop<-ifelse(mnpddata$Stop.Type=="VEV", 1, 0)
totalvevstop<-sum(vevstop, na.rm = T)
whitevev<-ifelse(white==1 & vevstop==1, 1, 0)
whitevev<-sum(whitevev, na.rm = T)
whitevevprop<-whitevev/totalvevstop
blackvev<-ifelse(black==1 & vevstop==1, 1, 0)
blackvev<-sum(blackvev, na.rm = T)
blackvevprop<-blackvev/totalvevstop
hispanicvev<-ifelse(hispanic==1 & vevstop==1, 1, 0)
hispanicvev<-sum(hispanicvev, na.rm = T)
hispanicvevprop<-hispanicvev/totalvevstop

blackvevpopdiff<-blackvevprop-avgpoppercent[1]
hispvevpopdiff<-hispanicvevprop-avgpoppercent[2]
whitevevpopdiff<-whitevevprop-avgpoppercent[3]

#Investigatory Stops
invstop<-ifelse(mnpddata$Stop.Type=="INV", 1, 0)
totalinvstop<-sum(invstop, na.rm = T)
whiteinv<-ifelse(white==1 & invstop==1, 1, 0)
whiteinv<-sum(whiteinv, na.rm = T)
whiteinvprop<-whiteinv/totalinvstop
blackinv<-ifelse(black==1 & invstop==1, 1, 0)
blackinv<-sum(blackinv, na.rm = T)
blackinvprop<-blackinv/totalinvstop
hispanicinv<-ifelse(hispanic==1 & invstop==1, 1, 0)
hispanicinv<-sum(hispanicinv, na.rm = T)
hispanicinvprop<-hispanicinv/totalinvstop

blackinvpopdiff<-blackinvprop-avgpoppercent[1]
hispinvpopdiff<-hispanicinvprop-avgpoppercent[2]
whiteinvpopdiff<-whiteinvprop-avgpoppercent[3]

#Moving Traffic Violation
mtvstop<-ifelse(mnpddata$Stop.Type=="MTV" | mnpddata$Stop.Type=="MOVNG", 1, 0)
totalmtvstop<-sum(mtvstop, na.rm = T)
whitemtv<-ifelse(white==1 & mtvstop==1, 1, 0)
whitemtv<-sum(whitemtv, na.rm = T)
whitemtvprop<-whitemtv/totalmtvstop
blackmtv<-ifelse(black==1 & mtvstop==1, 1, 0)
blackmtv<-sum(blackmtv, na.rm = T)
blackmtvprop<-blackmtv/totalmtvstop
hispanicmtv<-ifelse(hispanic==1 & mtvstop==1, 1, 0)
hispanicmtv<-sum(hispanicmtv, na.rm = T)
hispanicmtvprop<-hispanicmtv/totalmtvstop

blackmtvpopdiff<-blackmtvprop-avgpoppercent[1]
hispmtvpopdiff<-hispanicmtvprop-avgpoppercent[2]
whitemtvpopdiff<-whitemtvprop-avgpoppercent[3]

#Parking Stop
parkstop<-ifelse(mnpddata$Stop.Type=="PARK", 1, 0)
totalparkstop<-sum(parkstop, na.rm = T)
whitepark<-ifelse(white==1 & parkstop==1, 1, 0)
whitepark<-sum(whitepark, na.rm = T)
whiteparkprop<-whitepark/totalparkstop
blackpark<-ifelse(black==1 & parkstop==1, 1, 0)
blackpark<-sum(blackpark, na.rm = T)
blackparkprop<-blackpark/totalparkstop
hispanicpark<-ifelse(hispanic==1 & parkstop==1, 1, 0)
hispanicpark<-sum(hispanicpark, na.rm = T)
hispanicparkprop<-hispanicpark/totalparkstop

blackparkpopdiff<-blackparkprop-avgpoppercent[1]
hispparkpopdiff<-hispanicparkprop-avgpoppercent[2]
whiteparkpopdiff<-whiteparkprop-avgpoppercent[3]

#Regulatory Stop
regstop<-ifelse(mnpddata$Stop.Type=="REGS", 1, 0)
totalregstop<-sum(regstop, na.rm = T)
whitereg<-ifelse(white==1 & regstop==1, 1, 0)
whitereg<-sum(whitereg, na.rm = T)
whiteregprop<-whitereg/totalregstop
blackreg<-ifelse(black==1 & regstop==1, 1, 0)
blackreg<-sum(blackreg, na.rm = T)
blackregprop<-blackreg/totalregstop
hispanicreg<-ifelse(hispanic==1 & regstop==1, 1, 0)
hispanicreg<-sum(hispanicreg, na.rm = T)
hispanicregprop<-hispanicreg/totalregstop

blackregpopdiff<-blackregprop-avgpoppercent[1]
hispregpopdiff<-hispanicregprop-avgpoppercent[2]
whiteregpopdiff<-whiteregprop-avgpoppercent[3]

#Seatbelt Stop
beltstop<-ifelse(mnpddata$Stop.Type=="S/BELT", 1, 0)
totalbeltstop<-sum(beltstop, na.rm = T)
whitebelt<-ifelse(white==1 & beltstop==1, 1, 0)
whitebelt<-sum(whitebelt, na.rm = T)
whitebeltprop<-whitebelt/totalbeltstop
blackbelt<-ifelse(black==1 & beltstop==1, 1, 0)
blackbelt<-sum(blackbelt, na.rm = T)
blackbeltprop<-blackbelt/totalbeltstop
hispanicbelt<-ifelse(hispanic==1 & beltstop==1, 1, 0)
hispanicbelt<-sum(hispanicbelt, na.rm = T)
hispanicbeltprop<-hispanicbelt/totalbeltstop

blackbeltpopdiff<-blackbeltprop-avgpoppercent[1]
hispbeltpopdiff<-hispanicbeltprop-avgpoppercent[2]
whitebeltpopdiff<-whitebeltprop-avgpoppercent[3]

#Safety Stop
safestop<-ifelse(mnpddata$Stop.Type=="SAFETY", 1, 0)
totalsafestop<-sum(safestop, na.rm = T)
whitesafe<-ifelse(white==1 & safestop==1, 1, 0)
whitesafe<-sum(whitesafe, na.rm = T)
whitesafeprop<-whitesafe/totalsafestop
blacksafe<-ifelse(black==1 & safestop==1, 1, 0)
blacksafe<-sum(blacksafe, na.rm = T)
blacksafeprop<-blacksafe/totalsafestop
hispanicsafe<-ifelse(hispanic==1 & safestop==1, 1, 0)
hispanicsafe<-sum(hispanicsafe, na.rm = T)
hispanicsafeprop<-hispanicsafe/totalsafestop

blacksafepopdiff<-blacksafeprop-avgpoppercent[1]
hispsafepopdiff<-hispanicsafeprop-avgpoppercent[2]
whitesafepopdiff<-whitesafeprop-avgpoppercent[3]

##Figure 5.1
reasonspopdiffdf<-data.frame(blackvevpopdiff, hispvevpopdiff, whitevevpopdiff,
                             blackinvpopdiff, hispinvpopdiff, whiteinvpopdiff,
                             blackmtvpopdiff, hispmtvpopdiff, whitemtvpopdiff,
                             blackparkpopdiff, hispparkpopdiff, whiteparkpopdiff,
                             blackregpopdiff, hispregpopdiff, whiteregpopdiff,
                             blackbeltpopdiff, hispbeltpopdiff, whitebeltpopdiff,
                             blacksafepopdiff, hispsafepopdiff, whitesafepopdiff)

#######################################################
##Finding #6: MNPD officers conduct probable cause and
##consent searches of black and Hispanic drivers at more
##than twice the rate of white drivers
#######################################################

#searches
search<-ifelse(mnpddata$SEARCHOCCUR=="Y", 1, 0)
vehiclesearch<-ifelse(mnpddata$Vehicle.Searched=="Y", 1, 0)
patdown<-ifelse(mnpddata$Pat.Down.Search=="Y", 1, 0)
driversearch<-ifelse(mnpddata$Driver.Searched=="Y", 1, 0)
passengersearch<-ifelse(mnpddata$Passenger.Searched=="Y", 1, 0)

#search justificaiton
probablecause<-ifelse(mnpddata$Search.Probable.Cause=="Y", 1, 0)
consentsearch<-ifelse(probablecause==1, 0, ifelse(mnpddata$Search.Consent=="Y", 1, 0))
warrantsearch<-ifelse(mnpddata$Search.Warrant=="Y", 1, 0)
plainview<-ifelse(mnpddata$Search.Plain.View=="Y", 1, 0)
inventorysearch<-ifelse(mnpddata$Search.Inventory=="Y", 1, 0)
arrestsearch<-ifelse(mnpddata$Search.Arrest=="Y", 1, 0)

#all searches totals (Figure 6.1)
blacksearches<-ifelse(black==1 & search==1, 1, 0)
whitesearches<-ifelse(white==1 & search==1, 1, 0)
hispsearches<-ifelse(hispanic==1 & search==1, 1, 0)
blacksearches<-rowsum(blacksearches,group = year,  na.rm = T)
whitesearches<-rowsum(whitesearches,group = year, na.rm = T)
hispsearches<-rowsum(hispsearches,group = year, na.rm = T)

#Probable Cause Search totals (Figure 6.2)
blackpcsearches<-ifelse(black==1 & probablecause==1, 1, 0)
whitepcsearches<-ifelse(white==1 & probablecause==1, 1, 0)
hisppcsearches<-ifelse(hispanic==1 & probablecause==1, 1, 0)
blackpcsearches<-rowsum(blackpcsearches,group = year,  na.rm = T)
whitepcsearches<-rowsum(whitepcsearches,group = year, na.rm = T)
hisppcsearches<-rowsum(hisppcsearches,group = year, na.rm = T)

#Probable cause search odds (Figure 6.3)
pcodds<-sapply(2011:2015, function(x) exp(coef(glm(probablecause[year==x]~ black[ year==x] + hispanic[year==x], family = "binomial"))))
blackpcodds<-pcodds[2 , 1:5]
hisppcodds<-pcodds[3, 1:5]
whitepcodds<-rep(1, 5)

#Consent Search totals (Figure 6.4)
blackconsentsearches<-rowsum(consentsearch[black==1],group = year[black==1],  na.rm = T)
whiteconsentsearches<-rowsum(consentsearch[white==1],group = year[white==1], na.rm = T)
hispconsentsearches<-rowsum(consentsearch[hispanic==1],group = year[hispanic==1], na.rm = T)
totalconsentsearch<-rowsum(consentsearch, group = year, na.rm = T, reorder = T)

#Consent search odds (Figure 6.5)
consentodds<-sapply(2011:2015, function(x) exp(coef(glm(consentsearch[year==x]~ black[ year==x] + hispanic[year==x], family = "binomial"))))
blackconsentodds<-consentodds[2 , 1:5]
hispconsentodds<-consentodds[3, 1:5]
whiteconsentodds<-rep(1, 5)

##Plain View Searches (figure 6.6)
blackplainsearches<-ifelse(black==1 & plainview==1, 1, 0)
whiteplainsearches<-ifelse(white==1 & plainview==1, 1, 0)
hispplainsearches<-ifelse(hispanic==1 & plainview==1, 1, 0)
blackplainsearches<-rowsum(blackplainsearches,group = year,  na.rm = T)
whiteplainsearches<-rowsum(whiteplainsearches,group = year, na.rm = T)
hispplainsearches<-rowsum(hispplainsearches,group = year, na.rm = T)
totalplainsearches<-rowsum(plainview, group = year, na.rm = T)

#Plain View search odds (Figure 6.7)
plainviewodds<-sapply(2011:2015, function(x) exp(coef(glm(plainview[year==x]~ black[ year==x] + hispanic[year==x], family = "binomial"))))
blackpvodds<-plainviewodds[2 , 1:5]
hisppvodds<-plainviewodds[3, 1:5]
whitepvodds<-rep(1, 5)

#Pat down Search totals (Figure 6.8)
patdowntotalyr<-rowsum(patdown, group = year, na.rm = T, reorder = T )
patdownblackyr<-rowsum(patdown[black==1], group = year[black==1], na.rm = T, reorder = T )
patdownwhiteyr<-rowsum(patdown[white==1], group = year[white==1], na.rm = T, reorder = T )
patdownhispanicyr<-rowsum(patdown[hispanic==1], group = year[hispanic==1], na.rm = T, reorder = T )

#Pat Down search odds (Figure 6.9)
patdownodds<-sapply(2011:2015, function(x) exp(coef(glm(patdown[year==x]~ black[ year==x] + hispanic[year==x], family = "binomial"))))
blackpatdownodds<-patdownodds[2 , 1:5]
hisppatdownodds<-patdownodds[3, 1:5]
whitepatdownodds<-rep(1, 5)


##############################################################
##Finding #7: Discretionary searches of white drivers result
##in the discovery of incriminating evidence more often
##than discretionary searches of black and Hispanic drivers
#############################################################

#evidence found
evidence<-ifelse(is.na(mnpddata$EVIDENCESEIZED), NA, ifelse(mnpddata$EVIDENCESEIZED=="Y", 1, 0))
drugs<-ifelse(is.na(mnpddata$Drugs.Seized), NA, ifelse(mnpddata$Drugs.Seized=="Y", 1, 0))
weapons<-ifelse(is.na(mnpddata$Weapons.Seized), NA, ifelse(mnpddata$Weapons.Seized=="Y", 1, 0))
otherseized<-ifelse(is.na(mnpddata$Other.Seized), NA, ifelse(mnpddata$Other.Seized=="Y", 1, 0))

#Accuracy of Probable Cause Searches (Figure 7.1)
totalpcsearches<-rowsum(probablecause, group=year, na.rm = T, reorder = T)
pcsuccess<-ifelse(probablecause==1 & evidence==1, 1, 0)
pcsuccessyr<-rowsum(pcsuccess, group = year, na.rm = T, reorder = T)
percentpcsuccess<-pcsuccessyr/totalpcsearches
pcaccuracyyr<-rowsum(pcsuccess, group = year, na.rm = T, reorder = T)
blackpcsucessyr<-rowsum(pcsuccess[black==1], group = year[black==1], na.rm = T, reorder = T )
whitepcsucessyr<-rowsum(pcsuccess[white==1], group = year[white==1], na.rm = T, reorder = T )
hisppcsearchsuccessyr<-rowsum(pcsuccess[hispanic==1], group = year[hispanic==1], na.rm = T, reorder = T )

proppcsuccess<-pcaccuracyyr/totalpcsearches
propblackpcsucess<-blackpcsucessyr/totalpcsearches
propwhitepcsucess<-whitepcsucessyr/totalpcsearches
prophisppcsucess<-hisppcsearchsuccessyr/totalpcsearches

#Consent Search Accuracy (Figure 7.2)
consentsuccess<-ifelse(consentsearch==1 & evidence==1, 1, 0)
blackconsentsucess<-ifelse(black==1 & consentsearch==1 & evidence==1, 1, 0)
whiteconsentsucess<-ifelse(white==1 & consentsearch==1 & evidence==1, 1, 0)
hispanicconsentsucess<-ifelse(hispanic==1 & consentsearch==1 & evidence==1, 1, 0)
consentaccuracyyr<-rowsum(consentsuccess, group = year, na.rm = T, reorder = T)
blackconsentsucessyr<-rowsum(blackconsentsucess, group = year, na.rm = T, reorder = T )
whiteconsentsucessyr<-rowsum(whiteconsentsucess, group = year, na.rm = T, reorder = T )
hispconsearchsuccessyr<-rowsum(hispanicconsentsucess, group = year, na.rm = T, reorder = T )

propconsentsuccess<-consentaccuracyyr/totalconsentsearch
propblackconsentsucess<-blackconsentsucessyr/blackconsentsearches
propwhiteconsentsucess<-whiteconsentsucessyr/whiteconsentsearches
prophispconsentsucess<-hispconsearchsuccessyr/hispconsentsearches

#plain view accuracy With Alleged Probable cause (Figure 7.3)
plainviewpc<-ifelse(mnpddata$Search.Plain.View=="Y" & mnpddata$Search.Probable.Cause=="Y", 1, 0)

totalpvyr<-rowsum(plainviewpc, group = year, na.rm = T, reorder = T )
blackpvyr<-rowsum(plainviewpc[black==1], group = year[black==1], na.rm = T, reorder = T )
whitepvyr<-rowsum(plainviewpc[white==1], group = year[white==1], na.rm = T, reorder = T )
hispanicpvyr<-rowsum(plainview[hispanic==1], group = year[hispanic==1], na.rm = T, reorder = T )
pvsuccess<-ifelse(plainviewpc==1 & evidence==1, 1, 0)
pvsuccessyr<-rowsum(pvsuccess, group = year, na.rm = T, reorder = T)
percentpvsuccess<-pvsuccessyr/totalpvyr
pvaccuracyyr<-rowsum(pvsuccess, group = year, na.rm = T, reorder = T)
blackpvsucessyr<-rowsum(pvsuccess[black==1], group = year[black==1], na.rm = T, reorder = T )
whitepvsucessyr<-rowsum(pvsuccess[white==1], group = year[white==1], na.rm = T, reorder = T )
hisppvsearchsuccessyr<-rowsum(pvsuccess[hispanic==1], group = year[hispanic==1], na.rm = T, reorder = T )
proppvsuccess<-pvaccuracyyr/totalpvyr
#Figure 7.5
propblackpvsucess<-blackpvsucessyr/blackpvyr
propwhitepvsucess<-whitepvsucessyr/whitepvyr
prophisppvsucess<-hisppvsearchsuccessyr/hispanicpvyr


#plain view accuracy Without Probable Cause (FIgure 7.4)
plainviewnopc<-ifelse(mnpddata$Search.Plain.View=="Y" & mnpddata$Search.Probable.Cause=="N", 1, 0)

totalpvyr<-rowsum(plainviewnopc, group = year, na.rm = T, reorder = T )
blackpvyr<-rowsum(plainviewnopc[black==1], group = year[black==1], na.rm = T, reorder = T )
whitepvyr<-rowsum(plainviewnopc[white==1], group = year[white==1], na.rm = T, reorder = T )
hispanicpvyr<-rowsum(plainviewnopc[hispanic==1], group = year[hispanic==1], na.rm = T, reorder = T )
pvsuccess<-ifelse(plainviewnopc==1 & evidence==1, 1, 0)
pvsuccessyr<-rowsum(pvsuccess, group = year, na.rm = T, reorder = T)
percentpvsuccess<-pvsuccessyr/totalpvyr

pvaccuracyyr<-rowsum(pvsuccess, group = year, na.rm = T, reorder = T)
blackpvsucessyr<-rowsum(pvsuccess[black==1], group = year[black==1], na.rm = T, reorder = T )
whitepvsucessyr<-rowsum(pvsuccess[white==1], group = year[white==1], na.rm = T, reorder = T )
hisppvsearchsuccessyr<-rowsum(pvsuccess[hispanic==1], group = year[hispanic==1], na.rm = T, reorder = T )
proppvsuccess<-pvaccuracyyr/totalpvyr
#Figure 7.6
propblackpvsucess<-blackpvsucessyr/blackpvyr
propwhitepvsucess<-whitepvsucessyr/whitepvyr
prophisppvsucess<-hisppvsearchsuccessyr/hispanicpvyr

############################################################
##Finding #8: Nearly 80% of all MNPD traffic stops in 2015
##resulted in a warning, and in traffic stops including a
##search of the vehicle or driver, between one-third and
##half resulted in a warning, which means hundreds of
##thousands of drivers—a disproportionate number of
##whom are black—are being stopped unnecessarily
##########################################################

#stop outcome
verbalwarn<-ifelse(mnpddata$Verbal.Warning.Issued=="Y", 1, 0)
writtenwarn<-ifelse(mnpddata$Written.Warning.Issued=="Y", 1, 0)
warn<-ifelse(mnpddata$Verbal.Warning.Issued=="Y" | mnpddata$Written.Warning.Issued=="Y", 1, 0)
trafficcite<-ifelse(mnpddata$Traffic.Citation.Issued=="Y", 1, 0)
statecite<-ifelse(mnpddata$Misd.State.Citation.Issued=="Y", 1, 0)
arrest<-ifelse(mnpddata$Custodial.Arrest.Issued=="Y", 1, 0)

warnyr<-rowsum(warn, group = year, na.rm = T, reorder = T)
tciteyr<-rowsum(trafficcite, group = year, na.rm = T, reorder = T)
sciteyr<-rowsum(statecite, group = year, na.rm = T, reorder = T)
arrestyr<-rowsum(arrest, group = year, na.rm = T, reorder = T)

blackwarnyr<-rowsum(warn[black==1], group = year[black==1], na.rm = T, reorder = T)
blacktciteyr<-rowsum(trafficcite[black==1], group = year[black==1], na.rm = T, reorder = T)
blacksciteyr<-rowsum(statecite[black==1], group = year[black==1], na.rm = T, reorder = T)
blackarrestyr<-rowsum(arrest[black==1], group = year[black==1], na.rm = T, reorder = T)

blackwarnyrprop<-blackwarnyr/warnyr
blacktciteyrprop<-blacktciteyr/tciteyr
blacksciteyrprop<-blacksciteyr/sciteyr
blackarrestyrprop<-blackarrestyr/arrestyr

whitewarnyr<-rowsum(warn[white==1], group = year[white==1], na.rm = T, reorder = T)
whitetciteyr<-rowsum(trafficcite[white==1], group = year[white==1], na.rm = T, reorder = T)
whitesciteyr<-rowsum(statecite[white==1], group = year[white==1], na.rm = T, reorder = T)
whitearrestyr<-rowsum(arrest[white==1], group = year[white==1], na.rm = T, reorder = T)

whitewarnyrprop<-whitewarnyr/warnyr
whitetciteyrprop<-whitetciteyr/tciteyr
whitesciteyrprop<-whitesciteyr/sciteyr
whitearrestyrprop<-whitearrestyr/arrestyr

hispwarnyr<-rowsum(warn[hispanic==1], group = year[hispanic==1], na.rm = T, reorder = T)
hisptciteyr<-rowsum(trafficcite[hispanic==1], group = year[hispanic==1], na.rm = T, reorder = T)
hispsciteyr<-rowsum(statecite[hispanic==1], group = year[hispanic==1], na.rm = T, reorder = T)
hisparrestyr<-rowsum(arrest[hispanic==1], group = year[hispanic==1], na.rm = T, reorder = T)

hispwarnyrprop<-hispwarnyr/warnyr
hisptciteyrprop<-hisptciteyr/tciteyr
hispsciteyrprop<-hispsciteyr/sciteyr
hisparrestyrprop<-hisparrestyr/arrestyr

#Population differences (Figure 8.1)
blackwarningpopdiff<-blackwarnyrprop-percentblack
whitewarningpopdiff<-whitewarnyrprop-percentwhite
hispwarningpopdiff<-hispwarnyrprop-percenthispanic

blacktcitepopdiff<-blacktciteyrprop-percentblack
whitetcitepopdiff<-whitetciteyrprop-percentwhite
hisptcitepopdiff<-hisptciteyrprop-percenthispanic  
  
blackscitepopdiff<-blacksciteyrprop-percentblack
whitescitepopdiff<-whitesciteyrprop-percentwhite
hispscitepopdiff<-hispsciteyrprop-percenthispanic  

blackarrestpopdiff<-blackarrestyrprop-percentblack
whitearrestpopdiff<-whitearrestyrprop-percentwhite
hisparrestpopdiff<-hisparrestyrprop-percenthispanic  

#All Years (finding 8: Consent Searches leading to warning)
mnpddataallyears<-rbind(mnpddata01, mnpddata02, mnpddata03, mnpddata04, mnpddata05, 
                        mnpddata06, mnpddata07, mnpddata08, mnpddata09, mnpddata10,
                        mnpddata11, mnpddata12, mnpddata13, mnpddata14, mnpddata15)

hispanicallyr<-ifelse(mnpddataallyears$Suspect.Ethnicity=="H" | mnpddataallyears$Suspect.Ethnicity=="Y", 1, 0)
whiteallyr<-ifelse(hispanicallyr==1, 0, ifelse(mnpddataallyears$Race=="W", 1, 0))
blackallyr<-ifelse(hispanicallyr==1, 0, ifelse(mnpddataallyears$Race=="B", 1, 0))
otherraceallyr<-ifelse(whiteallyr==0 & blackallyr==0 & hispanicallyr==0, 1, 0)

mnpddataallyears<-subset(mnpddataallyears, otherraceallyr==0)

hispanicallyr<-ifelse(mnpddataallyears$Suspect.Ethnicity=="H" | mnpddataallyears$Suspect.Ethnicity=="Y", 1, 0)
whiteallyr<-ifelse(hispanicallyr==1, 0, ifelse(mnpddataallyears$Race=="W", 1, 0))
blackallyr<-ifelse(hispanicallyr==1, 0, ifelse(mnpddataallyears$Race=="B", 1, 0))
otherraceallyr<-ifelse(whiteallyr==0 & blackallyr==0 & hispanicallyr==0, 1, 0)
raceallyr<-ifelse(whiteallyr==1, "white", ifelse(blackallyr==1, "black", ifelse(hispanicallyr==1, "hispanic", NA)))
ageallyr<-ifelse(!is.na(mnpddataallyears$Age.of.Suspect), mnpddataallyears$Age.of.Suspect, NA)
agecaallyrt<-ifelse(age>=16 & age<18, "16-17", 
               ifelse(age>=18 & age<25, "18-24",
                      ifelse(age>=25 & age<30, "25-29",
                             ifelse(age>=30 & age<35, "30-34",
                                    ifelse(age>=35 & age<40, "35-39",
                                           ifelse(age>=40 & age<45, "40-44",
                                                  ifelse(age>=45 & age<50, "45-49",
                                                         ifelse(age>=50 & age<55, "50-54",
                                                                ifelse(age>=55 & age<60, "55-59",
                                                                       ifelse(age>=60 & age<65, "60-64",
                                                                              ifelse(age>=65 & age<70, "65-69",
                                                                                     ifelse(age>=70,"70+", NA))))))))))))

femaleallyr<-ifelse(is.na(mnpddataallyears$Sex), NA, ifelse(mnpddataallyears$Sex=="F", 1, 0))
yearallyr<-mnpddataallyears$year

madestopallyr<-rep(1, length(mnpddataallyears$Stop.Number))
totalstopsallyears<-rowsum(madestopallyr, group = yearallyr, na.rm = T, reorder = T)
blackstopallyr<-rowsum(blackallyr,group = yearallyr,  na.rm = T, reorder = T)
whitestopallyr<-rowsum(whiteallyr,group = yearallyr, na.rm = T, reorder = T)
hispstopallyr<-rowsum(hispanicallyr,group = yearallyr, na.rm = T, reorder = T)
otherstopallyr<-rowsum(otherraceallyr,group = yearallyr, na.rm = T, reorder = T)

blackpropallyr<-blackstopallyr/totalstopsallyears
whitepropallyr<-whitestopallyr/totalstopsallyears
hisppropallyr<-hispstopallyr/totalstopsallyears

#search justificaiton
probablecauseallyr<-ifelse(mnpddataallyears$Search.Probable.Cause=="Y", 1, 0)
consentsearchallyr<-ifelse(probablecauseallyr==1, 0, ifelse(mnpddataallyears$Search.Consent=="Y", 1, 0))
warrantsearchallyr<-ifelse(mnpddataallyears$Search.Warrant=="Y", 1, 0)
plainviewallyr<-ifelse(mnpddataallyears$Search.Plain.View=="Y", 1, 0)
inventorysearchallyr<-ifelse(mnpddataallyears$Search.Inventory=="Y", 1, 0)
arrestsearchallyr<-ifelse(mnpddataallyears$Search.Arrest=="Y", 1, 0)

blackconsentallyr<-rowsum(consentsearchallyr[blackallyr==1], group = yearallyr[blackallyr==1], na.rm = T, reorder = T)
whiteconsentallyr<-rowsum(consentsearchallyr[whiteallyr==1], group = yearallyr[whiteallyr==1], na.rm = T, reorder = T)
hispanicconsentallyr<-rowsum(consentsearchallyr[hispanicallyr==1], group = yearallyr[hispanicallyr==1], na.rm = T, reorder = T)
hispanicconsentallyr<-append(hispanicconsentallyr, c(0), after = 0)
otherconsentallyr<-rowsum(consentsearchallyr[otherraceallyr==1], group = yearallyr[otherraceallyr==1], na.rm = T, reorder = T)
totalconsentallyr<-rowsum(consentsearchallyr, group = yearallyr, na.rm = T, reorder = T)

#Outcomes

verbalwarnallyr<-ifelse(mnpddataallyears$Verbal.Warning.Issued=="Y", 1, 0)
writtenwarnallyr<-ifelse(mnpddataallyears$Written.Warning.Issued=="Y", 1, 0)
trafficciteallyr<-ifelse(mnpddataallyears$Traffic.Citation.Issued=="Y", 1, 0)
stateciteallyr<-ifelse(mnpddataallyears$Misd.State.Citation.Issued=="Y", 1, 0)
arrestallyr<-ifelse(mnpddataallyears$Custodial.Arrest.Issued=="Y", 1, 0)

warnallyr<-ifelse(trafficciteallyr==1, 0, 
             ifelse(stateciteallyr==1, 0, 
                    ifelse(arrestallyr==1, 0, 
                           ifelse(verbalwarnallyr==1, 1, 
                                  ifelse( writtenwarnallyr==1, 1, 0)))))

warnyrallyr<-rowsum(warnallyr, group = yearallyr, na.rm = T, reorder = T)
tciteyrallyr<-rowsum(trafficciteallyr, group = yearallyr, na.rm = T, reorder = T)
sciteyrallyr<-rowsum(stateciteallyr, group = yearallyr, na.rm = T, reorder = T)
arrestyrallyr<-rowsum(arrestallyr, group = yearallyr, na.rm = T, reorder = T)

blackconsentwarnyr<-rowsum(warnallyr[ consentsearchallyr==1 & blackallyr==1], group = yearallyr[ consentsearchallyr==1 & blackallyr==1], na.rm = T, reorder = T)
whiteconsentwarnyr<-rowsum(warnallyr[ consentsearchallyr==1 & whiteallyr==1], group = yearallyr[ consentsearchallyr==1 & whiteallyr==1], na.rm = T, reorder = T)
hispconsentwarnyr<-rowsum(warnallyr[ consentsearchallyr==1 & hispanicallyr==1], group = yearallyr[ consentsearchallyr==1 & hispanicallyr==1], na.rm = T, reorder = T)
hispconsentwarnyr<-append(hispconsentwarnyr, c(0), after = 0)

##Figure 8.2
blackconsenttowarn<-blackconsentwarnyr/blackconsentallyr
whiteconsenttowarn<-whiteconsentwarnyr/whiteconsentallyr
hispconsenttowarn<-hispconsentwarnyr/hispanicconsentallyr

##Figure 8.3
warnprop<-warnyrallyr/totalstopsallyears
tciteprop<-tciteyrallyr/totalstopsallyears
sciteprop<-sciteyrallyr/totalstopsallyears
arrestprop<-arrestyrallyr/totalstopsallyears

