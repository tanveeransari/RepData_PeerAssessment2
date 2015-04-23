#================================START REPR RESEARCH PEER ASSIGNMENT 2
#  must use database to answer the questions below and show the code for your entire analysis.
#  analysis can consist of tables, figures, or other summaries.
#
# Must address the following questions:
#  1) Across the United States, which types of events (as indicated in the EVTYPE variable)
#                                       are most harmful with respect to population health?
#  2) Across the United States, which types of events have the greatest economic consequences?
#
# NOTE : The events in the database start in the year 1950 and end in November 2011.
# In earlier years  generally fewer events recorded,  due to a lack of good records.
# More recent years should be considered more complete.
#
#
# Consider writing your report as if it were to be read by a government or municipal manager
require(knitr);
require(data.table); require(plyr);require(dplyr);require(tidyr);require(ggplot2);require(reshape2);
library(data.table); library(plyr);library(dplyr);library(tidyr);library(ggplot2);library(reshape2);


# download.file
if(!file.exists("stormdata.csv.bz2")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                destfile = "stormdata.csv.bz2")
}
## READ raw data from file
#storm <- read.csv("stormdata.csv.bz2",stringsAsFactors=F)
raw<-read.csv("stormdata.csv.bz2",stringsAsFactors=F,na.strings=c("NA,NAN",""))
storm<-tbl_df(raw)
#rm(raw)

## Only select those events with either fatalities or injuries
injuries=subset(storm, FATALITIES>0 | INJURIES>0)
##  Aggregate data by FATALITIES & INJURIES
#inj<-aggregate(cbind(FATALITIES,INJURIES)~EVTYPE, data=storm,sum)
inj<-injuries%>% group_by(EVTYPE)%>%summarize(killed=sum(FATALITIES,na.rm = T),hurt=sum(INJURIES,na.rm=T))
#inj<-arrange(t,desc(killed),desc(hurt))
# find top 10% of fatalities
quantile(inj$killed,prob=seq(0.95,1,by=0.01))
#Gives us 208 as 99th percentile - select event types killed more than 200 people in total?
inj<-subset(inj,killed>500)
inj<-arrange(inj,desc(killed),desc(hurt))
## Plot top harmful events
injmelt<-melt(inj,id=c("EVTYPE"),measure.vars=c("killed","hurt"))
#qplot(EVTYPE,value,data=injmelt,facets=variable~.,color=EVTYPE)

ggplot(injmelt,aes(x=EVTYPE,y=value,fill=factor(variable)))
+geom_bar(position="dodge",stat="identity")+ggtitle("Storm Events causing Top 6 Fatalities")

###ANALYZE TOP ECONOMIC CONSEQUENCES
## only select those events with either property or crop damage
stormdmg<-subset(storm,PROPDMG>0&!is.na(PROPDMGEXP)|(CROPDMG>0&!is.na(CROPDMGEXP)))
## take only columns we are interested in
# stormdmg<-select(stormdmg,EVTYPE,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP,STATE,BGN_DATE,BGN_TIME,TIME_ZONE)
stormdmg<-select(stormdmg,EVTYPE,PROPDMG,PROPDMGEXP,CROPDMG,CROdPDMGEXP)
#Some of the above suffixes appear to be invalid.
sort(unique(c(unique(stormdmg$CROPDMGEXP),unique(stormdmg$PROPDMGEXP))))
## Only consider damanges in either thousands, millions or billions
validDamage<-c("K","k","M","m","B","b")
hdmg<-filter(stormdmg,PROPDMGEXP %in% validDamage | CROPDMGEXP %in% validDamage)
#expvals<-as.vector(sort(unique(c(unique(hdmg$CROPDMGEXP),unique(hdmg$PROPDMGEXP)))))

if(any(is.na(hdmg$PROPDMGEXP))) hdmg[which(is.na(hdmg$PROPDMGEXP)),]$PROPDMGEXP=0
hdmg$PROPDMGEXP<-gsub("[Kk]",as.character(10^3),hdmg$PROPDMGEXP)
hdmg$PROPDMGEXP<-gsub("[Mm]",as.character(10^6),hdmg$PROPDMGEXP)
hdmg$PROPDMGEXP<-gsub("[Bb]",as.character(10^9),hdmg$PROPDMGEXP)
# There are still some rows with bad values, which are 5 and 3 - replace this with 0
hdmg$PROPDMGEXP<-gsub("5",as.character(0),hdmg$PROPDMGEXP)
hdmg$PROPDMGEXP<-gsub("3",as.character(0),hdmg$PROPDMGEXP)

if(any(is.na(hdmg$CROPDMGEXP))) hdmg[which(is.na(hdmg$CROPDMGEXP)),]$CROPDMGEXP=0
hdmg$CROPDMGEXP<-gsub("[Kk]",as.character(10^3),hdmg$CROPDMGEXP)
hdmg$CROPDMGEXP<-gsub("[Mm]",as.character(10^6),hdmg$CROPDMGEXP)
hdmg$CROPDMGEXP<-gsub("[Bb]",as.character(10^9),hdmg$CROPDMGEXP)
# There are still some rows with ? in CROPDMGEXP - replace this with 0
hdmg$CROPDMGEXP<-gsub("\\?",as.character(0),hdmg$CROPDMGEXP)

#Lets calculate the numbers in dollars and the total damage per row
hdmg<-mutate(hdmg,prop=PROPDMG*as.numeric(PROPDMGEXP),crop=CROPDMG*as.numeric(CROPDMGEXP),total=crop+prop)
# Now lets summarize and order by TotalDamage Desc
aggdmg<-hdmg%>%group_by(EVTYPE)%>%
  summarize(PropertyDamage=sum(prop),CropDamage=sum(crop),TotalDamage=sum(total))%>%arrange(desc(TotalDamage))
#Take the top six
aggdmg<-aggdmg[1:6,]
#Now we don't need the total as we are done sorting
aggdmg$TotalDamage<-NULL
#Lets melt the data together for plotting
dmg<-melt(data = aggdmg, id.vars = c("EVTYPE"), na.rm = T)
#qplot(EVTYPE,value/10^6,data=dmg,facets=variable~.,color=EVTYPE,ylab="Value in Millions",xlab="EventType")
ggplot(dmg,aes(x=EVTYPE,y=value/10^6,fill=factor(variable)))+geom_bar(position="dodge",stat="identity")+
  ggtitle("Storm Events causing Top 6 Crop damage") + labs(y="Damage in Millions")
