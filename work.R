# NOTE : Events in database start in 1950 end in November 2011.
# In earlier years  generally fewer events recorded,  due to a lack of good records.
# More recent years should be considered more complete.
require(knitr);
#require(data.table); require(plyr);require(tidyr);
require(dplyr);require(ggplot2);require(reshape2);
library(dplyr);library(ggplot2);library(reshape2);
#library(data.table); library(tidyr);library(plyr);
# download.file
if(!file.exists("stormdata.csv.bz2")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                destfile = "stormdata.csv.bz2")
}

## READ raw data from file
storm<-read.csv("stormdata.csv.bz2",stringsAsFactors=F,na.strings=c("NA,NAN",""))
storm<-tbl_df(storm)

#  1) Across the United States, which types of events are most harmful with respect to population health?
## Only select those events with either fatalities or injuries
injuries=subset(storm, FATALITIES>0 | INJURIES>0)
##  Aggregate data by FATALITIES & INJURIES
#inj<-aggregate(cbind(FATALITIES,INJURIES)~EVTYPE, data=storm,sum)
inj<-injuries%>% group_by(EVTYPE)%>%summarize(fatalities=sum(FATALITIES,na.rm = T),hurt=sum(INJURIES,na.rm=T))
#inj<-arrange(t,desc(fatalities),desc(hurt))
# find top 10% of fatalities
quantile(inj$fatalities,prob=seq(0.95,1,by=0.01))
#Gives us 208 as 99th percentile - select event types fatalities more than 200 people in total?
inj<-subset(inj,fatalities>500)
#Examine them
arrange(inj,desc(fatalities),desc(hurt))
#Order the eventtypes as a factor with levels in descending order of fatalities(existing order of data).
# This will make plot ordered by fatalities, on x-axis
inj$EVTYPE=factor(inj$EVTYPE,levels=inj$EVTYPE[order(desc(inj$fatalities))])
## Plot top harmful events
injmelt<-melt(inj,id=c("EVTYPE"),measure.vars=c("fatalities","hurt"))
#qplot(EVTYPE,value,data=injmelt,facets=variable~.,color=EVTYPE)

ggplot(injmelt,aes(x=EVTYPE,y=value,fill=factor(variable)))+geom_bar(position="dodge",stat="identity")+ggtitle(
  "Storm Events causing Top 6 Fatalities")

#  2) Across the United States, which types of events have the greatest economic consequences?
## only select those events with either property or crop damage
stormdmg<-subset(storm,(PROPDMG>0&!is.na(PROPDMGEXP))|(CROPDMG>0&!is.na(CROPDMGEXP)))
## take only columns we are interested in
# stormdmg<-select(stormdmg,EVTYPE,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP,STATE,BGN_DATE,BGN_TIME,TIME_ZONE)
stormdmg<-select(stormdmg,EVTYPE,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)
#Some of the above suffixes appear to be invalid.
sort(unique(c(unique(stormdmg$CROPDMGEXP),unique(stormdmg$PROPDMGEXP))))
## Only consider damanges in either thousands, millions or billions
validDamage<-c("K","k","M","m","B","b")
hdmg<-filter(stormdmg,PROPDMGEXP %in% validDamage | CROPDMGEXP %in% validDamage)

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
# Now lets summarize and order by TotalDamage Desc, then take the top 6
aggdmg<-hdmg%>%group_by(EVTYPE)%>%
  summarize(PropertyDamage=sum(prop),CropDamage=sum(crop),TotalDamage=sum(total))%>%arrange(desc(TotalDamage))
aggdmg<-aggdmg[1:6,]

#Order the eventtypes as a factor with levels in descending order of total damage (existing order of data).
#This will make our plot ordered by fatalities, highest first
aggdmg$EVTYPE=factor(aggdmg$EVTYPE,levels=aggdmg$EVTYPE[order(desc(aggdmg$TotalDamage))])

#Now we don't need the total as we are done sorting
aggdmg$TotalDamage<-NULL
#Lets melt the data together for plotting
dmg<-melt(data = aggdmg, id.vars = c("EVTYPE"), na.rm = T)
#qplot(EVTYPE,value/10^6,data=dmg,facets=variable~.,color=EVTYPE,ylab="Value in Millions",xlab="EventType")
ggplot(dmg,aes(x=EVTYPE,y=value/10^6,fill=factor(variable)))+geom_bar(position="dodge",stat="identity")+
  ggtitle("Storm Events causing Top 6 Crop damage") + labs(y="Damage in Millions")