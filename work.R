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

require(data.table); require(plyr);require(dplyr);
#require("tidyr");require(ggplot2);

# download.file
if(!file.exists("stormdata.csv.bz2")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                destfile = "stormdata.csv.bz2",method="curl")
}

#storm <- read.csv("stormdata.csv.bz2",stringsAsFactors=F)
storm<-read.csv("stormdata.csv.bz2",stringsAsFactors=F,na.strings=c("NA,NAN",""))

#storm<-tbl_df(raw)
#rm(raw)
#storm$EVTYPE=as.factor(storm$EVTYPE)
storm$STATE=as.factor(storm$STATE)
storm$FATALITIES=as.integer(storm$FATALITIES)
storm$INJURIES=as.integer(storm$INJURIES)
#inj<-aggregate(cbind(FATALITIES,INJURIES)~EVTYPE, data=storm,sum)
inj<-storm%>% group_by(EVTYPE)%>%summarize(dead=sum(FATALITIES,na.rm = T),hurt=sum(INJURIES,na.rm=T))
#inj<-arrange(t,desc(dead),desc(hurt))
# find top 10% of fatalities
quantile(inj$dead,prob=c(0.95,0.99))
#Gives us 3 - what event types killed more than 200 people in total?

topinj<-inj[inj$dead>200,]
par("crt"=90,mfrow=c(2,1))
with(topinj,{
  plot(EVTYPE,FATALITIES,main="FATALITIES",col="red")
  plot(EVTYPE,INJURIES,main="INJURIES",COL="yellow")
})

##  ?Convert begintime to posixlt for plotting?
# storm$BGN_DATE<-gsub("0:00:00","",storm$BGN_DATE)
# storm$END_DATE<-gsub("0:00:00","",storm$END_DATE)
# storm<-unite(storm,BGN_DT_TM,BGN_DATE,BGN_TIME, TIME_ZONE, sep=" ", remove=F)
# frmt<-"%m/%d/%Y %H%M"
# storm$BGN_DT_TM<-as.POSIXlt(strptime(storm$BGN_DT_TM,format=frmt))

