##----------------SYNOPSIS-------------------
##----------------DOWNLOAD(INPUT)DATA--------
setwd("C:/Users/Numair Shaikh/Desktop/R-Coursera")
if(!"data" %in% ls()) {
  data <- read.csv(bzfile("repdata_data_StormData.csv.bz2"), header=TRUE)
}
##--------------------------------------------
##-------------SETUP-LIBRARIES----------------
library(dplyr)
library(ggplot2)
library(lubridate)
##-------------BASIC ANALYSIS----------------
str(data)
head(data)
names(data)
tail(data)
##---------subset Required Data--------
storm <- data[, c("BGN_DATE", "COUNTY", "STATE", "END_DATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
head(storm,3)
tail(storm,3)
##!!See that not much data is avl is early years tell to the audience
storm$year <- as.Date(storm$BGN_DATE, format = "%m/%d/%Y %H:%M:%S")
storm$year <- year(storm$year)
table(storm$year)
storm <- subset(storm, storm$year>1970)
##------------------------CLEANING-DATA--------------------------
storm.damage <- storm[, c("EVTYPE", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
storm.damage$PROPDMGEXP[!grepl("K|M|B",storm.damage$PROPDMGEXP)] <- 0
storm.damage$PROPDMGEXP[grepl("K",storm.damage$PROPDMGEXP)] <- 1000
storm.damage$PROPDMGEXP[grepl("M",storm.damage$PROPDMGEXP)] <- 1000000
storm.damage$PROPDMGEXP[grepl("B",storm.damage$PROPDMGEXP)] <- 1000000000
storm.damage$CROPDMGEXP[!grepl("K|M|B",storm.damage$CROPDMGEXP)] <- 0
storm.damage$CROPDMGEXP[grepl("K",storm.damage$CROPDMGEXP)] <- 1000
storm.damage$CROPDMGEXP[grepl("M",storm.damage$CROPDMGEXP)] <- 1000000
storm.damage$CROPDMGEXP[grepl("M",storm.damage$CROPDMGEXP)] <- 1000000000
storm.damage$totalPropDmg <- storm.damage$PROPDMG*as.numeric(storm.damage$PROPDMGEXP)
storm.damage$totalCropDmg <- storm.damage$CROPDMG*as.numeric(storm.damage$CROPDMGEXP)
aggr <- aggregate(FATALITIES~EVTYPE, data=storm,FUN=sum)
aggr1 <- aggregate(INJURIES~EVTYPE, data=storm, FUN=sum)
aggr <- merge(aggr, aggr1, by.y="EVTYPE")
storm.final <- subset(aggr, FATALITIES>0 & INJURIES>0)
dmgaggr <- aggregate(totalPropDmg~EVTYPE, data=storm.damage, FUN=sum)
dmgaggr1 <- aggregate(totalCropDmg~EVTYPE, data=storm.damage, FUN=sum)
dmgaggr <- merge(dmgaggr, dmgaggr1, by.y="EVTYPE")
storm.final <- merge(storm.final, dmgaggr, by.y="EVTYPE")
storm.final$TYPE <- "OTHER"
storm.final$TYPE[grepl("AVALANC", storm.final$EVTYPE, ignore.case = TRUE)] <- "AVALANCHE"
storm.final$TYPE[grepl("BLIZZARD", storm.final$EVTYPE, ignore.case = TRUE)] <- "BLIZZARD"
storm.final$TYPE[grepl("SNOW|COLD|FREEZE|ICE", storm.final$EVTYPE, ignore.case = TRUE)] <- "SNOW"
storm.final$TYPE[grepl("FOG", storm.final$EVTYPE, ignore.case = TRUE)] <- "FOG"
storm.final$TYPE[grepl("STORM", storm.final$EVTYPE, ignore.case = TRUE)] <- "STORM"
storm.final$TYPE[grepl("HEAT", storm.final$EVTYPE, ignore.case = TRUE)] <- "HEAT"
storm.final$TYPE[grepl("FLOOD", storm.final$EVTYPE, ignore.case = TRUE)] <- "FLOOD"
storm.final$TYPE[grepl("TORNADO", storm.final$EVTYPE, ignore.case = TRUE)] <- "TORNADO"
storm.final$TYPE[grepl("CURRENT", storm.final$EVTYPE, ignore.case = TRUE)] <- "CURRENTS"
storm.final$TYPE[grepl("SLIDE", storm.final$EVTYPE, ignore.case = TRUE)] <- "LANDSLIDE"
storm.final$TYPE[grepl("HURRICANE", storm.final$EVTYPE, ignore.case = TRUE)] <- "HURRICANE"
storm.final$TYPE[grepl("LIGHTNING", storm.final$EVTYPE, ignore.case = TRUE)] <- "LIGHTNING"
storm.final <- na.omit(storm.final)
##-----PLOT-DATA-------------
fatalities <- aggregate(FATALITIES~TYPE, data=storm.final, FUN=sum)
injuries <- aggregate(INJURIES~TYPE, data=storm.final, FUN=sum)
CropDmg <- aggregate(totalCropDmg~TYPE, data=storm.final, FUN=sum)
PropDmg <- aggregate(totalPropDmg~TYPE, data=storm.final, FUN=sum)
##ggplot(fatalities, aes(TYPE, FATALITIES, label=FATALITIES)) + geom_bar(stat="identity") + coord_flip()
par(mfrow=c(2,1), mar=rep(2,4))
ggplot(fatalities, aes(TYPE, FATALITIES, label=FATALITIES)) + geom_bar(stat="identity") + coord_flip() + xlab("Event") + ylab("Fatalities") + ggtitle("Fatalities caused due to different weather events")
ggplot(injuries, aes(TYPE, INJURIES, label=INJURIES)) + geom_bar(stat="identity") + coord_flip() + + xlab("Event") + ylab("Injuries") + ggtitle("Injuries caused due to different weather events")