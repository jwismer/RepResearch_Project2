

# get the data file
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

# Load and preprocess the data
# assume zip file has been downloaded already
filename <- "./repdata-data-StormData.csv.bz2"

#download unzip the file if found
if (!file.exists(filename)) {
    download.file(url, filename)
}

#conditionally load the dataset
if (!exists("stormData")) {
    stormData <- read.csv(bzfile(filename))
}

#subset to just the factors needed for the analysis
eventImpact <- stormData[ , c("EVTYPE", "FATALITIES", "INJURIES", 
                               "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

#aggregate factors associated the harmful impact, by event type
fatality_agg <- aggregate(FATALITIES ~ EVTYPE, data=eventImpact, FUN="sum")
injury_agg   <- aggregate(INJURIES   ~ EVTYPE, data=eventImpact, FUN="sum")

#aggregate factors associated the highest economic impact, by event type
propdmg_agg <- aggregate(PROPDMG ~ EVTYPE, data=eventImpact, FUN="sum")
cropdmg_agg   <- aggregate(CROPDMG   ~ EVTYPE, data=eventImpact, FUN="sum")

#max fatalities and associated event type
subset(fatality_agg, FATALITIES == max(FATALITIES))

#top 6 fatal event types
fatality_agg[head(order(fatality_agg$FATALITIES, decreasing = TRUE)), ]

par(mfrow = c(2,1))
barplot(fatality_agg[head(order(fatality_agg$FATALITIES, decreasing = TRUE)), "FATALITIES"], 
        names.arg=fatality_agg[head(order(fatality_agg$FATALITIES, decreasing = TRUE)), "EVTYPE"],
        main="Total Fatalities by Weather Event Type: Top 6 Most Fatal", ylab="Total Fatalities")
barplot(injury_agg[head(order(injury_agg$INJURIES, decreasing = TRUE)), "INJURIES"], 
        names.arg=injury_agg[head(order(injury_agg$INJURIES, decreasing = TRUE)), "EVTYPE"],
        main="Total Injuries by Weather Event Type: Top 6 Most Injurous", ylab="Total Injuries")


#max injuries and associated event
subset(injury_agg, INJURIES == max(INJURIES))

#top 6 more injurous event types
injury_agg[head(order(injury_agg$INJURIES, decreasing = TRUE)), ]

#top 6 property damaging event types
propdmg_agg[head(order(propdmg_agg$PROPDMG, decreasing = TRUE)), ]

#top 6 crop damaging event types
cropdmg_agg[head(order(cropdmg_agg$CROPDMG, decreasing = TRUE)), ]
