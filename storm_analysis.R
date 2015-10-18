# Across the United States, which types of events (as indicated in the EVTYPE variable) 
# are most harmful with respect to population health?

library(dplyr)
library(ggplot2)
library(reshape2)

# Loading and inspecting the dataset
dat <- read.csv('repdata-data-StormData.csv.bz2')
dat <- tbl_df(dat)
dim(dat)
names(dat)
with(dat, sum(is.na(FATALITIES)))
with(dat, sum(is.na(INJURIES)))
with(dat, sum(is.na(PROPDMG)))
with(dat, sum(is.na(CROPDMG)))
n_distinct(dat$EVTYPE)

# Find events with the top 10 casualties (average casualty is used to break tie, if any)
casualty_top10 <- dat %>%
        group_by(EVTYPE) %>%
        summarise(casualty.total = sum(FATALITIES, INJURIES), casualty.mean = sum(FATALITIES, INJURIES)/n(),
                  Fatality = sum(FATALITIES), Injury = sum(INJURIES), event.count = n()) %>%
        arrange(desc(casualty.total), desc(casualty.mean)) %>%
        head(10)
options(dplyr.width = Inf)
casualty_top10        
        
# Plotting barplot for the events
casualty_top10$EVTYPE <- with(casualty_top10, factor(EVTYPE, as.character(EVTYPE)))
casualty_top10 <- melt(casualty_top10, id.vars = c("EVTYPE", "casualty.total", "casualty.mean", "event.count"), 
                      variable.name = "Casualty")
g1 <- ggplot(data = casualty_top10, aes(EVTYPE, value/1000, fill = Casualty)) 
g1 + geom_bar(stat = "identity") +
        labs(x = "Event Type", y = "No of Casualties (in thousands)", title = "Top 10 Event Types in Casualty Count from 1950-2011") +
        scale_y_continuous(breaks = seq(0,100,10)) +
        coord_flip()
        

#Across the United States, which types of events have the greatest economic consequences?
levels(dat$PROPDMGEXP)
levels(dat$CROPDMGEXP)
# https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html

# Extracting only property and crop damages that are in billions and millions, then find the top 10 in total damages
levels(dat$PROPDMGEXP)
levels(dat$CROPDMGEXP)
expTable <- data.frame(exp = c("B", "M", "m"), value = c(9, 6, 6))
economic_top10 <- dat %>% 
        filter(PROPDMGEXP %in% expTable$exp | CROPDMGEXP %in% expTable$exp) %>%
        mutate(prop_expValue = expTable$value[match(PROPDMGEXP, expTable$exp)],
               crop_expValue = expTable$value[match(CROPDMGEXP, expTable$exp)],
               Property = PROPDMG * 10^prop_expValue, Crop = CROPDMG * 10^crop_expValue) %>%
        group_by(EVTYPE) %>%
        summarise(dmg.total = sum(Property, Crop, na.rm = TRUE), dmg.mean = dmg.total/n(),
                  Property = sum(Property, na.rm = TRUE), Crop = sum(Crop, na.rm = TRUE), event.count = n()) %>%
        arrange(desc(dmg.total)) %>%
        head(10)
economic_top10       
        
# Plotting barplot for the events
economic_top10$EVTYPE <- with(economic_top10, factor(EVTYPE, as.character(EVTYPE)))
economic_top10 <- melt(economic_top10, id.vars = c("EVTYPE", "dmg.total","dmg.mean", "event.count"), 
                      variable.name = "Economy")
g2 <- ggplot(data = economic_top10, aes(EVTYPE, value/1000000000, fill = Economy)) 
g2 + geom_bar(stat = "identity") +
        labs(x = "Event Type", y = "USD (in billions)", title = "Top 10 Event Types in Economic Consequences from 1950-2011") +
        scale_y_continuous(breaks = seq(0,150,20)) +
        coord_flip()

# Most harmful event type
intersect(casualty_top10$EVTYPE, economic_top10$EVTYPE)
