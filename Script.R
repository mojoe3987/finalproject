install.packages("patchwork")
library(patchwork)
library(plyr)
library(ggplot2)
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url,"C:/Users/joerling/Dropbox/Sonstiges/Learning_Data_Science/Reproducible Research/Week 4/Project/StormData.csv")
df <- read.csv("C:/Users/joerling/Dropbox/Sonstiges/Learning_Data_Science/Reproducible Research/Week 4/Project/StormData.csv")

df$EVTYPE <- as.factor(df$EVTYPE)
str(df)
head(df)

#Subset data: Top 10 in terms of fatality
top_fatalities <- aggregate(FATALITIES ~ EVTYPE, df, FUN = sum)
top_fatalities <- arrange(top_fatalities,desc(top_fatalities$FATALITIES))
top_fatalities <- top_fatalities[1:10,]

#Subset data: Top 10 in terms of injuries
top_injuries <- aggregate(INJURIES ~ EVTYPE, df, FUN = sum)
top_injuries <- arrange(top_injuries,desc(top_injuries$INJURIES))
top_injuries <- top_injuries[1:10,]

#Subset data: Top 10 in terms of Property Damage
top_damage <- aggregate(PROPDMG ~ EVTYPE, df, FUN = sum)
top_damage <- arrange(top_damage,desc(top_damage$PROPDMG))
top_damage <- top_damage[1:10,]

#Generate plots for fatalities and injuries; finally combine plots to one overall harm plot
plot_fatalities <- ggplot(data = top_fatalities, aes(x = EVTYPE, y = FATALITIES))+
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Fatalities", x = "Event Type", y = "Fatalities") +
  theme(axis.text.x = element_text(angle = 90))
plot_fatalities

plot_injuries <- ggplot(data = top_injuries, aes(x = EVTYPE, y = INJURIES))+
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Injuries", x = "Event Type", y = "Injuries") +
  theme(axis.text.x = element_text(angle = 90))
plot_injuries

plot_people_harm <- plot_fatalities + plot_injuries
plot_people_harm

#Plot economic consequences

plot_damage <- ggplot(data = top_damage, aes(x = EVTYPE, y = PROPDMG))+
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Property Damage", x = "Event Type", y = "Damage in $") +
  theme(axis.text.x = element_text(angle = 90))
plot_damage
