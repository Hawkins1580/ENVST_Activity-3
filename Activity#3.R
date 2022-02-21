# Activity & Homework #3


# In-class Prompts

# read in data
# cloud is always lowercase
datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")
Climate_Change <- read.csv("/cloud/project/activity03/climate-change.csv")
Annual_CO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")

# loading packages
# install.packages(c("dplyr", "ggplot2"))
# install.packages("lubridate")
# install.packages("scales")
library("scales")
library(dplyr)
library(ggplot2)
library(lubridate)


# Prompt #1
# Make a plot of air temperature anomalies in the Northern and Southern Hemisphere in base R and in ggplot2.

# Parsing the date for the Climate Change data frame
Climate_Change$Day <- ymd(Climate_Change$Day)

# Separating hemispheres
Northern_Hemisphere <- Climate_Change[Climate_Change$Entity == "Northern Hemisphere",]
Southern_Hemisphere <- Climate_Change[Climate_Change$Entity == "Southern Hemisphere",]


# Making plot with base R for Nothern & Southern Hemisphere

# Adding Northern Hemisphere
plot(Northern_Hemisphere$Day, # x data
       Northern_Hemisphere$temperature_anomaly,
       type = "l",
       col = "goldenrod3",
     ylab = "Temperature Anomaly (in Celcuis)", #y axis label
     xlab = "Year" # axis label
)

# Adding Southern Hemisphere
points(Southern_Hemisphere$Day, # x data
       Southern_Hemisphere$temperature_anomaly,
       type = "l",
       col = "tomato3") 


# Making plot with ggplot2 without World
ggplot(data = Climate_Change, aes(x = Day, 
                                  y = temperature_anomaly, 
                                  color = Entity))+
  geom_line()+
  labs(x="Year", y="Temperature Anomaly (in Celcuis)") # make axis labels
  

# Making plot with ggplot2 without World
ggplot(data = Climate_Change[Climate_Change$Entity != "World",], aes(x = Day, 
                                    y = temperature_anomaly, 
                                    color = Entity))+
geom_line()+
labs(x="Year", y="Temperature Anomaly (in Celcuis)") # make axis labels
  



# Prompt #2
# Plot the total all time emissions for the United States, Mexico, and Canada

# Renaming column
colnames(datCO2)
colnames(datCO2)[4] <- "Annual_CO2"

North_America <- datCO2[datCO2$Entity == "United States" |
                   datCO2$Entity == "Canada" |
                   datCO2$Entity == "Mexico", ]

ggplot(data = North_America, # data for plot
       aes(x = Year, y=Annual_CO2, color=Entity ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", y="CO2 Emissions (in tons)")+ # make axis labels
  theme_classic()



# Start of Homework 
# Make a graph that communicates about emissions from any countries of your choice


Five_High_Emittors <- datCO2[datCO2$Entity == "United States" |
                          datCO2$Entity == "China" |
                          datCO2$Entity == "Russia" |
                          datCO2$Entity == "Japan" |
                          datCO2$Entity == "India", ]

# Making plot
ggplot(data = Five_High_Emittors, # data for plot
       aes(x = Year, y=Annual_CO2, color=Entity ) )+ # aes, x and y
  geom_line()+
  scale_y_continuous(labels = label_number(scale = 1/1000000000))+
  labs(x="Year", y="CO2 Emissions (in billions of tons)")+ # make axis labels
  theme_classic()




