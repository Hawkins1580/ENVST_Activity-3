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

# Downloading package for labelling lines 
# install.packages("ggrepel")
library("ggrepel")


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

# Labeling lines
ggplot(data = North_America, # data for plot
       aes(x = Year, y=Annual_CO2, color=Entity ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", y="CO2 Emissions (in tons)")+ # make axis labels
  theme_classic()



# Start of Homework 
# Make a graph that communicates about emissions from any countries of your choice

# Question #1
Five_High_Emittors <- datCO2[datCO2$Entity == "United States" |
                          datCO2$Entity == "China" |
                          datCO2$Entity == "Russia" |
                          datCO2$Entity == "Japan" |
                          datCO2$Entity == "India", ]

# Making plot
FIRST_Graph <- ggplot(data = Five_High_Emittors, # data for plot
       aes(x = Year, y=Annual_CO2, color=Entity ) )+ # aes, x and y
  geom_line(size = 0.75)+
  scale_y_continuous(labels = label_number(scale = 1/1000000000, accuracy = 1))+
  labs(x="Year", y="CO2 Emissions (in billions of tons)")+ # make axis labels
  ggtitle("The Worlds Top 5 CO2 Emittors:", subtitle = "Will China Ever Reverse its Emissions Trend?")+
  theme_classic()
FIRST_Graph

# Creating second last year variable to attach label to 
FINAL_Year <- max(Five_High_Emittors$Year[Five_High_Emittors$Year == max(Five_High_Emittors$Year)])

# Graph with labels on each line
FIRST_Graph + 
  geom_label_repel(data = filter(Five_High_Emittors, Year == Last_Year),
                   aes(label = Entity),
                   nudge_x = .75,
                   na.rm = TRUE) + 
  theme(legend.position = "none")
  
  
  
# Question #2
# Plot world CO2 emissions on one graph 
ggplot(data = datCO2, # data for plot
       aes(x = Year, y=Annual_CO2))+ # aes, x and y
  geom_line(aes(color="tomato3"),
            show.legend = FALSE)+
  scale_y_continuous(labels = label_number(scale = 1/1000000000, accuracy = 1))+
  labs(x="Year", y="CO2 Emissions (in billions of tons)")+ # make axis labels
  ggtitle("Total World CO2 Emissions")+
  theme_classic()


# Plot world air temperature anomalies on the other graph.
ggplot(data = Climate_Change[Climate_Change$Entity == "World",], 
       aes(x = Day,
           y = temperature_anomaly))+
  geom_line(aes(color="tomato3"),
            show.legend = FALSE)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=1)+
  ggtitle("World Air Temperature Anomalies")+
  labs(x="Year", y="Temperature Anomaly (in Celcuis)")+ # make axis labels
  theme_classic()





# Question #3 - Graph from Our World Data

# Reading in Data
Energy_Source <- read.csv("/cloud/project/Energy_Source.csv")

# Creating United States Dataframe
US_Energy <- Energy_Source[Energy_Source$Entity == "United States",]

# Renaming columns
colnames(US_Energy)
colnames(US_Energy)[4] <- "Wind"
colnames(US_Energy)[5] <- "Hydro"
colnames(US_Energy)[6] <- "Solar"
colnames(US_Energy)[7] <- "Nuclear"
colnames(US_Energy)[8] <- "Biofuels"
colnames(US_Energy)[9] <- "Geothermal, Biomass, Other"
colnames(US_Energy)[10] <- "Coal"
colnames(US_Energy)[11] <- "Oil"
colnames(US_Energy)[12] <- "Gas"

# install.packages("reshape2")
library("reshape2")

US_Energy = subset(US_Energy, select = -c(Entity,Code)) # Deleting entity and code columns
LONG_US_Energy <- melt(US_Energy, id="Year") # sorting each energy sector by year
LABEL_Data <- LONG_US_Energy # duplicating dataframe to try and add labels 
colnames(LABEL_Data)[2] <- "Energy_Source"


GRAPH <- ggplot(data = LABEL_Data, # data for plot
       aes(x = Year,
           y = value,
           color = Energy_Source))+
  geom_line()+
  scale_y_continuous(labels = comma)+ 
  labs(x="Year", y="Energy Consumption (in thousands of TWh)")+ # make axis labels
  ggtitle("United States Energy Consumption:", 
          subtitle = "Are Fossil Fuels Fading Away?")+
  theme_classic()
GRAPH


# Adding labels to each line

# Creating second last year variable to attach label to 
Last_Year <- max(LABEL_Data$Year[LABEL_Data$Year == max(LABEL_Data$Year)])

# Graph with labels on each line
GRAPH + 
  geom_label_repel(data = filter(LABEL_Data, Year == Last_Year),
                   aes(label = Energy_Source),
                   nudge_x = .75,
                   na.rm = TRUE) + 
  theme(legend.position = "none") 





