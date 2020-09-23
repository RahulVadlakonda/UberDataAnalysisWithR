#########################  Uber Data Analytics  PROJECT    #####################

##########  Step 1  Loading the Libraries
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

######### Step 2 Defining Colors
colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")



########### 3. Reading the Data into their designated variables
apr_data <- read.csv("E:\\Uber Data Analysis\\Uber-dataset\\uber-raw-data-apr14.csv")
may_data <- read.csv("E:\\Uber Data Analysis\\Uber-dataset\\uber-raw-data-may14.csv")
jun_data <- read.csv("E:\\Uber Data Analysis\\Uber-dataset\\uber-raw-data-jun14.csv")
jul_data <- read.csv("E:\\Uber Data Analysis\\Uber-dataset\\uber-raw-data-jul14.csv")
aug_data <- read.csv("E:\\Uber Data Analysis\\Uber-dataset\\uber-raw-data-aug14.csv")
sep_data <- read.csv("E:\\Uber Data Analysis\\Uber-dataset\\uber-raw-data-sep14.csv")


data_2014 <- rbind(apr_data,may_data,jun_data,jul_data,aug_data,sep_data)

data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")

data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format = "%H:%M:%S")

data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)

data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))

data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))




################  PLOTTING THE TRIPS BY THE HOURS IN A DAY   ####################
hour_data <- data_2014 %>%
  group_by(hour) %>%
  dplyr :: summarise(Total = n())
datatable(hour_data)

ggplot(hour_data, aes(hour, Total)) + geom_bar(stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") + theme(legend.position = "none") + scale_y_continuous(labels = comma)


month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr :: summarise(Total = n())
datatable(month_hour)

## Plotting now


ggplot(month_hour, aes(hour, Total, fill = month)) + geom_bar(stat = "identity") +
  ggtitle("Trips By Hour and Month") + scale_y_continuous(labels = comma)


###################3 PLOTTING OF THE DATA BY TRIPS DURING EVERYDAY OF THE MONTH   ##########

day_group <- data_2014 %>% group_by(day) %>% dplyr::summarise(Total = n())
datatable(day_group)

ggplot(day_group, aes(day, Total)) + geom_bar(stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Day") + theme(legend.position = "none") + scale_y_continuous(labels = comma)


day_month_group <- data_2014 %>% group_by(month,day) %>% dplyr::summarise(Total = n())

ggplot(day_month_group, aes(day, Total, fill = month)) + geom_bar(stat = "identity") +
  ggtitle("Trips By Day and Month") + scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)


#########  NUMBER OF TRIPS TAKING PLACE DURING MONTHS IN A YEAR  #######################33
month_group <- data_2014 %>% group_by(month) %>% dplyr::summarise(Total = n())
datatable(month_group)


ggplot(month_group, aes(month, Total, fill = month)) + geom_bar(stat = "identity") +
  ggtitle("Trips By Month") + theme(legend.position = "none") + scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)


month_weekday <- data_2014 %>% group_by(month, dayofweek) %>% dplyr::summarise(Total = n())
ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Trips By Day and Month") + scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)


###########  FINDING OUT THE NUMBER OF TRIPS BY BASES   #########################
ggplot(data_2014, aes(Base)) + geom_bar(fill = "darkred") + scale_y_continuous(labels = comma) + ggtitle("Trips by Bases")

# Trips by Bases and Month
ggplot(data_2014, aes(Base, fill = month)) + geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) + ggtitle ("Trips by Bases and Month") +
  scale_fill_manual(values = colors)


# Trips by Bases and DayofWeek
ggplot(data_2014, aes(Base, fill = dayofweek)) + geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) + ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values = colors)


########### Generating Heatmaps of Trips   ########################

# Heat Map is a data Visualization technique that shows magnitude of phenomenon as color in
# 2 dimesnsions

# We will plot 5 HeatMaps using ggpplots
# 1. We will plot heatmap by Hour and Day
# 2. We will plot Heatmap by Month and Day
# 3. We will plot HeatMap by Month and Day of the Week
# 4. We will plot Heatmap which will delineates Month and Bases
# 5. We will plot Heatmap by bases and day of the week

day_and_hour <- data_2014 %>% group_by(day,hour) %>% dplyr::summarise(Total = n())
datatable(day_and_hour)

ggplot(day_and_hour, aes(day,hour,fill = Total)) + geom_tile(color = "white") + 
  ggtitle("HeatMap by Hour and Day")


ggplot(day_month_group, aes(day,month,fill = Total)) + geom_tile(color = "white") + 
  ggtitle("HeatMap by Month and Day")

ggplot(month_weekday, aes(dayofweek,month,fill = Total)) + geom_tile(color = "white") + 
  ggtitle("HeatMap by Month and Day of Week")


month_base <- data_2014 %>% group_by(Base, month) %>% dplyr::summarise(Total = n())
dayofweek_bases <- data_2014 %>% group_by(Base,dayofweek) %>% dplyr::summarise(Total = n())
ggplot(month_base, aes(Base, month, fill = Total)) + geom_tile(color = "white") + 
  ggtitle("Heat Map by Month and Bases")

ggplot(dayofweek_bases, aes(Base, dayofweek, fill = Total)) + geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")


##############  MAP VISUALIZATION OF RIDES IN NEW YORK CITY    ####################
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004


ggplot(data_2014, aes(x = Lon, y = Lat)) + geom_point(size = 1, color = "blue") +
  scale_x_continuous(limits = c(min_long, max_long)) +
  scale_y_continuous(limits = c(min_lat, max_lat)) +
  theme_map()+ ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR - SEP)")


ggplot(data_2014, aes(x = Lon, y = Lat, color = Base)) + geom_point(size = 1) +
  scale_x_continuous(limits = c(min_long, max_long)) +
  scale_y_continuous(limits = c(min_lat, max_lat)) +
  theme_map()+ ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR - SEP) by BASE")
