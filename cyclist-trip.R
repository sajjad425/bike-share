#loading all the necessary packages
install.packages("tidyverse")
library(tidyverse)

install.packages("lubridate")
library(lubridate)

install.packages("ggplot2")
library(ggplot2)

setwd("C:/Users/anonymous/Downloads/Data Analyst/Google Data/Google Case Study/Cyclist Ride CSV")
getwd()

install.packages("rmarkdown")
install.packages("dplyr")
library(dplyr)

# STEP 1: COLLECT DATA
# Upload Divvy datasets (csv files) here
july_22 <- read.csv("C:/Users/anonymous/Downloads/Data Analyst/Google Data/Google Case Study/Cyclist Ride CSV/202207-divvy-tripdata.csv")
august_22 <- read.csv("C:/Users/anonymous/Downloads/Data Analyst/Google Data/Google Case Study/Cyclist Ride CSV/202208-divvy-tripdata.csv")
september_22 <- read.csv("C:/Users/anonymous/Downloads/Data Analyst/Google Data/Google Case Study/Cyclist Ride CSV/202209-divvy-publictripdata.csv")
october_22 <- read.csv("C:/Users/anonymous/Downloads/Data Analyst/Google Data/Google Case Study/Cyclist Ride CSV/202210-divvy-tripdata.csv")
november_22 <- read.csv("C:/Users/anonymous/Downloads/Data Analyst/Google Data/Google Case Study/Cyclist Ride CSV/202211-divvy-tripdata.csv")
december_22 <- read.csv("C:/Users/anonymous/Downloads/Data Analyst/Google Data/Google Case Study/Cyclist Ride CSV/202212-divvy-tripdata.csv")
january_23 <- read.csv("C:/Users/anonymous/Downloads/Data Analyst/Google Data/Google Case Study/Cyclist Ride CSV/202301-divvy-tripdata.csv")
febuary_23 <- read.csv("C:/Users/anonymous/Downloads/Data Analyst/Google Data/Google Case Study/Cyclist Ride CSV/202302-divvy-tripdata.csv")
march_23 <- read.csv("C:/Users/anonymous/Downloads/Data Analyst/Google Data/Google Case Study/Cyclist Ride CSV/202303-divvy-tripdata.csv")
april_23 <- read.csv("C:/Users/anonymous/Downloads/Data Analyst/Google Data/Google Case Study/Cyclist Ride CSV/202304-divvy-tripdata.csv")
may_23 <- read.csv("C:/Users/anonymous/Downloads/Data Analyst/Google Data/Google Case Study/Cyclist Ride CSV/202305-divvy-tripdata.csv")
june_23 <- read.csv("C:/Users/anonymous/Downloads/Data Analyst/Google Data/Google Case Study/Cyclist Ride CSV/202306-divvy-tripdata.csv")

# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
# Compare column names each of the files
colnames(july_22)
colnames(august_22)
colnames(september_22)
colnames(october_22)
colnames(november_22)
colnames(december_22)
colnames(january_23)
colnames(febuary_23)
colnames(march_23)
colnames(april_23)
colnames(may_23)
colnames(june_23)

# Inspect the dataframes and look for incongruencies
str(july_22)
str(august_22)
str(september_22)
str(october_22)
str(november_22)
str(december_22)
str(january_23)
str(febuary_23)
str(march_23)
str(april_23)
str(may_23)
str(june_23)

# Stack individual quarter's data frames into one big data frame
all_trip <- bind_rows(july_22, august_22, september_22, october_22, november_22, december_22, january_23, febuary_23, march_23, april_23, may_23, june_23)

# Remove lat and long fields as this data.
all_trip <- all_trip %>%
  select(-c(start_lat,start_lng,end_lat,end_lng))

# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
# Inspect the new table that has been created
colnames(all_trip) #List of column names
nrow(all_trip) #How many rows are in data frame?
dim(all_trip)  #Dimensions of the data frame?
head(all_trip) #See the first 6 rows of data frame.
str(all_trip) #See list of columns and data types
summary(all_trip) #Statistical summary of data

#There are a few problems we will need to fix:
# (1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.
# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders ... we will want to make our dataframe consistent with their current nomenclature
# N.B.: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level

#Begin by seeing how many observations fall under each usertype
table(all_trip$member_casual)

#Reassign to the desired values
all_trip <- all_trip %>%
  mutate(member_casual = recode(member_casual,"Subscriber"="member","Customer"="casual"))

#Check to make sure the proper number of observations were reassigned
table(all_trip$member_casual)

#Add columns that list the date, month, day, and year of each ride
all_trip$date <- as.Date(all_trip$started_at)
all_trip$month <- format(as.Date(all_trip$date),"%m")
all_trip$day <- format(as.Date(all_trip$date),"%d")
all_trip$year <- format(as.Date(all_trip$date),"%Y")
all_trip$day_of_week <- format(as.Date(all_trip$date),"%A")

#Add a "ride_length" calculation to all_trips (in seconds)
all_trip$ride_length <- difftime(all_trip$ended_at,all_trip$started_at)

#Inspect the structure of the columns
str(all_trip)

#Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trip$ride_length)
all_trip$ride_length <- as.numeric(as.character(all_trip$ride_length))
is.numeric(all_trip$ride_length)

#Remove "bad" data
#The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
#We will create a new version of the dataframe (v2) since data is being removed
all_trip_v2 <- all_trip[!(all_trip$start_station_name == "HQ QR" | all_trip$ride_length<0),]

# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
# Descriptive analysis on ride_length
mean(all_trip_v2$ride_length)
median(all_trip_v2$ride_length)
max(all_trip_v2$ride_length)
min(all_trip_v2$ride_length)

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trip_v2$ride_length)

# Compare members and casual users
aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual, FUN=mean)
aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual, FUN=median)
aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual, FUN=max)
aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual, FUN=min)

# See the average ride time by each day for members vs casual users
aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual + all_trip_v2$day_of_week, FUN=mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trip_v2$day_of_week <- ordered(all_trip_v2$day_of_week, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual + all_trip_v2$day_of_week, FUN=mean)

# analyze ridership data by type and weekday
all_trip_v2 %>%
  mutate(weekday=wday(started_at,label=TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>%
  arrange(member_casual,weekday)

# Let's visualize the number of rides by rider type
all_trip_v2 %>%
  mutate(weekday=wday(started_at,label=TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual)) + geom_col(position="dodge")

# Let's create a visualization for average duration
all_trip_v2 %>%
  mutate(weekday=wday(started_at,label=TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual)) + geom_col(position="dodge")

# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
counts <- aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual + all_trip_v2$day_of_week, FUN=mean)
#export the data
write.csv(counts, file="C:\\Users\\anonymous\\Downloads\\Data Analyst\\Google Data\\Google Case Study\\Cyclist Ride CSV\\cyclist_trip.csv")
