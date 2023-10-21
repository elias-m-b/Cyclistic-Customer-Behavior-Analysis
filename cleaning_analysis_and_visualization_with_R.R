library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data



datap1 <- read.csv("202203-divvy-tripdata.csv", header=TRUE)
datap2 <- read.csv("202204-divvy-tripdata.csv", header=TRUE)
datap3 <- read.csv("202205-divvy-tripdata.csv", header=TRUE)
datap4 <- read.csv("202206-divvy-tripdata.csv", header=TRUE)
datap5 <- read.csv("202207-divvy-tripdata.csv", header=TRUE)
datap6 <- read.csv("202208-divvy-tripdata.csv", header=TRUE)
datap7 <- read.csv("202209-divvy-tripdata.csv", header=TRUE)
datap8 <- read.csv("202210-divvy-tripdata.csv", header=TRUE)
datap9 <- read.csv("202211-divvy-tripdata.csv", header=TRUE)
datap10 <- read.csv("202212-divvy-tripdata.csv", header=TRUE)
datap11 <- read.csv("202301-divvy-tripdata.csv", header=TRUE)
datap12 <- read.csv("202302-divvy-tripdata.csv", header=TRUE)
data <- rbind(datap1, datap2, datap3, datap4, datap5, datap6, datap7, datap8, datap9, datap10, datap11, datap12)
write.csv(data,"merged.csv")
# I READ THE CSV FILE AND I MERGED IT USING 'RBIND' AND WROTE IT INTO A FILE NAMED MERGED.CSV



data = read.csv("merged.csv")
datau <- data[duplicated(data)]
View(datau)
# I CHECKED FOR DUPLICATE ROWS AND DIDN'T FIND ANY




data$month <- month(data$started_at)
data$month_name <- month.abb[data$month]
data$ride_length <- as.numeric(as.POSIXct(data$ended_at)) - as.numeric(as.POSIXct(data$started_at))
data$ride_length <- format(data$ride_length, "%H:%M:%S")
data$day_of_week <- weekdays(as.POSIXct(data$started_at))
# I CREATED 3 MORE COLUMNS BY MANIPULATING THE OTHER COLUMNS. 'AS.POSIXct' FORMAT AN OBJECT AS A DATE



dataCleaned <- subset(data, select = -c(month, started_at, ended_at, start_lat, start_lng, end_lat, end_lng, start_station_id, end_station_id))
row.names(data2ColRemoved) <- NULL
write.csv(dataCleaned, "dataCleaned.csv")
# I REMOVED COLUMNS THAT I WILL NOT USE FOR ANALYSIS


data <- read.csv("dataCleaned.csv")
View(data)

table(data$member_casual)
# casual  member 
# 2365120 3463964 

# # create a vector of values
values <- c(2365120, 3463964)
# 
# # colors
colors <- c("lightseagreen", "salmon")
# 
# # create a vector of labels
labels <- c("casual", "member")
# 
# # create a pie chart
pie(values, labels, col = colors, main = "Casual vs. member")




# # Create a POSIXct object representing a time value
time_value <- as.POSIXct(data$ride_length, format = "%H:%M:%S")
# # Create a POSIXct object representing the origin
origin <- as.POSIXct("00:00:00", format = "%H:%M:%S")
# # Convert the POSIXct object to a numeric value representing the number of seconds since the origin
time_seconds <- as.numeric(difftime(time_value, origin, units = "secs"))
data <- data %>% mutate(ride_length_sec = time_seconds)
write.csv(data, "dataCleaned.csv")



summary(ride_length_sec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.0   344.0   609.0   978.8  1094.0 86399.0




# aggregate(data$ride_length_sec ~ data$member_casual, FUN = mean)
# data$member_casual data$ride_length_sec
# 1             casual            1331.7485
# 2             member             737.8325
# > aggregate(data$ride_length_sec ~ data$member_casual, FUN = median)
# data$member_casual data$ride_length_sec
# 1             casual                  770
# 2             member                  524
# > aggregate(data$ride_length_sec ~ data$member_casual, FUN = max)
# data$member_casual data$ride_length_sec
# 1             casual                86399
# 2             member                86399
# > aggregate(data$ride_length_sec ~ data$member_casual, FUN = min)
# data$member_casual data$ride_length_sec
# 1             casual                    0
# 2             member                    0



aggregate(data$ride_length_sec ~ data$member_casual + data$day_of_week, FUN = mean)
# # Notice that the days of the week are out of order. Let's fix that.
data$day_of_week <- ordered(data$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
# data$member_casual data$day_of_week data$ride_length_sec
# 1              casual           Sunday            1533.7410
# 2              member           Sunday             819.2604
# 3              casual           Monday            1353.8825
# 4              member           Monday             711.1979
# 5              casual          Tuesday            1186.0215
# 6              member          Tuesday             699.1092
# 7              casual        Wednesday            1139.3863
# 8              member        Wednesday             702.7871
# 9              casual         Thursday            1186.0198
# 10             member         Thursday             713.2385
# 11             casual           Friday            1249.8028
# 12             member           Friday             725.7596
# 13             casual         Saturday            1499.3667
# 14             member         Saturday             825.0291





# # analyze ridership data by type and weekday
data %>%
  group_by(member_casual, day_of_week) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration
            ,average_duration = mean(ride_length_sec)) %>% 		# calculates the average duration
  arrange(member_casual, day_of_week)								# sorts
# `summarise()` has grouped output by 'member_casual'. You can override
# using the `.groups` argument.
# # A tibble: 14 × 4
# # Groups:   member_casual [2]
# member_casual day_of_week number_of_rides average_duration
# <chr>         <ord>                 <int>            <dbl>
#   1 casual        Sunday               398647            1534.
# 2 casual        Monday               283327            1354.
# 3 casual        Tuesday              272322            1186.
# 4 casual        Wednesday            279897            1139.
# 5 casual        Thursday             313642            1186.
# 6 casual        Friday               338806            1250.
# 7 casual        Saturday             478479            1499.
# 8 member        Sunday               402869             819.
# 9 member        Monday               488630             711.
# 10 member        Tuesday              546620             699.
# 11 member        Wednesday            542433             703.
# 12 member        Thursday             547400             713.
# 13 member        Friday               481194             726.
# 14 member        Saturday             454818             825.





# # Let's visualize the number of rides by rider type
data %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_sec)) %>%
  arrange(member_casual, day_of_week)  %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "CASUAL VS. MEMBER RIDE NUMBERS ON DIFFERENT DAYS",
       x = "Weekday", y = "Number of rides") +
  theme_bw()





# # # Let's create a visualization for average duration
data %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_sec)) %>%
  arrange(member_casual, day_of_week)  %>%
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
labs(title = "CASUAL VS. MEMBER AVERAGE RIDE DURATION ON DIFFERENT DAYS OF THE WEEK",
       x = "Weekday", y = "Average duration in sec") +
  theme_bw()



# Let's create a visualization for each month
data %>%
    group_by(member_casual, month_name) %>%
    summarise(number_of_rides = n()
              ,average_duration = mean(ride_length_sec)) %>%
    arrange(member_casual, month_name)  %>%
    ggplot(aes(x = month_name, y = number_of_rides, fill = member_casual)) +
    geom_col(position = "dodge") +
    labs(title = "CASUAL VS. MEMBER RIDE NUMBERS, MAR 2022 - FEB 2023",
         x = "Month", y = "Number of rides") +
    theme_bw()





# # Let's create a visualization for ride-able type
data %>%
    group_by(member_casual, rideable_type) %>%
    summarise(number_of_rides = n()) %>%
    arrange(member_casual, rideable_type)  %>%
    ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) +
    geom_col(position = "dodge") +
    labs(title = "CASUAL VS. MEMBER RIDEABLE TYPE",
         x = "Rideable type", y = "Number of rides") +
    theme_bw()





dataCleaned <- subset(data, select = -c(X.1, X))
write.csv(dataCleaned, "dataCleaned.csv")




counts <- aggregate(data$ride_length_sec ~ data$member_casual + data$day_of_week, FUN = mean)
write.csv(counts, "counts.csv")

weekday <- data$day_of_week
write.csv(weekday, "weekday.csv")