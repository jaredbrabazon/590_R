library(dplyr)
library(nycflights13)
library(babynames)
library(ggplot2)
library(tidyr)

#Make a data frame tbl for nycflights weather
nyc_flight_weather <- data.frame(nycflights13::weather)


#####find outliers of wind data by visually plotting
ggplot(nyc_flight_weather, aes(time_hour, wind_speed))+
  geom_point()

#removed values that seem unnatural
wind_speed_filtered = nyc_flight_weather %>%
  group_by(wind_speed) %>%
  filter(wind_speed < 200) %>%
  print(wind_speed_filtered)
 
#####Find direction with highest median speed at each airport

#1) this dataframe shows each airport with the median speed for each direction
median_speed_dir = wind_speed_filtered %>%
  group_by(wind_dir, origin) %>%
  summarise(wind_speed_md = median(wind_speed)) %>%
  group_by(origin, wind_dir) %>%
  summarise(wind_speed_high_med = max(wind_speed_md))

#2) This dataframe shows the max speed of the wind direction for each airport, but I cant get the direction to show
median_speed_dir_location = wind_speed_filtered %>%
  group_by(wind_dir, origin) %>%
  summarise(wind_speed_md = median(wind_speed)) %>%
  group_by(origin) %>%
  summarise(wind_speed_high_med = max(wind_speed_md))

#3) the right way to get the highest median speed, corresponding direction, and location
median_speed = wind_speed_filtered %>%
  group_by(wind_dir, origin) %>%
  summarise(wind_speed_md = median(wind_speed)) %>%
  group_by(origin) %>%
  filter(wind_speed_md == max(wind_speed_md))

##EWR is highest at 290,300,320,330 with a speed of 12.65858
## JFK is highest at 290,300,310,330 with a median speed of 14.96014
## LGA is highest at 270 and 290 with a median speed of 13.80936

##### Make a table and a plot of median wind speed by direction, for each airport.
# split to respective airports

#for EWR
EWR = median_speed_dir %>%
  filter(origin == 'EWR')

#for JFK
JFK = median_speed_dir %>%
  filter(origin == 'JFK')

#for LGA
LGA = median_speed_dir %>%
  filter(origin == 'LGA')

#make rose diagram for the wind speeds using ggplot
#EWR plot
ggplot(EWR, aes(wind_dir, wind_speed_high_med))+
  geom_col()+
  coord_polar(theta = "x")+
  xlab('EWR')+
  ylab('Median Wind Speed')
  

#JFK plot
ggplot(JFK, aes(wind_dir, wind_speed_high_med))+
  geom_col()+
  coord_polar(theta = "x")+
  xlab('JFK')+
  ylab('Median Wind Speed')

#LGA plot
ggplot(LGA, aes(wind_dir, wind_speed_high_med))+
  geom_col()+
  coord_polar(theta = "x")+
  xlab('LGA')+
  ylab('Median Wind Speed')

##### Part 2 using nycflights13 flights and airlines
flights <- nycflights13::flights
airlines <- nycflights13::airlines

#join based on common carrier variable, join and retain only rows with matches
nyc_flights <- inner_join(flights,airlines)

#create table with name and median distance from jfk
name_med_dst = nyc_flights %>%
  filter(origin == 'JFK') %>%
  group_by(name) %>%
  summarise(Median_Dst_JFK = median(distance)) %>%
  arrange(desc(Median_Dst_JFK))
  
##### make a wide format data frame that displays the number of flights that leave Newark ("EWR") airport each month, from each airline
wide = nyc_flights %>%
  group_by(month, name) %>%
  summarise(month.2013 = length(month)) %>%
  spread(name, month.2013)

##### Use the babynames dataset
baby_names <- babynames::babynames
  
##### Identify the ten most common male and female names in 2014. Make a plot of their frequency (prop) since 1880. (This may require two separate piped statements).

# make a table with top 10 girl and boy names for 2014
top_ten = baby_names %>%
  filter(year == 2014) %>%
  group_by(sex) %>%
  top_n(10,n) %>%
  select(year, sex, name)

#top ten for girls
top_ten_girl = baby_names %>%
  filter(year == 2014) %>%
  filter(sex == 'F') %>%
  top_n(10,n)

#top ten for girls
top_ten_boy = baby_names %>%
  filter(year == 2014) %>%
  filter(sex == 'M') %>%
  top_n(10,n)

#table to plot name by frequency from 1880 for girls top 10 from 2014
name_plot_girl <- baby_names %>%
  filter(sex == 'F') %>%
  filter(name %in% top_ten_girl$name) %>%
  select(year,sex,name,prop) %>%
  group_by(name)
  
#table to plot name by frequency from 1880 for boys top 10 from 2014
name_plot_boy <- baby_names %>%
  filter(sex == 'M') %>%
  filter(name %in% top_ten_boy$name) %>%
  select(year,sex,name,prop) %>%
  group_by(name)
 
#boys top 10 frequency plot since 1880
ggplot(name_plot_boy, aes(year, prop, color = name))+
  geom_line()

#girls top 10 frequency plot since 1880
ggplot(name_plot_girl, aes(year, prop, color = name))+
  geom_line()

##### Make a single table of the 26th through 29th most common girls names in the year 1896, 1942, and 2016
#Note 2016 doesnt exist in the dataset

#make a table for females in 1896
common_names_1896 <- baby_names %>%
  select(year, sex, name, n) %>%
  filter(sex == 'F') %>%
  filter(year == 1896)
#make a table for females in 1942
common_names_1942 <- baby_names %>%
  select(year, sex, name, n) %>%
  filter(sex == 'F') %>%
  filter(year == 1942)
#make a table for females in 2014 since 2016 isnt on the original dataframe
common_names_2014 <- baby_names %>%
  select(year, sex, name, n) %>%
  filter(sex == 'F') %>%
  filter(year == 2014)

#join the three tables together
common_names_join = full_join(common_names_1896, common_names_1942)
common_names = full_join(common_names_join, common_names_2014)

#chooses the 26-29 most popular names of girls for the three years
names_26_29 = common_names %>%
  group_by(year) %>%
  top_n(29,n) %>%
  top_n(-4,n)
print(names_26_29)
  
#####Write task that involves some of the functions on the Data Wrangling Cheat Sheet and execute it. You may either use your own data or data packages

#make a graph of number of occurrence per year of males with my name
my_name = baby_names %>%
  filter(name == 'Jared') %>%
  filter(sex == 'M')

ggplot(my_name, aes(year,n))+
  geom_bar(stat = "identity")
  
  
