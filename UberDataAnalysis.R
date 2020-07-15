library(ggplot2)
install.packages('ggthemes')
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
install.packages('DT')
library(DT)
library(scales)

#Create vector of colors to be implemented in plots
colors=c('"#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0"')

#Reading the data into their designated variable
apr_data=read.csv('/Users/joohyunyoon/workspace/MachineLearning/UberDataAnalysis/UberDataAnalysis/Uber-dataset/uber-raw-data-apr14.csv')
may_data=read.csv('/Users/joohyunyoon/workspace/MachineLearning/UberDataAnalysis/UberDataAnalysis/Uber-dataset/uber-raw-data-may14.csv')
jun_data=read.csv('/Users/joohyunyoon/workspace/MachineLearning/UberDataAnalysis/UberDataAnalysis/Uber-dataset/uber-raw-data-jun14.csv')
jul_data=read.csv('/Users/joohyunyoon/workspace/MachineLearning/UberDataAnalysis/UberDataAnalysis/Uber-dataset/uber-raw-data-jul14.csv')
aug_data=read.csv('/Users/joohyunyoon/workspace/MachineLearning/UberDataAnalysis/UberDataAnalysis/Uber-dataset/uber-raw-data-aug14.csv')
sep_data=read.csv('/Users/joohyunyoon/workspace/MachineLearning/UberDataAnalysis/UberDataAnalysis/Uber-dataset/uber-raw-data-sep14.csv')
##Stored these in corresponding data frames like apr_data, may_data, etc

View(apr_data)

#Combine all of this data into a single dataframe
data_2014=rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)
View(data_2014)

#Perform the appropriate formatting of Date.Time column
data_2014$Date.Time=as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")
data_2014$Time=format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data_2014$Date.Time=ymd_hms(data_2014$Date.Time)

#Create factors of time objects like day, month, year etc
data_2014$day=factor(day(data_2014$Date.Time))
data_2014$month=factor(month(data_2014$Date.Time,label = T))
data_2014$year=factor(year(data_2014$Date.Time))
data_2014$dayofweek=factor(wday(data_2014$Date.Time,label = T))
data_2014$hour=factor(hour(hms(data_2014$Time)))
data_2014$minute=factor(minute(hms(data_2014$Time)))
data_2014$second=factor(second(hms(data_2014$Time)))

#How many passengers fares throughout the day?
hour_data=data_2014 %>%
    group_by(hour) %>%
        dplyr::summarise(Total=n())
        ##Aggregate the data with dplyr
datatable(hour_data)
##Observable the number of trips are higher in the evening around 5 and 6PM.

#Plotting the trips by the hours in a day
ggplot(hour_data,aes(hour, Total))+
    geom_bar(stat='identity',fill='white', color='orange')+
    ggtitle('Trip Every Hour')+
    theme(legend.position = 'none')+
    scale_y_continuous(labels = comma)

#How many passengers fares throughout the month?
month_hour=data_2014 %>%
    group_by(month, hour) %>%
        dplyr::summarise(Total=n())

ggplot(month_hour,aes(hour, Total, fill=month))+
    geom_bar(stat='identity')+
        ggtitle('Trips by hours and month')+
        scale_y_continuous(labels = comma)

#Plotting data by trips during every day of the month
day_group=data_2014 %>%
    group_by(day) %>%
        dplyr::summarise(Total=n())
datatable(day_group)
##30th of the month had the highest trips(167160) in the year which is mostly contributed by the month of April

ggplot(day_group,aes(day,Total))+
    geom_bar(stat = 'identity',fill='white')+
        ggtitle('Trips every day')+
        theme(legend.position = 'none')+
        scale_y_continuous(labels = comma)

day_month_group=data_2014 %>%
    group_by(month,day) %>%
        dplyr::summarise(Total=n())

ggplot(day_month_group,aes(day,Total,fill=month))+
    geom_bar(stat = 'identity')+
        ggtitle('Trips by day and month')+
        scale_y_continuous(labels = comma)

#Number of trips taking place during months in a year
month_group=data_2014 %>%
    group_by(month) %>%
        dplyr::summarise(Total=n())
datatable(month_group)
##Observable most trips were made during the month of September(1028136)

ggplot(month_group, aes(month, Total, fill=month))+
    geom_bar(stat = 'identity')+
        ggtitle('Trips by month')+
        theme(legend.position = 'none')+
        scale_y_continuous(labels = comma)

month_weekday=data_2014 %>%
    group_by(month,dayofweek) %>%
        dplyr::summarise(Total=n())
datatable(month_weekday)

ggplot(month_weekday,aes(month, Total, fill=dayofweek))+
    geom_bar(stat = 'identity',position = 'dodge')+
        ggtitle('Trips by day and month')+
        scale_y_continuous(labels = comma)
##Obtain visual reports of the number of trips that were made on every day of the week

#Finding out the number of Trips by bases
ggplot(data_2014, aes(Base))+
    geom_bar(fill='darkred')+
        scale_y_continuous(labels = comma)+
        ggtitle('Trips by Bases')
##Plot the number of trips that have been taken by the passengers from each of the bases.
##These are five bases in all out of which observe that B02617 had the highest number of trips.
##Furthermore, this base had the highest number of trips in the month B02617.

ggplot(data_2014,aes(Base, fill=month))+
    geom_bar(position = 'dodge')+
        scale_y_continuous(labels=comma)+
        ggtitle('Trips by Bases and Month')

ggplot(data_2014,aes(Base, fill=dayofweek))+
    geom_bar(position='dodge')+
        scale_y_continuous(labels = comma)+
        ggtitle('Trips by Bases and DayofWeek')
##Thursday observed highest trips in the three bases-B02598, B02617, B02682.

#Create a heatmap visualization by Hour and Day
day_and_hour=data_2014 %>%
    group_by(day,hour) %>%
        dplyr::summarise(Total=n())
datatable(day_and_hour)

ggplot(day_and_hour,aes(day,hour,fill=Total))+
    geom_tile(color='white')+
        ggtitle('Heat map by hour and day')

#Create a heatmap visualization by Month and Day
ggplot(day_month_group,aes(day,month,fill=Total))+
    geom_tile(color='white')+
        ggtitle('Heat map by month and day')
           
#Create a heatmap visualization by Month and Day of the week
ggplot(month_weekday,aes(month,dayofweek,fill=Total))+
    geom_tile(color='white')+
        ggtitle('Heat map by month and day of week')

#A heatmap delineates Month and Bases
month_base=data_2014 %>%
    group_by(Base,month) %>%
        dplyr::summarise(Total=n())

dayofweek_base=data_2014 %>%
    group_by(Base,dayofweek) %>%
        dplyr::summarise(Total=n())

ggplot(month_base,aes(Base,month,fill=Total))+
    geom_tile(color='white')+
        ggtitle('Heat map by month and bases')

#A heatmap delineated Bases and day of the week
ggplot(dayofweek_base,aes(Base,dayofweek,fill=Total))+
    geom_tile(color='white')+
        ggtitle('Heat map by Bases and day of week')

#Creating a map visualization of rides in NY
min_lat=40.5774
max_lat=40.9176
min_long=-74.15
max_long=-73.7004

ggplot(data_2014,aes(x=Lon,y=Lat))+
    geom_point(size=1,color='orange')+
    scale_x_continuous(limits = c(min_long,max_long))+
    scale_y_continuous(limits = c(min_lat,max_lat))+
    theme_map()+
        ggtitle('NYC map based on UBER rides during 2014(Apr-Sep')
##Visualize the rides in NYC by creating a geo-plot

ggplot(data_2014,aes(x=Lon,y=Lat,color=Base))+
    geom_point(size=1)+
    scale_x_continuous(limits=c(min_long,max_long))+
    scale_y_continuous(limits=c(min_lat,max_lat))+
    theme_map()+
        ggtitle('NYC map vased on UBER rides during 2014(Apr-Sep) by Base')
##Visualize the rides in NYC by the bases by creating a geo-plot




