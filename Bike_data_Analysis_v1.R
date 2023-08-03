##installing and loading packages
install.packages("tidyverse")
install.packages("janitor")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(janitor)
##importing and combing data
tripdata01<- read_csv("C/202101-divvy-tripdata/202101-divvy-tripdata.csv")
tripdata02<- read_csv("Data Analytics/Case studies/Case study 1_Bike data/202102-divvy-tripdata/202102-divvy-tripdata.csv")
tripdata03<- read_csv("Data Analytics/Case studies/Case study 1_Bike data/202103-divvy-tripdata/202103-divvy-tripdata.csv")
tripdata04<- read_csv("Data Analytics/Case studies/Case study 1_Bike data/202104-divvy-tripdata/202104-divvy-tripdata.csv")
tripdata05<- read_csv("Data Analytics/Case studies/Case study 1_Bike data/202105-divvy-tripdata/202105-divvy-tripdata.csv")
tripdata06<- read_csv("Data Analytics/Case studies/Case study 1_Bike data/202106-divvy-tripdata/202106-divvy-tripdata.csv")
tripdata07<- read_csv("Data Analytics/Case studies/Case study 1_Bike data/202107-divvy-tripdata/202107-divvy-tripdata.csv")
tripdata08<- read_csv("Data Analytics/Case studies/Case study 1_Bike data/202108-divvy-tripdata/202108-divvy-tripdata.csv")
tripdata09<- read_csv("Data Analytics/Case studies/Case study 1_Bike data/202109-divvy-tripdata/202109-divvy-tripdata.csv")
tripdata10<-read_csv("Data Analytics/Case studies/Case study 1_Bike data/202110-divvy-tripdata/202110-divvy-tripdata.csv")
tripdata11<-read_csv("Data Analytics/Case studies/Case study 1_Bike data/202111-divvy-tripdata/202111-divvy-tripdata.csv")
tripdata12<-read_csv("Data Analytics/Case studies/Case study 1_Bike data/202112-divvy-tripdata/202112-divvy-tripdata.csv")

##comparing columns
colnames(tripdata01)
colnames(tripdata02)
janitor::compare_df_cols(tripdata01,tripdata02,tripdata03,tripdata04,tripdata05,tripdata06,tripdata07,tripdata08,tripdata09,tripdata10,tripdata11,tripdata12)

tripdata<-rbind(tripdata01,tripdata02,tripdata03,tripdata04,tripdata05,tripdata06,tripdata07,tripdata08,tripdata09,tripdata10,tripdata11,tripdata12)
tibble(tripdata)
str(tripdata)
table(tripdata$member_casual)

tripdata$date <- as.Date(tripdata$started_at) 
tripdata$month <- format(as.Date(tripdata$date), "%m")
tripdata$day <- format(as.Date(tripdata$date), "%d")
tripdata$year <- format(as.Date(tripdata$date), "%Y")
tripdata$day_of_week <- format(as.Date(tripdata$date), "%A")

tripdata$ride_length<-difftime(tripdata$ended_at,tripdata$started_at)
str(tripdata)
table(tripdata$start_station_name)
tripdata_v2<-tripdata %>% filter(-ride_length<0)

##statistics
mean(tripdata_v2$ride_length)
median(tripdata_v2$ride_length)
min(tripdata_v2$ride_length)
max(tripdata_v2$ride_length)

aggregate(tripdata_v2$ride_length~tripdata_v2$member_casual,FUN=mean)
aggregate(tripdata_v2$ride_length~tripdata_v2$member_casual,FUN=median)
aggregate(tripdata_v2$ride_length~tripdata_v2$member_casual,FUN=min)
aggregate(tripdata_v2$ride_length~tripdata_v2$member_casual,FUN=max)

aggregate(tripdata_v2$ride_length~tripdata_v2$member_casual+tripdata_v2$day_of_week,FUN=mean)
tripdata_v2$day_of_week<-ordered(tripdata_v2$day_of_week,levels=c("Sunday","MOnday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
aggregate(tripdata_v2$ride_length~tripdata_v2$member_casual+tripdata_v2$day_of_week,FUN=mean)

tripdata_v2 %>% 
mutate(weekday=wday(started_at,label=TRUE)) %>% 
group_by(member_casual,weekday) %>% 
  summarise(number_of_ride=n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday)

##visualise
tripdata_v2 %>% 
  mutate(weekday=wday(started_at,label=TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_ride=n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday,y=number_of_ride,fill=member_casual))+geom_col(position="dodge") +
  labs(title="Comparing Number of rides on weekdays by User Type")

tripdata_v2 %>% 
  mutate(weekday=wday(started_at,label=TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_ride=n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday,y=average_duration,fill=member_casual))+geom_col(position="dodge")+
labs(title="Comparing Average duration of rides on Weekdays by User Type")

counts <- aggregate(tripdata_v2$ride_length ~ tripdata_v2$member_casual + tripdata_v2$day_of_week, FUN = mean)
write.csv(counts, file = '~/Data Analytics/Case studies/Case study 1_Bike data/avg_ride_length.csv')



