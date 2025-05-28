library(tidyverse)
library(janitor)
library(lubridate)
library(dplyr)


read.csv("INDIA-AQI-DATA-2015-2020.csv") %>% 
  clean_names()-> airindia

view(airindia)
#Coordinated Universal Time (UTC)- universal u k is the midpoint of the time zone
#

airindia %>% 
  mutate(year = date %>% year(),
         month = date %>% month(),
         day= date %>% day(),
         week= date %>% week(),
         weekday = date %>% wday(label=T))-> aqidf1

colnames(aqidf1)
unique(aqidf1$city)
aqidf1 %>% 
  pivot_longer(3:14, names_to = "parameter", values_to = "values") ->aqidf2

view(aqidf2)

#yearwise pollutant trends
aqidf2 %>% 
  group_by(year, parameter) %>% 
  summarise(mean_value = mean(values, na.rm = T)) -> aqi_yearwise
print(aqi_yearwise)


ggplot(aqi_yearwise, aes (x=year, y=mean_value))+
  geom_line(color = "maroon")+
  facet_wrap(~parameter,scales = "free_y")+
  labs(title = "Air pollutants data",
       subtitle = "from 2015 to 2024",
       x=NULL,
       y = "Pollutant values",
       caption = "Source: AQI India Dataset")+
  theme_linedraw()-> nanu

ggsave("Air Pollutants Trends.pdf",
       plot = nanu,
       units = "in",
       width = 10,
       height = 6)

#Air quality trends for Bengaluru
#co trends for all cities
#Air quality trends for Bengaluru, Chennai, Mumbai, Hyderabad
#pm2.5 trend for bengaluru for 2015-2020

#Air quality trends for Bengaluru
aqidf2 %>% 
  filter(city == "Bengaluru") %>% 
  group_by(year, parameter) %>% 
  summarise(mean_value = mean(values, na.rm = T))-> bangalore
print(bangalore)

ggplot(bangalore, aes (x=year, y=mean_value))+
  geom_line(color = "darkgreen")+
  facet_wrap(~parameter,scales = "free_y")+
  labs(title = "Bengaluru Air pollutants data",
       subtitle = "from 2015 to 2024",
       x=NULL,
       y = "Pollutant values",
       caption = "Source: AQI India Dataset")+
  theme_linedraw()-> nanu

#co trends for all cities
aqidf2 %>% 
  filter(parameter == "co") %>% 
  group_by(year, city) %>% 
  summarise(mean_value = mean(values, na.rm = T))-> co_trend
print(co_trend)

ggplot(co_trend, aes (x=year, y=mean_value))+
  geom_line(color = "darkgreen")+
  facet_wrap(~city,scales = "free_y")+
  labs(title = "co pollutants in different cities",
       subtitle = "from 2015 to 2024",
       x=NULL,
       y = "Pollutant values",
       caption = "Source: AQI India Dataset")+
  theme_linedraw()-> nanu

##Air quality trends for Bengaluru, Chennai, Mumbai, Hyderabad
aqidf2 %>% 
  filter(city == c("Bengaluru","Chennai", "Mumbai", "Hyderabad")) %>% 
  group_by(year, city, parameter) %>% 
  summarise(mean_value = mean(values, na.rm = T)) %>% 

ggplot(aes (x=year, y=mean_value))+
  geom_line(color = "darkgreen")+
  facet_wrap(~parameter,scales = "free_y")+
  labs(title = "Air quality trends for Bengaluru, Chennai, Mumbai, Hyderabad",
       subtitle = "from 2015 to 2024",
       x=NULL,
       y = "Pollutant values",
       caption = "Source: AQI India Dataset")+
  theme_linedraw()-> nanu

#heat map
aqidf2 %>% 
  filter(parameter == "co") %>% 
  group_by(week, weekday,month) %>% 
  summarise(meanval = mean(values, na.rm = T)) %>% 
  ggplot(aes(x=week,
             y=weekday,
             fill = meanval))+
  geom_tile()+
  facet_wrap(~month, scales="free_x")+
  #scale_fill_gradient(low = "yellow", high = "red")+
  scale_fill_gradientn(colors = c("darkgreen", "yellow", "red"))+
  theme_minimal()+
  labs(title="CO heat map",
       subtitle = "temperature",
       x=NULL,
       y= NULL)






