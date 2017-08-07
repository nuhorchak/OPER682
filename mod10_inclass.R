library(lubridate)
library(tidyverse)
library(nycflights13)

flights
# 1.
str(flights$time_hour)
#2.
as.POSIXct("2011-08-17 08:00:00") - as.POSIXct("2013-09-30 00:00:00")
# 3. 
flights$weekday <- weekdays(flights$time_hour) %>% 
  factor(levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday",
           "Friday","Saturday"))
# 4. 
flights$fulldate <- date(flights$time_hour)
# 5.
flights <- flights %>%
  mutate(Season = if_else(fulldate %within% interval("2013-03-22","2013-06-21"),"Spring",
                          if_else(fulldate %within% interval("2013-06-22","2013-09-21"),"Summer",
                                  if_else(fulldate %within% interval("2013-09-22","2013-12-21"),"Autumn",
                                          "Winter"))))
flights$Season <- factor(flights$Season,
                         levels = c("Spring","Summer","Autumn","Winter"))

# 6. 

flights[!duplicated(flights$fulldate),] %>%
  mutate(fulldate = if_else(fulldate > as.Date("2013-12-21"),
                            fulldate - years(1),
                            fulldate)) %>%
  group_by(Season) %>%
  summarise(Median = median(fulldate))


# 7.
flights %>%
  group_by(fulldate) %>%
  summarise(nflights = n()) %>%
  ggplot() + geom_line(aes(x = fulldate, y = nflights)) +
  geom_point(aes(x = fulldate[which.max(nflights)], 
                 y = nflights[which.max(nflights)]), 
             color = "red") +
  geom_text(aes(x = fulldate[which.max(nflights)], 
                y = nflights[which.max(nflights)], 
                label = fulldate[which.max(nflights)]), nudge_y = 15) 
  #xlab("Date") + ylab("Number of Flights”)

# 8.
flights %>%
  group_by(weekday) %>%
  summarise(total.flights = n(),
            total.days = length(unique(fulldate))) %>%
  mutate(Avg.Flights = round(total.flights/total.days,2)) %>%
  select(weekday, Avg.Flights)

# 9.
flights %>%
  select(dep_time, weekday) %>%
  ggplot(aes(x = dep_time)) +
  geom_density(stat = "density", fill = "#DDDDDD") +
  facet_wrap(~ weekday, nrow = 3) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
# 10.

flights %>% 
  group_by(weekday) %>% 
  ggplot()+
  geom_boxplot(aes(x=weekday, y=dep_delay)) +
  coord_cartesian(ylim=c(-50,50))

# 11.

flights %>%
  filter(month == 8, day == 7) %>%
  ggplot() + geom_histogram(aes(x = dep_time), bins = 24, 
                            fill = rep(c("black","#939393"),12))

#BONUS
flights %>% 
  filter(year == 2013, month == 7) %>% 
  group_by(carrier) %>% 
  mutate(tot_delay = (arr_delay - dep_delay)) %>% 
  ggplot() +
  geom_col(aes(x=fulldate, y=tot_delay)) + 
  facet_wrap(~carrier)
  