library(ggplot2)
library(ggplot2movies)
library(tidyverse)

total <- movies

ggplot(data=total) + geom_bar(aes(x=length)) + coord_cartesian(xlim = c(0, 300))

#long = more than 2 SD
STD_length <- sd(total$length)
long_movies <- filter(movies, length > 2*STD_length)
long_movies <- arrange(long_movies, length)

precent_long <- count(long_movies) / count(movies) * 100


short_films <- filter(total, length <= 2* STD_length)
#total_shorts <- count(short_films)
avg_short <- mean(short_films$length, na.rm=T)

total2 <- mutate(total, Short = length < 2*STD_length)
total3 <- mutate(total2, Long = length > 2*STD_length)


avg_movies <- total3 %>%
  filter(Short ==FALSE && Long == FALSE)


