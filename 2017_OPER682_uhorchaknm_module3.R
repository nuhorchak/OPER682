library(readr)
library(readxl)

#read in the first 1000 lines and only 6 columns of flights data

flight_data = read_csv("flights.csv",
         n_max=1000,
         col_types = cols_only(
           year = col_integer(),
           month = col_integer(), 
           day = col_integer(), 
           dep_time = col_integer(), 
           sched_dep_time = col_integer(),
           dep_delay = col_number()
         ))
str(flight_data)
View(flight_data)

#read in a tsv facebook file

facebook_data = read_tsv("facebook.tsv")

#what would you use to read in a table deliminated by "|"?
# read_delim and you would specify the delimiter



