load("~/OPER682/Module7/data/tidy_data.RData")

library(tidyverse)
cases <- cases %>%
  gather(Year, m, 2:4)

bomber_wide <- readRDS("~/OPER682/Module7/data/bomber_wide.rds") %>%
  gather(yr, gals, -c(Type:MD))

bomber_long <- readRDS("~/OPER682/Module7/data/bomber_long.rds") %>%
  spread(Output, Value)

bomber_combined <- read_rds("~/OPER682/Module7/data/bomber_combined.rds") %>%
  separate(AC, into=c("Type", "MD"), sep = " ")

bomber_prefix <- read_rds("~/OPER682/Module7/data/bomber_prefix.rds") %>%
  unite(MD, prefix, number, sep="-")

read_rds("~/OPER682/Module7/data/bomber_mess.rds") %>%
  unite(MD, c("prefix", "number"), sep="-") %>%
  separate(Metric, into=c("FY", "Output"), sep = "_") %>%
  as.tibble() %>%
  ggplot() + 
  geom_line(aes(x=FY, y=Value, group = MD, linetype = MD)) + 
  facet_wrap(~Output, ncol=1, scales = "free_y") 
  
