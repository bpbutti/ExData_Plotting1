library(tidyverse)
library(lubridate)

#Load the data
pwr_consumption_df <- read_delim(file="household_power_consumption.txt", delim=";")

#Filters data for the dates we are interested
dates_used <- c("2007-02-01", "2007-02-02")
pwr_consumption_filtered_df <- pwr_consumption_df %>%
  mutate(Date = lubridate::dmy(Date)) %>%
  filter(Date %in% ymd(dates_used))


#Makes and saves plot to file
png(filename = "plot1.png",
    width = 480,
    height = 480,
    units = "px")

pwr_consumption_filtered_df %>% 
  pull(Global_active_power) %>% 
  hist(breaks=15, 
       main="Global Active Power", 
       xlab = "Global active power (kilowatts)",
       col="red")

dev.off()
