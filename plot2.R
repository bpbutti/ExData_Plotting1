library(tidyverse)
library(lubridate)

Sys.setlocale("LC_ALL","English")


#Load the data
pwr_consumption_df <- read_delim(file="household_power_consumption.txt", delim=";")

#Filters data for the dates we are interested
dates_used <- c("2007-02-01", "2007-02-02")
pwr_consumption_filtered_df <- pwr_consumption_df %>%
  mutate(Date = lubridate::dmy(Date)) %>%
  filter(Date %in% ymd(dates_used)) %>%
  unite(date_time, c("Date", "Time"), sep = " ") %>%
  mutate(date_time = lubridate::ymd_hms(date_time))


#Makes and saves plot to file
png(filename = "plot2.png",
    width = 480,
    height = 480,
    units = "px")

pwr_consumption_filtered_df %>% 
  ggplot(aes(x=date_time, y=Global_active_power)) + 
  geom_line() + 
  scale_x_datetime(date_breaks = "1 day",
                   date_labels = "%A")+
  xlab("")+
  ylab("Global active power (Kilowatts)")+
  theme_classic()
  
dev.off()
