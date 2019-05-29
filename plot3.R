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
png(filename = "plot3.png",
    width = 480,
    height = 480,
    units = "px")

pwr_consumption_filtered_df %>% 
  gather(key="sub_metering",value="energy", Sub_metering_1,Sub_metering_2,Sub_metering_3) %>%
  ggplot(aes(x=date_time, 
             y=energy, 
             color=sub_metering,
             group=sub_metering)) + 
  geom_line() + 
  scale_x_datetime(date_breaks = "1 day",
                   date_labels = "%A")+
  xlab("")+
  ylab("Energy sub metering")+
  scale_color_discrete(l=40)+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.8,0.8)
  )

dev.off()
