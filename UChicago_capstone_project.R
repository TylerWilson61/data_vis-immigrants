##Tyler wilson
## Quant Analysis Capstone
## 7 Aug 2018

library(tidyverse)
library(dplyr)
library(readr)
library(broom)
library(ggplot2)
library(scales)
library(tidyquant)

## Need to write up summary and then explore the meaning of each regression.

setwd("/Users/tylerwilson/Desktop/UChicago Files/CAPSTONE")
refugees <- read_csv("refugee_flows.csv")

lm1 <- lm(ln_arrival_total ~ ln_riots_prevweek, data = refugees)
summary(lm1)
## The coeffcient (.488) of riots in the previous is week shows us the percent change in arrivals. Significant correlation.

lm2 <- lm(ln_arrival_total ~ ln_wave_height_prevweek, data = refugees)
summary(lm2)
## The coefficent (-2.71546) tells us when the waves are smaller there will be more arrivals. elasticity coefficient is x. 

lm3 <- lm(ln_arrival_total ~ ln_wave_height_prevweek + ln_riots_prevweek, data = refugees)
summary(lm3)

##adding fixed effects
#refugees$month <- factor(refugees$month)

lm4 <- lm(ln_arrival_total ~ ln_wave_height_prevweek + ln_riots_prevweek + month, data = refugees)
summary(lm4)



## Making new column for a combined date. We will graph rate by day.
refugees$Date <- as.Date(paste(refugees$day, refugees$month, refugees$year, sep = "-"), "%d-%m-%Y")

##plot of deathrate over time.
ggplot(refugees) +
  geom_ma(aes(x = Date, y = deathrate, color = "deathrate"), linetype = 1, n = 15, span=0.03, se = FALSE) +
  scale_y_continuous(name = "Death Rate", breaks = seq(0.0,1,0.2), limits = c(0, 1)) +
  theme(panel.background = element_blank(), legend.title = element_blank(), legend.position = "top", 
  panel.border = element_rect(fill = NA, color = "black", size = 1),
  plot.title = element_text(face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y") +
  labs(title = "Migrant Death Rate at Sea Over Time") +
  scale_color_manual(values = "red")

##plot of wave height over time
ggplot(refugees) +
geom_ma(aes(x = Date, y = wave_height_prevweek, color = "wave height"),linetype = 1, n =7, span = 0.03) +
scale_y_continuous(name = "Wave Height", breaks = seq(0,2.5, 0.5), limits = c(0, 2.5)) +
theme(panel.background = element_blank(), legend.title = element_blank(), legend.position = "top", 
panel.border = element_rect(fill = NA, color = "black", size = 1),
plot.title = element_text(face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1)) +
labs(title = "Mediterranean Wave Height Over Time") +
scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y") +
scale_color_manual(values = "blue")
  
  




##Combined plot. Shows us the seasonal effects of both the Death Rate and the Wave Height. 

ggplot(refugees) +
geom_ma(aes(x = Date, y = deathrate, color = "deathrate"), linetype = 1, n = 15, span=0.03, se = FALSE) +
geom_ma(aes(x = Date, y = wave_height_prevweek*0.4, color = "wave height"),linetype = 1, n = 7, span = 0.03) +
scale_y_continuous(name = "Death Rate", limits = c(0,1), breaks = seq(0.0,1,0.2),
sec.axis = sec_axis(~.*2.5, name = "Wave Height", breaks = seq(0.0,2.5, 0.5))) +
theme(panel.background = element_blank(), legend.title = element_blank(), legend.position = "top", 
panel.border = element_rect(fill = NA, color = "black", size = 1),
plot.title = element_text(face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1)) +
labs(title ="Seasonal Effect on Migrant Death Rate at Sea
     and Mediterranean Wave Height ")  +
scale_color_manual(values = c("red", "blue")) +
scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y")

## if you switch to smooth, take out linetype and make n = 100. second switch to line for readability. take out line type.
## can use expand function to put 1 and 2.5 at top of graph. or revert back? Not sure.
##can also use the expand function to play with how close to the edge the data is. I think.