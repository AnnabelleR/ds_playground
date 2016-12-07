rm(list=ls())
cat("\014")

options(scipen=999)

## Loading Libraries
library(ggplot2)
library(scales)
library(reshape2)
library(lubridate)
library(dplyr)
library(doBy)
library(data.table)
library(corrplot)
library(zoo)
library(Amelia)
library(caret)
library(pscl)
library(psych)
library(leaflet)
library(shiny)

## Functions
factor_to_numeric <- function(x) {
  return (as.numeric(as.character(x)))
}

length_unique <- function(x) {
  return (length(unique(x)))
}

give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

nice_time <- function(x) {
  return (strptime(x, "%d%b%Y"))
}

nice_time2 <- function(x) {
  return (strptime(x, "%Y-%m-%d"))
}

to_date <- function(x) {
  return (as.Date(x, origin="1970-01-01"))
}

my_min <- function(x) {
  return (min(x, na.rm = TRUE))
}

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

setwd('/Users/annabelle/Documents/kaggle/airports/')

###############################################################################################################
# Loading Data
###############################################################################################################
## Airports
airports_data <- read.csv("./airportsnew2.csv")
names(airports_data) <- c("iata","airport_name","airport_city","state","country","lat","long")
head(airports_data)
summary(airports_data)

## Carriers
carriers_data <- read.csv("./carriers.csv")
carriers_data <- carriers_data[!apply(carriers_data == "", 1, all),]
names(carriers_data)[2] <- "carrier_name"
head(carriers_data)
summary(carriers_data)

## Traffic Data
all_data_air <- read.csv("./intermediate_data_sets/all_data_air.csv",row.names = 1)
head(all_data_air)
summary(all_data_air)



###############################################################################################################
# Analysis
###############################################################################################################

#######################
# By Time
#######################

## Delays by time of day, week
flights_by_month <- summaryBy(FlightNum ~ FlightMONTH + FlightYEAR, data = all_data_air, FUN=length)
names(flights_by_month)[3] <- "flights_count"
head(flights_by_month)
sum(flights_by_month$flights_count)

delays_by_month <- summaryBy(DepDel15 + DepDel1H + ArrDel15 + ArrDel1H
                            ~ FlightMONTH + FlightYEAR, data = all_data_air, FUN=sum, na.rm=TRUE)
delays_by_month <- merge(delays_by_month,flights_by_month,by=c("FlightMONTH","FlightYEAR"))
delays_by_month$prop_arr_delayed <- delays_by_month$ArrDel15.sum/delays_by_month$flights_count
delays_by_month$prop_arr_delayed_1H <- delays_by_month$ArrDel1H.sum/delays_by_month$flights_count
head(delays_by_month)
sum(delays_by_month$flights_count)
#write.csv(delays_by_month,file = "./intermediate_data_sets/delays_by_month.csv")

ggplot(data=delays_by_month,
       aes(x=FlightMONTH, y=prop_arr_delayed, group=FlightYEAR)) +
  #geom_bar(aes(fill = FlightYEAR), colour = "grey", stat="identity", position=position_dodge(), width = 1) +
  geom_line(aes(colour = FlightYEAR, alpha=flights_count), size = 1) +
  xlab("") + ylab("% Flights Delayed") + ggtitle("% Flights Delayed") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks=1:12)  +
  theme(legend.position="bottom",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.text = element_text(size=10),
        legend.justification = "left") +
  scale_fill_manual(values = c("#66c2a5", '#fc8d62', "#8da0cb"), 
                    labels = c("2014", "2015", "2016"), name="Year")


ggplot(data=delays_by_month,
       aes(x=FlightMONTH, y=flights_count, group=FlightYEAR)) +
  #geom_bar(aes(fill = FlightYEAR), colour = "grey", stat="identity", position=position_dodge(), width = 1) +
  geom_line(aes(colour = FlightYEAR), size = 1) +
  xlab("") + ylab("Flights") + ggtitle("Flights") +
  scale_y_continuous(labels = comma,limits = c(0,600000)) +
  scale_x_continuous(breaks=1:12)  +
  theme(legend.position="bottom",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.text = element_text(size=10),
        legend.justification = "left") +
  scale_fill_manual(values = c("#66c2a5", '#fc8d62', "#8da0cb"), 
                    labels = c("2014", "2015", "2016"), name="Year")

flights_by_month_carrier <- summaryBy(FlightNum ~ FlightMONTH + FlightYEAR + UniqueCarrier + carrier_name, data = all_data_air, FUN=length)
names(flights_by_month_carrier)[5] <- "flights_count"
head(flights_by_month_carrier)
sum(flights_by_month_carrier$flights_count)
#write.csv(flights_by_month_carrier,file = "./intermediate_data_sets/flights_by_month_carrier.csv")

delays_by_month_carrier <- summaryBy(DepDel15 + DepDel1H + ArrDel15 + ArrDel1H
                             ~ FlightMONTH + FlightYEAR + UniqueCarrier + carrier_name, data = all_data_air, FUN=sum, na.rm=TRUE)
delays_by_month_carrier <- merge(delays_by_month_carrier,flights_by_month_carrier,by=c("FlightMONTH","FlightYEAR","UniqueCarrier","carrier_name"))
delays_by_month_carrier$prop_arr_delayed <- delays_by_month_carrier$ArrDel15.sum/delays_by_month_carrier$flights_count
delays_by_month_carrier$prop_arr_delayed_1H <- delays_by_month_carrier$ArrDel1H.sum/delays_by_month_carrier$flights_count
delays_by_month_carrier$FlightYEAR <- as.factor(delays_by_month_carrier$FlightYEAR)
head(delays_by_month_carrier)
sum(delays_by_month_carrier$flights_count)
#write.csv(delays_by_month_carrier,file = "./intermediate_data_sets/delays_by_month_carrier.csv")


ggplot(data=subset(delays_by_month_carrier, UniqueCarrier == "NK"),
       aes(x=FlightMONTH, y=prop_arr_delayed, group=FlightYEAR)) +
  #geom_bar(aes(fill = FlightYEAR), colour = "grey", stat="identity", position=position_dodge(), width = 1) +
  geom_line(aes(colour = FlightYEAR, alpha=flights_count), size = 1) +
  xlab("") + ylab("% Flights Delayed") + ggtitle("% Flights Delayed") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks=1:12)  +
  theme(legend.position="bottom",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.text = element_text(size=10),
        legend.justification = "left") +
  scale_fill_manual(values = c("#66c2a5", '#fc8d62', "#8da0cb"), 
                    labels = c("2014", "2015", "2016"), name="Year")


flights_by_month_carrier_spirit <- subset(flights_by_month_carrier,UniqueCarrier == "NK")
head(flights_by_month_carrier_spirit)

ggplot(data=flights_by_month_carrier_spirit,
       aes(x=FlightMONTH, y=flights_count, group=as.factor(FlightYEAR))) +
  geom_line(aes(colour = as.factor(FlightYEAR)), size = 1) +
  xlab("") + ylab("Flights") + ggtitle("Spirit Airlines - Flights") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks=1:12)  +
  theme(legend.position="bottom",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.text = element_text(size=10),
        legend.justification = "left") +
  scale_colour_manual(values = c("#66c2a5", '#fc8d62'), 
                    labels = c("2015", "2016"), name="Year")

flights_by_time <- summaryBy(FlightNum ~ FlightDOW + FlightHOUR, data = all_data_air, FUN=length)
names(flights_by_time)[3] <- "flights_count"
head(flights_by_time)
sum(flights_by_time$flights_count)
#write.csv(flights_by_time,file = "./intermediate_data_sets/flights_by_time.csv")

delays_by_time <- summaryBy(DepDel15 + DepDel1H + DepDelayMinutes + ArrDel15 + ArrDel1H + ArrDelayMinutes + AirTime
                            ~ FlightDOW + FlightHOUR, data = all_data_air, FUN=sum, na.rm=TRUE)
delays_by_time <- merge(delays_by_time,flights_by_time,by=c("FlightDOW","FlightHOUR"))
delays_by_time$prop_arr_delayed <- delays_by_time$ArrDel15.sum/delays_by_time$flights_count
delays_by_time$prop_arr_delayed_1H <- delays_by_time$ArrDel1H.sum/delays_by_time$flights_count
head(delays_by_time)
#write.csv(delays_by_time,file = "./intermediate_data_sets/delays_by_time.csv")

delays_by_time_long <- melt(delays_by_time[,c("FlightHOUR","FlightDOW","prop_arr_delayed_1H")],id=c("FlightDOW","FlightHOUR"))
head(delays_by_time_long)

table(delays_by_time_long$FlightHOUR)

delays_by_time_long$FlightDOW <- factor(delays_by_time_long$FlightDOW, 
                                        levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))
names(delays_by_time_long)[1] <- "Weekday"

ggplot(data=delays_by_time_long,
       aes(x=FlightHOUR, y=value, group=Weekday)) +
  geom_line(aes(colour = Weekday), size = 1) +
  xlab("") + ylab("% Delayed more than 1 hour") + ggtitle("% Flights Delayed by Hour") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks=0:23)  +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=10),
        legend.justification = "left")

delays_by_time_long2 <- melt(delays_by_time[,c("FlightHOUR","FlightDOW","flights_count")],id=c("FlightDOW","FlightHOUR"))
head(delays_by_time_long2)

delays_by_time_long2$FlightDOW <- factor(delays_by_time_long2$FlightDOW, 
                                         levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))
names(delays_by_time_long2)[1] <- "Weekday"

ggplot(data=delays_by_time_long2,
       aes(x=FlightHOUR, y=value, group=FlightDOW)) +
  geom_line(aes(colour = FlightDOW), size = 1,alpha=0.8) +
  xlab("") + ylab("Flights") + ggtitle("Flights Count by Hour") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks=0:23)  +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=10),
        legend.justification = "left")


delays_by_time_long_final <- merge(x=delays_by_time_long,y=delays_by_time_long2[,c(1,2,4)],by=c("Weekday", "FlightHOUR"))
head(delays_by_time_long_final)

ggplot(data=delays_by_time_long_final,
       aes(x=FlightHOUR, y=value.x, group=Weekday, alpha=value.y)) +
  geom_line(aes(colour = Weekday), size = 1) +
  guides(alpha = FALSE) +
  xlab("") + ylab("% Delayed more than 1 hour") + ggtitle("% Flights Delayed by Hour") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks=0:23)  +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=10),
        legend.justification = "left")

## Delays by carriers, time of day, week
flights_by_carrier_time <- summaryBy(FlightNum ~ UniqueCarrier + carrier_name + FlightDOW + FlightHOUR, data = all_data_air, FUN=length)
names(flights_by_carrier_time)[5] <- "flights_count"
head(flights_by_carrier_time)
sum(flights_by_carrier_time$flights_count)

delays_by_carrier_time <- summaryBy(DepDel15 + DepDel1H + DepDelayMinutes + ArrDel15 + ArrDel1H + ArrDelayMinutes + AirTime + Arr_delay_flag
                                    ~ UniqueCarrier + carrier_name + FlightDOW + FlightHOUR, data = all_data_air, FUN=sum, na.rm=TRUE)
delays_by_carrier_time <- merge(delays_by_carrier_time,flights_by_carrier_time,by=c("UniqueCarrier","carrier_name","FlightDOW","FlightHOUR"))
delays_by_carrier_time$prop_arr_delayed <- delays_by_carrier_time$ArrDel15.sum/delays_by_carrier_time$flights_count
delays_by_carrier_time$prop_arr_delayed_1H <- delays_by_carrier_time$ArrDel1H.sum/delays_by_carrier_time$flights_count
delays_by_carrier_time$avg_delay <- delays_by_carrier_time$ArrDelayMinutes.sum/delays_by_carrier_time$Arr_delay_flag.sum
head(delays_by_carrier_time)
#write.csv(delays_by_carrier_time,file = "./intermediate_data_sets/delays_by_carrier_time.csv")

## By month + by airport (snow in norht, typhoons in south)


#######################
# By Carrier
#######################

# flag for delay of more than 1h
# delay as ratio of flight time

## Flights by carrier
flights_by_carrier <- summaryBy(FlightNum ~ UniqueCarrier + carrier_name, data = all_data_air, FUN=length)
names(flights_by_carrier)[3] <- "flights_count"
head(flights_by_carrier)
sum(flights_by_carrier$flights_count)
#write.csv(flights_by_carrier,file = "./intermediate_data_sets/flights_by_carrier.csv")

flights_by_carrier$carrier_name <- factor(flights_by_carrier$carrier_name, 
                                          levels = flights_by_carrier$carrier_name[order(-flights_by_carrier$flights_count)])

ggplot(data=flights_by_carrier,
       aes(x=carrier_name, y=flights_count,order = desc(flights_count))) +
  geom_bar(fill = "#2ebd59", colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("Flights") + ggtitle("Flights by Carrier") +
  scale_y_continuous(labels = comma) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 45, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")

## Flights cancelled 
table(subset(all_data_air,UniqueCarrier == "NK")$Cancelled)

my_sum <- function(x) {
  return (sum(as.numeric(x), na.rm = TRUE))
}

flights_cancelled_by_carrier <- summaryBy(FlightNum + Arr_delay_flag + Cancelled + Diverted ~ UniqueCarrier + carrier_name, data = all_data_air, FUN=c(length,my_sum))
names(flights_cancelled_by_carrier)[3] <- "flights_count"
names(flights_cancelled_by_carrier)[8] <- "flights_delayed"
names(flights_cancelled_by_carrier)[9] <- "flights_cancelled"
names(flights_cancelled_by_carrier)[10] <- "flights_diverted"
flights_cancelled_by_carrier$delayed_ratio <- flights_cancelled_by_carrier$flights_delayed/flights_cancelled_by_carrier$flights_count
flights_cancelled_by_carrier$cancelled_ratio <- flights_cancelled_by_carrier$flights_cancelled/flights_cancelled_by_carrier$flights_count
flights_cancelled_by_carrier$diverted_ratio <- flights_cancelled_by_carrier$flights_diverted/flights_cancelled_by_carrier$flights_count
head(flights_cancelled_by_carrier)
sum(flights_cancelled_by_carrier$flights_count)
#write.csv(flights_cancelled_by_carrier,file = "./intermediate_data_sets/flights_cancelled_by_carrier.csv")

flights_cancelled_by_carrier$carrier_name <- factor(flights_cancelled_by_carrier$carrier_name, 
                                                    levels = flights_cancelled_by_carrier$carrier_name[order(-flights_cancelled_by_carrier$cancelled_ratio)])

ggplot(data=flights_cancelled_by_carrier,
       aes(x=carrier_name, y=cancelled_ratio,order = desc(cancelled_ratio))) +
  geom_bar(fill = "#2ebd59", colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("% Flights Cancelled") + ggtitle("% Flights Cancelled by Carrier") +
  scale_y_continuous(labels = percent) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 60, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")


## Carriers with most delays 
delays_by_carrier <- summaryBy(DepDel15 + DepDel1H + DepDelayMinutes + ArrDel15 + ArrDel1H + ArrDelayMinutes + AirTime
                               ~ UniqueCarrier + carrier_name, data = all_data_air, FUN=sum, na.rm=TRUE)
delays_by_carrier <- merge(delays_by_carrier,flights_by_carrier,by=c("UniqueCarrier","carrier_name"))
delays_by_carrier$prop_arr_delayed <- delays_by_carrier$ArrDel15.sum/delays_by_carrier$flights_count
delays_by_carrier$prop_arr_delayed_1H <- delays_by_carrier$ArrDel1H.sum/delays_by_carrier$flights_count
head(delays_by_carrier)
#write.csv(delays_by_carrier,file = "./intermediate_data_sets/delays_by_carrier.csv")

### As a proportion of total flights
delays_by_carrier$carrier_name <- factor(delays_by_carrier$carrier_name, 
                                         levels = delays_by_carrier$carrier_name[order(-delays_by_carrier$prop_arr_delayed)])

ggplot(data=delays_by_carrier,
       aes(x=carrier_name, y=prop_arr_delayed,order = desc(prop_arr_delayed))) +
  geom_bar(fill = "#2ebd59", colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("% Flights Delayed at Arrival") + ggtitle("Flights Delayed at Arrival (%)") +
  scale_y_continuous(labels = percent) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 45, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")

delays_by_carrier$carrier_name <- factor(delays_by_carrier$carrier_name, 
                                         levels = delays_by_carrier$carrier_name[order(-delays_by_carrier$prop_arr_delayed_1H)])

ggplot(data=delays_by_carrier,
       aes(x=carrier_name, y=prop_arr_delayed_1H,order = desc(prop_arr_delayed))) +
  geom_bar(fill = "#2ebd59", colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("% Flights Delayed at Arrival") + ggtitle("Flights Delayed at Arrival - More than 1 hour (%)") +
  scale_y_continuous(labels = percent) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 45, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")

### Delay as proportion of flight time
head(all_data_air)
delays_prop_airtime <- summaryBy(DepDelay_prop_airtime + ArrDelay_prop_airtime
                                 ~ UniqueCarrier + carrier_name, 
                                 data = subset(all_data_air, (DepDelayMinutes > 0 | ArrDelayMinutes > 0)), 
                                 FUN=mean, na.rm=TRUE)
head(delays_prop_airtime)
#write.csv(delays_prop_airtime,file = "./intermediate_data_sets/delays_prop_airtime.csv")

delays_prop_airtime$carrier_name <- factor(delays_prop_airtime$carrier_name, 
                                           levels = delays_prop_airtime$carrier_name[order(-delays_prop_airtime$DepDelay_prop_airtime.mean)])

ggplot(data=delays_prop_airtime,
       aes(x=carrier_name, y=DepDelay_prop_airtime.mean,order = desc(DepDelay_prop_airtime.mean))) +
  geom_bar(fill = "#2ebd59", colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("% of Airtime") + ggtitle("Average Delay as % of Airtime") +
  scale_y_continuous(labels = percent) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 45, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")


## Split by reason
# CarrierDelay	Carrier Delay, in Minutes
# WeatherDelay	Weather Delay, in Minutes
# NASDelay	National Air System Delay, in Minutes
# SecurityDelay	Security Delay, in Minutes
# LateAircraftDelay	Late Aircraft Delay, in Minutes
delays_types_by_carrier <- summaryBy(CarrierDelay + WeatherDelay + NASDelay + SecurityDelay + LateAircraftDelay
                                     ~ UniqueCarrier + carrier_name, data = all_data_air, FUN=sum, na.rm=TRUE)

for (i in (1:nrow(delays_types_by_carrier))) {
  delays_types_by_carrier$total_delay[i] <- sum(delays_types_by_carrier[i,c(3:7)])
}  
head(delays_types_by_carrier)

delays_types_by_carrier_prop <- delays_types_by_carrier
delays_types_by_carrier_prop[,c(3:7)] <- delays_types_by_carrier_prop[,c(3:7)]/delays_types_by_carrier_prop[,8]

for (i in (1:nrow(delays_types_by_carrier_prop))) {
  delays_types_by_carrier_prop$total_delay_prop[i] <- sum(delays_types_by_carrier_prop[i,c(3:7)])
} 

head(delays_types_by_carrier_prop)
#write.csv(delays_types_by_carrier_prop,file = "./intermediate_data_sets/delays_types_by_carrier_prop.csv")

delays_types_by_carrier_prop_long <- melt(delays_types_by_carrier_prop[,c(2:7)],id="carrier_name")
head(delays_types_by_carrier_prop_long)

ggplot(data=delays_types_by_carrier_prop_long,
       aes(x=carrier_name, y=value, group=variable)) +
  geom_bar(aes(fill = variable), colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("% Total Delays") + ggtitle("Reasons for Delays") +
  scale_y_continuous(labels = percent) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 60, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left") +
  scale_fill_manual(values = c("#66c2a5", '#fc8d62', "#8da0cb", "#4daf4a", "#e78ac3"), 
                    labels = c("Carrier", "Weather", "NAS", "Security", "Late Aircraft"), name="Reason")

#######################
# By Airport
#######################

## Airports with most departure delays (check reason that can be used)
flights_airport_dep <- summaryBy(FlightNum ~ Origin + OriginAirportID + airport_name_origin, data = all_data_air, FUN=length)
names(flights_airport_dep)[4] <- "flights_count"
head(flights_airport_dep)
summary(flights_airport_dep)
#write.csv(flights_airport_dep,file = "./intermediate_data_sets/flights_airport_dep.csv")

major_airports <- subset(flights_airport_dep, flights_count > 50000)[3]
major_airports$airport_name_origin <- as.character(major_airports$airport_name_origin)
major_airports <- major_airports[1][[1]]
major_airports

sum(flights_airport_dep$flights_count)
length_unique(flights_airport_dep$Origin)

flights_airport_dep$airport_name_origin <- factor(flights_airport_dep$airport_name_origin, 
                                                  levels = flights_airport_dep$airport_name_origin[order(-flights_airport_dep$flights_count)])

ggplot(data=subset(flights_airport_dep, flights_count > 50000),
       aes(x=airport_name_origin, y=flights_count,order = desc(flights_count))) +
  geom_bar(fill = "#2ebd59", colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("Flights") + ggtitle("Flights by Departure Airport") +
  scale_y_continuous(labels = comma) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 60, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")

## Delays, reasons, nice leaflet map
delays_by_airport_dep <- summaryBy(DepDel15 + DepDel1H + Dep_delay_flag + DepDelayMinutes
                                   ~ Origin + OriginAirportID + airport_name_origin, data = all_data_air, FUN=sum, na.rm=TRUE)
delays_by_airport_dep <- merge(delays_by_airport_dep,flights_airport_dep,by=c("Origin","OriginAirportID","airport_name_origin"))
delays_by_airport_dep$prop_dep_delayed <- delays_by_airport_dep$DepDel15.sum/delays_by_airport_dep$flights_count
delays_by_airport_dep$prop_dep_delayed_1H <- delays_by_airport_dep$DepDel1H.sum/delays_by_airport_dep$flights_count
delays_by_airport_dep$avg_delay <- delays_by_airport_dep$DepDelayMinutes.sum/delays_by_airport_dep$Dep_delay_flag.sum
head(delays_by_airport_dep)
summary(delays_by_airport_dep)
#write.csv(delays_by_airport_dep,file = "./intermediate_data_sets/delays_by_airport_dep.csv")


library(gtable)
library(grid)

p1 <- ggplot(data=subset(delays_by_airport_dep, flights_count > 50000),
             aes(x=airport_name_origin, y=prop_dep_delayed,order = desc(prop_dep_delayed))) +
  xlab("") + ggtitle("Flights Delayed (%)") +
  geom_bar(fill = "#2ebd59", colour = "grey", stat="identity", position=position_dodge()) +
  scale_y_continuous(labels = percent) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 65, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")

p2 <- ggplot(data=subset(delays_by_airport_dep, flights_count > 50000),
             aes(x=airport_name_origin, y=avg_delay, group=1)) +
  ggtitle("Average Delay") +
  geom_line(colour = "red", size=1) + theme_bw() %+replace% 
  theme(panel.background = element_blank(),
      panel.grid.minor = element_blank(), 
      panel.grid.major = element_blank())

g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
g$grobs[[8]]$children$GRID.text.820$label <- c("% Delayed\n", "Average Delay (min)")
g$grobs[[8]]$children$GRID.text.820$x <- unit(c(-0.155, 0.829), "npc")

# draw it
grid.draw(g)
  


delays_by_airport_dep$airport_name_origin <- factor(delays_by_airport_dep$airport_name_origin, 
                                                    levels = delays_by_airport_dep$airport_name_origin[order(-delays_by_airport_dep$prop_dep_delayed)])

ggplot(data=subset(delays_by_airport_dep, flights_count > 50000),
       aes(x=airport_name_origin, y=prop_dep_delayed,order = desc(prop_dep_delayed))) +
  geom_bar(fill = "#2ebd59", colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("Flights Delayed (%)") + ggtitle("Flights Delayed by Departure Airport (%) - more than 1H, major airports") +
  scale_y_continuous(labels = percent) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 65, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")

delays_by_airport_dep$airport_name_origin <- factor(delays_by_airport_dep$airport_name_origin, 
                                                    levels = delays_by_airport_dep$airport_name_origin[order(-delays_by_airport_dep$avg_delay)])

ggplot(data=subset(delays_by_airport_dep, flights_count > 50000),
       aes(x=airport_name_origin, y=avg_delay, group=1)) +
  geom_line(colour = "#2ebd59") +
  xlab("") + ylab("Average Delay (min)") + ggtitle("Average Delay for Delayed Flights - major airports") +
  scale_y_continuous(labels = comma) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 70, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")

delays_types_by_dep_airport <- summaryBy(WeatherDelay + NASDelay + SecurityDelay
                                         ~ Origin + OriginAirportID + airport_name_origin, data = all_data_air, FUN=sum, na.rm=TRUE)

for (i in (1:nrow(delays_types_by_dep_airport))) {
  delays_types_by_dep_airport$total_delay[i] <- sum(delays_types_by_dep_airport[i,c(4:6)])
}  
head(delays_types_by_dep_airport)

delays_types_by_dep_airport_prop <- delays_types_by_dep_airport
delays_types_by_dep_airport_prop[,c(4:6)] <- delays_types_by_dep_airport_prop[,c(4:6)]/delays_types_by_dep_airport_prop[,7]

for (i in (1:nrow(delays_types_by_dep_airport_prop))) {
  delays_types_by_dep_airport_prop$total_delay_prop[i] <- sum(delays_types_by_dep_airport_prop[i,c(4:6)])
} 

head(delays_types_by_dep_airport_prop)
#write.csv(delays_types_by_dep_airport_prop,file = "./intermediate_data_sets/delays_types_by_dep_airport_prop.csv")

delays_types_by_dep_airport_prop_long <- melt(delays_types_by_dep_airport_prop[,c(3:6)],id="airport_name_origin")
head(delays_types_by_dep_airport_prop_long)

ggplot(data=subset(delays_types_by_dep_airport_prop_long, airport_name_origin %in% major_airports),
       aes(x=airport_name_origin, y=value, group=variable)) +
  geom_bar(aes(fill = variable), colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("% Total Delays") + ggtitle("Reasons for Delays") +
  scale_y_continuous(labels = percent) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 60, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left") +
  scale_fill_manual(values = c("#66c2a5", '#fc8d62', "#8da0cb"), 
                    labels = c("Weather", "NAS", "Security"), name="Reason")

## map dep airports (flights, delayed more than 1h, avg delay)

dep_aiport_map <- merge(x=delays_by_airport_dep,y=airports_data[,c(1,6,7)],by.x="Origin",by.y="iata")
dep_aiport_map <- subset(dep_aiport_map, flights_count > 1000)
dep_aiport_map$flights_count_scaled <- (range01(dep_aiport_map$flights_count) * 10) + 2
dep_aiport_map$prop_dep_delayed_1H_perc <- percent(dep_aiport_map$prop_dep_delayed_1H)
head(dep_aiport_map)
summary(dep_aiport_map)

pal <- colorNumeric(
  palette = "Reds",
  domain = dep_aiport_map$prop_dep_delayed_1H
)

leaflet() %>% addProviderTiles("CartoDB.Positron",options = providerTileOptions(opacity = 0.8)) %>%
  setView(lng = -95, lat = 40,  zoom = 3) %>% 
  addCircleMarkers(
    data = dep_aiport_map,
    ~long, 
    ~lat, 
    radius = ~dep_aiport_map$flights_count_scaled,
    color = ~pal(dep_aiport_map$prop_dep_delayed_1H),
    stroke = FALSE, fillOpacity = 0.8,
    popup=paste(strong("Airport:"), dep_aiport_map$airport_name_origin, "<br>", 
                strong("Total Flights:"), prettyNum(dep_aiport_map$flights_count,big.mark=",",scientific=FALSE), "<br>",
                strong("Average Delay (mn):"), round(dep_aiport_map$avg_delay,1), "<br>",
                strong("Flight Delayed for more than 1h:"), dep_aiport_map$prop_dep_delayed_1H_perc)
  ) %>%
  addLegend("bottomright", pal = pal, values = dep_aiport_map$prop_dep_delayed_1H,
            title = "% Flights Delayed",
            opacity = 1, labFormat = labelFormat(
              prefix = '', suffix = '%', between = ', ',
              transform = function(x) 100 * x
            )) 

head(dep_aiport_map)
summary(dep_aiport_map)

ggplot(data=dep_aiport_map,
       aes(x=prop_dep_delayed, y=flights_count)) +
  geom_point(colour = "#2ebd59", size = 1) +
  xlab("% Delayed") + ylab("Flights") + ggtitle("Flights Count vs % Flights Delayed") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = percent, limits = c(0:1/3)) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.text = element_text(size=10),
        legend.justification = "left") + guides(colour=FALSE)
 # geom_text_repel(aes(label=airport_name_origin,colour=airport_name_origin)) +
  

## Airports with most arrival delays (check reason that can be used)
flights_airport_arr <- summaryBy(FlightNum ~ Dest + DestAirportID + airport_name_dest, data = all_data_air, FUN=length)
names(flights_airport_arr)[4] <- "flights_count"
head(flights_airport_arr)
#write.csv(flights_airport_arr,file = "./intermediate_data_sets/flights_airport_arr.csv")

sum(flights_airport_arr$flights_count)
length_unique(flights_airport_arr$Dest)

flights_airport_arr$airport_name_dest <- factor(flights_airport_arr$airport_name_dest, 
                                                levels = flights_airport_arr$airport_name_dest[order(-flights_airport_arr$flights_count)])

ggplot(data=subset(flights_airport_arr, flights_count > 50000),
       aes(x=airport_name_dest, y=flights_count,order = desc(flights_count))) +
  geom_bar(fill = "#2ebd59", colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("Flights") + ggtitle("Flights by Arrival Airport") +
  scale_y_continuous(labels = comma) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 60, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")

#######################
# By Flight
#######################

## Flight Paths with most delays (check reason that can be used, find best airline to fly with)
flights_paths <- summaryBy(FlightNum ~ 
                             Origin + OriginAirportID + airport_name_origin + 
                             Dest + DestAirportID + airport_name_dest, 
                           data = all_data_air, FUN=length)
names(flights_paths)[7] <- "flights_count"
flights_paths$path <- paste(flights_paths$Origin,flights_paths$Dest,sep="-")
head(flights_paths)
summary(flights_paths)
sum(flights_paths$flights_count)
#write.csv(flights_paths,file = "./intermediate_data_sets/flights_paths.csv")

flights_paths$path <- factor(flights_paths$path, 
                             levels = flights_paths$path[order(-flights_paths$flights_count)])

ggplot(data=subset(flights_paths, flights_count > 15000),
       aes(x=path, y=flights_count,order = desc(flights_count))) +
  geom_bar(fill = "#2ebd59", colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("Flights") + ggtitle("Flights by Path") +
  scale_y_continuous(labels = comma) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 60, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")

## Map
flights_map <- merge(x=flights_paths,y=airports_data[,c(1,6,7)],by.x="Origin",by.y="iata")
names(flights_map)[9] <- "start_lat"
names(flights_map)[10] <- "start_long"
flights_map <- merge(x=flights_map,y=airports_data[,c(1,6,7)],by.x="Dest",by.y="iata")
names(flights_map)[11] <- "end_lat"
names(flights_map)[12] <- "end_long"
flights_map <- flights_map[,c(4,6:12)]
head(flights_map)

flights_map2 <- data.frame(group = flights_map$path,
                           lat = c(flights_map$start_lat, flights_map$end_lat),
                           long = c(flights_map$start_long, flights_map$end_long))
flights_map2 <- merge(flights_map2,flights_map[,c("path","flights_count")],by.x = "group",by.y = "path")
head(flights_map2)

flights_map_select <- subset(flights_map2,flights_count > 15000)
nrow(flights_map_select)
head(flights_map_select,10)

pal2 <- colorFactor(
  palette = "Set2",
  domain = flights_map_select$group
)

leaflet() %>% addProviderTiles("CartoDB.Positron",options = providerTileOptions(opacity = 0.8)) %>%
  setView(lng = -95, lat = 40,  zoom = 3) %>% 
  addPolylines(data = flights_map_select, 
               lng = ~long, lat = ~lat, group = ~group, 
               weight = 2, color = ~pal2(group), 
               popup=paste(strong("Path:"), flights_map_select$group, "<br>", 
                           strong("Total Flights:"), prettyNum(flights_map_select$flights_count,big.mark=",",scientific=FALSE)))


## Delays
head(all_data_air)

delays_by_path <- summaryBy(DepDel15 + DepDel1H + Dep_delay_flag + DepDelayMinutes 
                            ~ Origin + OriginAirportID + airport_name_origin + 
                              Dest + DestAirportID + airport_name_dest, 
                            data = all_data_air, FUN=sum, na.rm=TRUE)
delays_by_path$path <- paste(delays_by_path$Origin,delays_by_path$Dest,sep="-")
delays_by_path <- merge(delays_by_path,flights_paths,by=c("Origin","OriginAirportID","airport_name_origin",
                                                          "Dest","DestAirportID","airport_name_dest","path"))
delays_by_path$prop_dep_delayed <- delays_by_path$DepDel15.sum/delays_by_path$flights_count
delays_by_path$prop_dep_delayed_1H <- delays_by_path$DepDel1H.sum/delays_by_path$flights_count
delays_by_path$avg_delay <- delays_by_path$DepDelayMinutes.sum/delays_by_path$Dep_delay_flag.sum
head(delays_by_path)
#write.csv(delays_by_path,file = "./intermediate_data_sets/delays_by_path.csv")

delays_by_path$path <- factor(delays_by_path$path, 
                              levels = delays_by_path$path[order(-delays_by_path$prop_dep_delayed_1H)])

ggplot(data=subset(delays_by_path, flights_count > 15000),
       aes(x=path, y=prop_dep_delayed_1H,order = desc(prop_dep_delayed_1H))) +
  geom_bar(fill = "#2ebd59", colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("Flights Delayed (%)") + ggtitle("Flights Delayed by Path (%) - more than 1H, major paths") +
  scale_y_continuous(labels = percent) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 60, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")

## Best alternatives for major paths: by path & by carrier & reasons
### SFO-LAS, SFO-LAS, ORD-LGA

flights_paths_carr <- summaryBy(FlightNum ~ 
                                  Origin + OriginAirportID + airport_name_origin + 
                                  Dest + DestAirportID + airport_name_dest +
                                  UniqueCarrier + carrier_name, 
                                data = all_data_air, FUN=length)
names(flights_paths_carr)[9] <- "flights_count"
flights_paths_carr$path <- paste(flights_paths_carr$Origin,flights_paths_carr$Dest,sep="-")
head(flights_paths_carr)
#write.csv(flights_paths_carr,file = "./intermediate_data_sets/flights_paths_carr.csv")

delays_by_path_carr <- summaryBy(DepDel15 + DepDel1H + Dep_delay_flag + DepDelayMinutes 
                                 ~ Origin + OriginAirportID + airport_name_origin + 
                                   Dest + DestAirportID + airport_name_dest + 
                                   UniqueCarrier + carrier_name, 
                                 data = all_data_air, FUN=sum, na.rm=TRUE)
delays_by_path_carr$path <- paste(delays_by_path_carr$Origin,delays_by_path_carr$Dest,sep="-")
delays_by_path_carr <- merge(delays_by_path_carr,flights_paths_carr,by=c("Origin","OriginAirportID","airport_name_origin",
                                                                         "Dest","DestAirportID","airport_name_dest","path","UniqueCarrier","carrier_name"))
delays_by_path_carr$prop_dep_delayed <- delays_by_path_carr$DepDel15.sum/delays_by_path_carr$flights_count
delays_by_path_carr$prop_dep_delayed_1H <- delays_by_path_carr$DepDel1H.sum/delays_by_path_carr$flights_count
delays_by_path_carr$avg_delay <- delays_by_path_carr$DepDelayMinutes.sum/delays_by_path_carr$Dep_delay_flag.sum
delays_by_path_carr$path_carr <- paste(delays_by_path_carr$carrier_name,delays_by_path_carr$path,sep=" ")
head(delays_by_path_carr)
#write.csv(delays_by_path_carr,file = "./intermediate_data_sets/delays_by_path_carr.csv")

#top_paths <- c("LAS-SFO","SFO-LAS","LAX-SFO","SFO-LAX","ORD-LGA","LGA-ORD")
#subset(delays_by_path_carr, path %in% top_paths),

delays_by_path_carr$path_carr <- factor(delays_by_path_carr$path_carr, 
                                        levels = delays_by_path_carr$path_carr[order(-delays_by_path_carr$prop_dep_delayed_1H)])

ggplot(data=subset(delays_by_path_carr, path == "JFK-LAX"),
       aes(x=path_carr, y=prop_dep_delayed_1H,order = desc(prop_dep_delayed_1H))) +
  geom_bar(fill = "#2ebd59", colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("Flights Delayed (%)") + ggtitle("Flights Delayed by Path (%) - JFK to LAX") +
  scale_y_continuous(labels = percent) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 60, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")

library(ggvis)

delays_by_path_carr %>% 
  ggvis(~path_carr, ~prop_dep_delayed_1H) %>% 
  filter(path %in% eval(input_checkboxgroup(c("JFK-LAX", "JFK-LAS"), 
                                             selected = "JFK-LAX"))) %>% 
  layer_points(fill = ~path, size := 50, opacity := 0.5) %>%
  add_axis("x", title = "", properties = axis_props(labels = list(angle = 60, fontSize = 10))) %>%
  add_axis("y", title = "Flights Delayed (%)")

# Time in Air by Path and carrier
airtime_by_path_carr <- summaryBy(AirTime + ActualElapsedTime + Distance
                                  ~ Origin + OriginAirportID + airport_name_origin + 
                                    Dest + DestAirportID + airport_name_dest + 
                                    UniqueCarrier + carrier_name, 
                                  data = all_data_air, FUN=mean, na.rm=TRUE)
airtime_by_path_carr$path <- paste(airtime_by_path_carr$Origin,airtime_by_path_carr$Dest,sep="-")
head(airtime_by_path_carr)

airtime_by_path_carr_LAS_SFO <- subset(airtime_by_path_carr, path=="LAS-SFO")
head(airtime_by_path_carr_LAS_SFO)



# LAS-SFO: delays reasons by carrier
delays_types_by_path_carr <- summaryBy(CarrierDelay + WeatherDelay + NASDelay + SecurityDelay + LateAircraftDelay
                                       ~ UniqueCarrier + carrier_name + 
                                         Origin + airport_name_origin + 
                                         Dest + airport_name_dest, 
                                       data = all_data_air, FUN=sum, na.rm=TRUE)

for (i in (1:nrow(delays_types_by_path_carr))) {
  delays_types_by_path_carr$total_delay[i] <- sum(delays_types_by_path_carr[i,c(7:11)])
}  
head(delays_types_by_path_carr)

delays_types_by_path_carr_prop <- delays_types_by_path_carr
delays_types_by_path_carr_prop[,c(7:11)] <- delays_types_by_path_carr_prop[,c(7:11)]/delays_types_by_path_carr_prop[,12]

for (i in (1:nrow(delays_types_by_path_carr_prop))) {
  delays_types_by_path_carr_prop$total_delay_prop[i] <- sum(delays_types_by_path_carr_prop[i,c(7:11)])
} 

delays_types_by_path_carr_prop$path <- paste(delays_types_by_path_carr_prop$Origin,delays_types_by_path_carr_prop$Dest,sep="-")
delays_types_by_path_carr_prop$path_carr <- paste(delays_types_by_path_carr_prop$carrier_name,delays_types_by_path_carr_prop$path,sep=" ")
head(delays_types_by_path_carr_prop)
#write.csv(delays_types_by_path_carr_prop,file = "./intermediate_data_sets/delays_types_by_path_carr_prop.csv")

delays_types_by_path_carr_prop_long <- melt(delays_types_by_path_carr_prop[,c(2,7:11,14,15)],id=c("path_carr","path","carrier_name"))
head(delays_types_by_path_carr_prop_long)

ggplot(data=subset(delays_types_by_path_carr_prop_long,path=="SAN-SFO"),
       aes(x=carrier_name, y=value, group=variable)) +
  geom_bar(aes(fill = variable), colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("% Total Delays") + ggtitle("Reasons for Delays - SAN to SFO") +
  scale_y_continuous(labels = percent) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 45, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left") +
  scale_fill_manual(values = c("#66c2a5", '#fc8d62', "#8da0cb", "#4daf4a", "#e78ac3"), 
                    labels = c("Carrier", "Weather", "NAS", "Security", "Late Aircraft"), name="Reason")


ggplot(data=subset(delays_types_by_path_carr_prop_long,path=="ORD-LGA"),
       aes(x=carrier_name, y=value, group=variable)) +
  geom_bar(aes(fill = variable), colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("% Total Delays") + ggtitle("Reasons for Delays - ORD to LGA") +
  scale_y_continuous(labels = percent) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 45, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left") +
  scale_fill_manual(values = c("#66c2a5", '#fc8d62', "#8da0cb", "#4daf4a", "#e78ac3"), 
                    labels = c("Carrier", "Weather", "NAS", "Security", "Late Aircraft"), name="Reason")

ggplot(data=subset(delays_types_by_path_carr_prop_long,path=="DFW-ORD"),
       aes(x=carrier_name, y=value, group=variable)) +
  geom_bar(aes(fill = variable), colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("% Total Delays") + ggtitle("Reasons for Delays - DFW to ORD") +
  scale_y_continuous(labels = percent) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 45, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left") +
  scale_fill_manual(values = c("#66c2a5", '#fc8d62', "#8da0cb", "#4daf4a", "#e78ac3"), 
                    labels = c("Carrier", "Weather", "NAS", "Security", "Late Aircraft"), name="Reason")

#######################
# Best vs Worse Airlines
#######################
## Spirit Airlines

#### Where does Spirit fly (map)
#### Reasons of delays (dig deeper in data - time in air vs actual...)
#### Cancelled flights
#### Most delayed flights, average delay
#### Time of delays

head(delays_types_by_carrier_prop_long)

ggplot(data=subset(delays_types_by_carrier_prop_long, carrier_name == "Spirit Air Lines"),
       aes(x=carrier_name, y=value, group=variable)) +
  geom_bar(aes(fill = variable), colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("% Total Delays") + ggtitle("Reasons for Delays") +
  scale_y_continuous(labels = percent) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=12),
        legend.text = element_text(size=12),
        legend.justification = "left") +
  scale_fill_manual(values = c("#66c2a5", '#fc8d62', "#8da0cb", "#4daf4a", "#e78ac3"), 
                    labels = c("Carrier", "Weather", "NAS", "Security", "Late Aircraft"), name="Reason")

head(flights_paths_carr)
flights_paths_carr_spirit <- subset(flights_paths_carr,UniqueCarrier == "NK")
head(flights_paths_carr_spirit)
summary(flights_paths_carr_spirit)

flights_paths_carr_spirit$path <- factor(flights_paths_carr_spirit$path, 
                                         levels = flights_paths_carr_spirit$path[order(-flights_paths_carr_spirit$flights_count)])

ggplot(data=subset(flights_paths_carr_spirit, flights_count > 1000),
       aes(x=path, y=flights_count,order = desc(flights_count))) +
  geom_bar(fill = "#2ebd59", colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("Flights") + ggtitle("Spirit Airlines - Flights by Path (Top Paths)") +
  scale_y_continuous(labels = comma) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 60, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")

flights_map_spirit <- merge(flights_map,flights_paths_carr_spirit[,c("path","flights_count")],by="path")
head(flights_map_spirit)


library(plotly)
packageVersion('plotly')
library(dplyr)


# map projection
geo <- list(
  scope = 'north america',
  projection = list(type = 'azimuthal equal area'),
  showland = TRUE,
  landcolor = "#cdc9c9",
  countrycolor = "	#8b8989	"
)

plot_geo(locationmode = 'USA-states', color = I("red")) %>%
  add_segments(
    data = flights_map_spirit,
    x = ~start_long, xend = ~end_long,
    y = ~start_lat, yend = ~end_lat, text = ~path,
    alpha = 0.3, size = I(1), hoverinfo = "text"
  ) %>%
  layout(
    title = 'Spirit Airlines - Flights Map',
    geo = geo, showlegend = FALSE, height=800
  )



flights_map_spirit2 <- data.frame(group = flights_map_spirit$path,
                                  lat = c(flights_map_spirit$start_lat, flights_map_spirit$end_lat),
                                  long = c(flights_map_spirit$start_long, flights_map_spirit$end_long))
head(flights_map_spirit2)

leaflet() %>% addProviderTiles("CartoDB.Positron",options = providerTileOptions(opacity = 0.8)) %>%
  setView(lng = -95, lat = 40,  zoom = 3) %>% 
  addPolylines(data = flights_map_spirit2, 
               lng = ~long, lat = ~lat, group = ~group, 
               weight = 2, color = "blue")


head(delays_by_path_carr)
delays_by_path_carr_spirit <- subset(delays_by_path_carr,UniqueCarrier == "NK")
head(delays_by_path_carr_spirit)

delays_by_path_carr_spirit$path <- factor(delays_by_path_carr_spirit$path, 
                                          levels = delays_by_path_carr_spirit$path[order(-delays_by_path_carr_spirit$prop_dep_delayed_1H)])

ggplot(data=subset(delays_by_path_carr_spirit, flights_count > 1000),
       aes(x=path, y=prop_dep_delayed_1H,order = desc(prop_dep_delayed_1H))) +
  geom_bar(fill = "#2ebd59", colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("Flights Delayed (%)") + ggtitle("Spirit Airlines - Flights Delayed by Path (%)") +
  scale_y_continuous(labels = percent) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 60, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")

delays_by_path_carr_spirit$path <- factor(delays_by_path_carr_spirit$path, 
                                          levels = delays_by_path_carr_spirit$path[order(-delays_by_path_carr_spirit$avg_delay)])

ggplot(data=subset(delays_by_path_carr_spirit, flights_count > 1000),
       aes(x=path, y=avg_delay,order = desc(avg_delay))) +
  geom_bar(fill = "#2ebd59", colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("Average Delay (min)") + ggtitle("Spirit Airlines - Average Delay by Path") +
  scale_y_continuous(labels = comma) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 60, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")


head(delays_types_by_path_carr_prop_long)

spirit_delayed_major_paths <- subset(delays_by_path_carr_spirit, flights_count > 1500)[7]
spirit_delayed_major_paths <- spirit_delayed_major_paths[1][[1]]

ggplot(data=subset(delays_types_by_path_carr_prop_long,carrier_name == "Spirit Air Lines" & path %in% spirit_delayed_major_paths),
       aes(x=path, y=value, group=variable)) +
  geom_bar(aes(fill = variable), colour = "grey", stat="identity", position=position_dodge()) +
  xlab("") + ylab("% Total Delays") + ggtitle("Reasons for Delays - Top Flights") +
  scale_y_continuous(labels = percent) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 45, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left") +
  scale_fill_manual(values = c("#66c2a5", '#fc8d62', "#8da0cb", "#4daf4a", "#e78ac3"), 
                    labels = c("Carrier", "Weather", "NAS", "Security", "Late Aircraft"), name="Reason")


head(delays_by_carrier_time)

delays_by_carrier_time_spirit <- subset(delays_by_carrier_time,UniqueCarrier == "NK")
head(delays_by_carrier_time_spirit)

delays_by_carrier_time_spirit_long <- melt(delays_by_carrier_time_spirit[,c("FlightHOUR","FlightDOW","prop_arr_delayed_1H")],id=c("FlightDOW","FlightHOUR"))
head(delays_by_carrier_time_spirit_long)

table(delays_by_carrier_time_spirit_long$FlightHOUR)

delays_by_carrier_time_spirit_long$FlightDOW <- factor(delays_by_carrier_time_spirit_long$FlightDOW, 
                                                       levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))

ggplot(data=delays_by_carrier_time_spirit_long,
       aes(x=FlightHOUR, y=value, group=FlightDOW)) +
  geom_line(aes(colour = FlightDOW), size = 1) +
  xlab("") + ylab("% Delayed more than 1H") + ggtitle("Spirit Airlines - % Flights Delayed") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks=0:23)  +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=10),
        legend.justification = "left")

delays_by_carrier_time_spirit_long2 <- melt(delays_by_carrier_time_spirit[,c("FlightHOUR","FlightDOW","flights_count")],id=c("FlightDOW","FlightHOUR"))
head(delays_by_carrier_time_spirit_long2)

delays_by_carrier_time_spirit_long2$FlightDOW <- factor(delays_by_carrier_time_spirit_long2$FlightDOW, 
                                                        levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))

ggplot(data=delays_by_carrier_time_spirit_long2,
       aes(x=FlightHOUR, y=value, group=FlightDOW)) +
  geom_line(aes(colour = FlightDOW), size = 1,alpha=0.8) +
  xlab("") + ylab("Flights") + ggtitle("Spirit Airlines - Flights Count") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks=0:23)  +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=10),
        legend.justification = "left")

delays_by_carrier_time_spirit_long3 <- melt(delays_by_carrier_time_spirit[,c("FlightHOUR","FlightDOW","avg_delay")],id=c("FlightDOW","FlightHOUR"))
head(delays_by_carrier_time_spirit_long3)

delays_by_carrier_time_spirit_long3$FlightDOW <- factor(delays_by_carrier_time_spirit_long3$FlightDOW, 
                                                        levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))

ggplot(data=delays_by_carrier_time_spirit_long3,
       aes(x=FlightHOUR, y=value, group=FlightDOW)) +
  geom_line(aes(colour = FlightDOW), size = 1,alpha=0.8) +
  xlab("") + ylab("Average Delay (min)") + ggtitle("Spirit Airlines - Average Delay") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks=0:23)  +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=10),
        legend.justification = "left")


#######################
# Airlines Comp, only 12...
#######################
head(delays_by_carrier)

library(ggrepel)

ggplot(data=delays_by_carrier,
       aes(x=prop_arr_delayed, y=flights_count)) +
  geom_point(aes(colour = carrier_name), size = 3) +
  xlab("% Delayed") + ylab("Flights") + ggtitle("% Delayed vs Flights Count") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = percent, limits = c(0:1/3)) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=10),
        legend.justification = "left") + 
  geom_text_repel(aes(label=carrier_name,colour=carrier_name)) +
  guides(colour=FALSE)

ggplot(data=delays_by_carrier,
       aes(x=prop_arr_delayed_1H, y=flights_count)) +
  geom_point(aes(colour = carrier_name), size = 3) +
  xlab("% Delayed") + ylab("Flights") + ggtitle("% Delayed vs Flights Count - More than 1 hour") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = percent, limits = c(0:1/10)) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=10),
        legend.justification = "left") + 
  geom_text_repel(aes(label=carrier_name,colour=carrier_name)) +
  guides(colour=FALSE)

