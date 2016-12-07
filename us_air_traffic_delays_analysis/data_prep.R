rm(list=ls())
cat("\014")

options(scipen=999)

## Loading Libraries
library(reshape2)
library(lubridate)
library(dplyr)
library(doBy)
library(data.table)

setwd('/Users/annabelle/Documents/kaggle/airports/Data/All_data/')

## Airports
airports_data <- read.csv("./airportsnew2.csv")
names(airports_data) <- c("iata","airport_name","airport_city","state","country","lat","long")
head(airports_data)
summary(airports_data)

subset(airports_data, iata == 'DFW')

## Carriers
carriers_data <- read.csv("./carriers.csv")
carriers_data <- carriers_data[!apply(carriers_data == "", 1, all),]
names(carriers_data)[2] <- "carrier_name"
head(carriers_data)
summary(carriers_data)

## Flights
file_names <- dir(path = ".") 
file_names 

all_data <- do.call(rbind,lapply(file_names,read.csv))
head(all_data)
summary(all_data)

cols <- c("DayOfWeek", "FlightDate", "UniqueCarrier", "FlightNum",
          "OriginAirportID", "Origin", "OriginCityName", "OriginStateName",
          "DestAirportID", "Dest", "DestCityName", "DestStateName",
          "CRSDepTime", "DepTime", "DepDelay", "DepDelayMinutes", "DepDel15",
          "CRSArrTime", "ArrTime", "ArrDelay", "ArrDelayMinutes", "ArrDel15",
          "Cancelled", "CancellationCode", "Diverted",
          "AirTime", "ActualElapsedTime", "Distance",
          "CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay")
all_data_select <- all_data[,cols]
all_data_select$FlightDate <-  nice_time2(all_data_select$FlightDate)
all_data_select$FlightDOW <- lubridate::wday(all_data_select$FlightDate, label=TRUE)

all_data_select$FlightHOUR <- as.numeric(ifelse(nchar(all_data_select$CRSDepTime) == 4,substr(all_data_select$CRSDepTime, 0, 2),
                                                     ifelse(nchar(all_data_select$CRSDepTime) == 3,substr(all_data_select$CRSDepTime, 0, 1),0)))

all_data_select$FlightMONTH <- month(all_data_select$FlightDate)

all_data_select$DepDel1H <- ifelse(all_data_select$DepDelayMinutes > 60,1,0)
all_data_select$DepDelay_prop_airtime <- all_data_select$DepDelayMinutes/all_data_select$AirTime

all_data_select$ArrDel1H <- ifelse(all_data_select$ArrDelayMinutes > 60,1,0)
all_data_select$ArrDelay_prop_airtime <- all_data_select$ArrDelayMinutes/all_data_select$AirTime

head(all_data_select)
summary(all_data_select)
str(all_data_select)

all_data_air <- merge(x=all_data_select,y=carriers_data, by.x="UniqueCarrier", by.y="Code", all.x=TRUE)
all_data_air <- merge(x=all_data_air,y=airports_data[,c(1:2)], by.x="Origin", by.y="iata", all.x=TRUE)
all_data_air <- merge(x=all_data_air,y=airports_data[,c(1:2)], by.x="Dest", by.y="iata", all.x=TRUE)
names(all_data_air)[42] <- "airport_name_origin"
names(all_data_air)[43] <- "airport_name_dest"
all_data_air$Dep_delay_flag <- ifelse(all_data_select$DepDelayMinutes > 0,1,0)
all_data_air$Arr_delay_flag <- ifelse(all_data_select$ArrDelayMinutes > 0,1,0)

levels(all_data_air$carrier_name) <- c(levels(all_data_air$carrier_name), "US Airways Inc.") 
all_data_air$carrier_name[all_data_air$UniqueCarrier == "US"]  <- "US Airways Inc." 
all_data_air$carrier_name <- factor(all_data_air$carrier_name)
unique(all_data_air$carrier_name)

all_data_air$FlightYEAR <- as.factor(substr(all_data_air$FlightDate, 0, 4))

head(all_data_air)
summary(all_data_air)

write.csv(all_data_air,file = "./intermediate_data_sets/all_data_air.csv")
