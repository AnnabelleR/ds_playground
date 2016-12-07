rm(list=ls())
cat("\014")

options(scipen=999)

library(lubridate)
library(dplyr)
library(data.table)
library(ggplot2)
library(scales)
library(doBy)
library(reshape)
library(proj4)

length_unique <- function(x) {
  return (length(unique(x)))
}

give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

factor_to_numeric <- function(x) {
  return (as.numeric(as.character(x)))
}

setwd('/Users/annabelle/Documents/Black_spots/UK_Report/')

#######################################################################################
## Accident Factors
#######################################################################################
### Data for 2009-2014: 899,995 accidents

## Road Type
road_types <- read.table("../Data_doc/7752_road-accident-safety-data-guide/Road_Type_Table1.csv",
                         sep=",", header=TRUE, as.is=TRUE)
road_types <- road_types[,c(1:2)]
names(road_types)[2] <- "road_label"
head(road_types)

road_types_all <- read.table("./road_types/part-00000", sep=",", header=TRUE, as.is=TRUE)
head(road_types_all)
summary(road_types_all)


road_types_all <- merge(x=road_types_all, y=road_types,
                               by.x="Road_Type", by.y = "code")
head(road_types_all)

sum(road_types_all$accidents)

road_types_all$road_label <- factor(road_types_all$road_label, 
                                        levels = road_types_all$road_label[order(-road_types_all$accidents)])

ggplot(data=road_types_all,
       aes(x=road_label, y=accidents, order = desc(accidents))) +
  geom_bar(fill = "#06436d", colour = "grey", stat="identity") +
  scale_y_continuous(labels = comma) +
  xlab("") + ylab("Accidents") + ggtitle("Road Types") +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size=12),
        legend.justification = "left") 

## Junction Details
junction_details <- read.table("../Data_doc/7752_road-accident-safety-data-guide/Junction_Detail_Table_1.csv",
                               sep=",", header=TRUE, as.is=TRUE)
junction_details <- junction_details[,c(1:2)]
names(junction_details)[2] <- "junction_details_label"
head(junction_details)

junction_details_all <- read.table("./junction_details/part-00000", sep=",", header=TRUE, as.is=TRUE)
head(junction_details_all)

junction_details_all <- merge(x=junction_details_all, y=junction_details,
                        by.x="Junction_Detail", by.y = "code")
head(junction_details_all)

sum(junction_details_all$accidents)

junction_details_all$junction_details_label <- factor(junction_details_all$junction_details_label, 
                                    levels = junction_details_all$junction_details_label[order(-junction_details_all$accidents)])

ggplot(data=junction_details_all,
       aes(x=junction_details_label, y=accidents, order = desc(accidents))) +
  geom_bar(fill = "#06436d", colour = "grey", stat="identity") +
  scale_y_continuous(labels = comma) +
  xlab("") + ylab("Accidents") + ggtitle("Junction Details") +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10, angle = 45, hjust = 1),
        legend.text = element_text(size=12),
        legend.justification = "left") 

## Date & Time
dow <- read.table("../Data_doc/7752_road-accident-safety-data-guide/Day_of_Week_Table_1.csv",
                               sep=",", header=TRUE, as.is=TRUE)
dow <- dow[,c(1:2)]
names(dow)[2] <- "Weekday"
head(dow)

datetime <- read.table("./datetime/part-00000", sep=",", header=TRUE, as.is=TRUE)
head(datetime)

datetime <- merge(x=datetime, y=dow,
                              by.x="Day_of_Week", by.y = "code")
head(datetime)

datetime <- datetime[order(datetime$Day_of_Week),]

datetime$Weekday <- factor(datetime$Weekday,
                       levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(data=datetime,
       aes(x=hour, y=accidents)) +
  geom_line(aes(group=Weekday), colour="#06436d", size=1) +
  xlab("") + ylab("Accidents") + ggtitle("Accidents by Hour and Day") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(breaks=c(0:23)) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size = 8),
        legend.text = element_text(size=12),
        legend.justification = "left") + facet_wrap(~ Weekday, scales = "free_x")

## Accidents by Year
accidents_by_year <- read.table("./accidents_by_year.csv",
                  sep=",", header=TRUE, as.is=TRUE)
accidents_by_year <- accidents_by_year[,c(2:3)]
accidents_by_year <- accidents_by_year[1:6,]
head(accidents_by_year)

ggplot(data=accidents_by_year,
       aes(x=year, y=accidents)) +
  geom_bar(fill = "#06436d", colour = "grey", stat="identity") +
  xlab("") + ylab("Accidents") + ggtitle("Accidents by Year") +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=12),
        legend.text = element_text(size=12),
        legend.justification = "left") 

## Urban vs Rural
urban_rural <- read.table("./urban_rural.csv",
                                sep=",", header=TRUE, as.is=TRUE)
urban_rural <- urban_rural[,c(2:3)]
urban_rural <- urban_rural[1:2,]
urban_rural$type <- ifelse(urban_rural$Urban_or_Rural_Area == 1, "Urban", "Rural")
head(urban_rural)

ggplot(data=urban_rural,
       aes(x=type, y=accidents)) +
  geom_bar(fill = "#06436d", colour = "grey", stat="identity") +
  xlab("") + ylab("Accidents") + ggtitle("Accidents by Type") +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=12),
        legend.text = element_text(size=12),
        legend.justification = "left") 

## Number of Vehicles
number_vehicles <- read.csv("./number_vehicles.csv",
                          sep=",", header=TRUE, as.is=TRUE)
number_vehicles <- number_vehicles[,c(2:3)]
#number_vehicles$accidents <- as.integer(number_vehicles$accidents)
head(number_vehicles)
summary(number_vehicles)

ggplot(data=number_vehicles,
       aes(x=Number_of_Vehicles, y=accidents)) +
  geom_bar(fill = "#06436d", colour = "grey", stat="identity") +
  xlab("") + ylab("Accidents") + ggtitle("Accidents by Number of Vehicles") +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=12),
        legend.text = element_text(size=12),
        legend.justification = "left") 

### Vehicles Stats (1,643,560)

## Gender
sod <- read.table("../Data_doc/7752_road-accident-safety-data-guide/Sex_of_Driver_Table_1.csv",
                  sep=",", header=TRUE, as.is=TRUE)
sod <- sod[,c(1:2)]
sod <- sod[1:4,]
head(sod)

sex_driv <- read.table("./sex_driv/part-00000", sep=",", header=TRUE, as.is=TRUE)
head(sex_driv)

sex_driv <- merge(x=sex_driv, y=sod,
                  by.x="Sex_of_Driver", by.y = "code")

ggplot(data=sex_driv,
       aes(x=label, y=count)) +
  geom_bar(fill = "#06436d", colour = "grey", stat="identity") +
  xlab("") + ylab("Accidents") + ggtitle("Accidents by Gender") +
  scale_y_continuous(labels = comma) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=12),
        legend.text = element_text(size=12),
        legend.justification = "left") 

## Age
age_dat <- read.table("../Data_doc/7752_road-accident-safety-data-guide/Age_Band_Table_1.csv",
                  sep=",", header=TRUE, as.is=TRUE)
age_dat <- age_dat[,c(1:2)]
head(age_dat)

age_driv <- read.table("./age_band/part-00000", sep=",", header=TRUE, as.is=TRUE)
head(age_driv)

age_driv <- merge(x=age_driv, y=age_dat,
                  by.x="Age_Band_of_Driver", by.y = "code")

age_driv$label <- factor(age_driv$label,
                           levels = c("0 - 5", "6 - 10", "11 - 15", "16 - 20", "21 - 25", "26 - 35", "36 - 45",
                                      "46 - 55", "56 - 65", "66 - 75", "Over 75"))

ggplot(data=age_driv,
       aes(x=label, y=count)) +
  geom_bar(fill = "#06436d", colour = "grey", stat="identity") +
  xlab("") + ylab("Accidents") + ggtitle("Accidents by Age") +
  scale_y_continuous(labels = comma) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=12),
        legend.text = element_text(size=12),
        legend.justification = "left") 

## Vehicle Type
vehicle_types <- read.table("../Data_doc/7752_road-accident-safety-data-guide/Vehicle_Type_Table_1.csv",
                            sep=",", header=TRUE, as.is=TRUE)
vehicle_types <- vehicle_types[,c(1:2)]
names(vehicle_types)[2] <- "vehicle_label"
head(vehicle_types)

veh_type <- read.table("./Vehicle_Type/part-00000", sep=",", header=TRUE, as.is=TRUE)
head(veh_type)

veh_type <- merge(x=veh_type, y=vehicle_types,
                  by.x="Vehicle_Type", by.y = "code")

veh_type$vehicle_label <- factor(veh_type$vehicle_label, 
                                    levels = veh_type$vehicle_label[order(-veh_type$count)])

ggplot(data=veh_type,
       aes(x=vehicle_label, y=count, order=desc(count))) +
  geom_bar(fill = "#06436d", colour = "grey", stat="identity") +
  xlab("") + ylab("Accidents") + ggtitle("Vehicles involved in Accidents") +
  scale_y_continuous(labels = comma) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10, angle = 45, hjust = 1),
        legend.text = element_text(size=12),
        legend.justification = "left") 

## Vehicle Type Associations
veh_type_asso <- read.table("./Vehicle_Type_assoc/part-00000", sep=",", header=TRUE, as.is=TRUE)
head(veh_type_asso)

veh_type_asso <- merge(x=veh_type_asso, y=vehicle_types,
                  by.x="type1", by.y = "code")
names(veh_type_asso)[4] <- "vehicle_label1"

veh_type_asso <- merge(x=veh_type_asso, y=vehicle_types,
                       by.x="type2", by.y = "code")
names(veh_type_asso)[5] <- "vehicle_label2"

veh_type_asso <- veh_type_asso[,c(3:5)]
veh_type_asso <- veh_type_asso[order(-veh_type_asso$accidents),]

veh_type_asso$key1 <- paste(veh_type_asso$vehicle_label1,veh_type_asso$vehicle_label2, sep = "-")
veh_type_asso$key2 <- paste(veh_type_asso$vehicle_label2,veh_type_asso$vehicle_label1, sep = "-")

veh_type_asso2 <- merge(x=veh_type_asso[,c("accidents", "key1")], y=veh_type_asso[,c("accidents", "key2")],
                       by.x="key1", by.y = "key2")

veh_type_asso2$total_accidents <- veh_type_asso2$accidents.x + veh_type_asso2$accidents.y

head(veh_type_asso2)

veh_type_asso2 <- veh_type_asso2[order(-veh_type_asso2$total_accidents),]
veh_type_asso2_top10 <- veh_type_asso2[1:10,]

veh_type_asso2_top10$key1 <- factor(veh_type_asso2_top10$key1, 
                                 levels = veh_type_asso2_top10$key1[order(-veh_type_asso2_top10$total_accidents)])

veh_type_asso2_top5 <- veh_type_asso2_top10[c(1,3,5,7,9),]

ggplot(data=veh_type_asso2_top5,
       aes(x=key1, y=total_accidents, order=desc(total_accidents))) +
  geom_bar(fill = "#06436d", colour = "grey", stat="identity") +
  xlab("") + ylab("Accidents") + ggtitle("Top Vehicles Associations involved in Accidents") +
  scale_y_continuous(labels = comma) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10, angle = 45, hjust = 1),
        legend.text = element_text(size=12),
        legend.justification = "left") 

## Manoeuvre Type
vehicle_manoeuvre <- read.table("../Data_doc/7752_road-accident-safety-data-guide/Vehicle_Manoeuvre_Table_1.csv",
                                sep=",", header=TRUE, as.is=TRUE)
vehicle_manoeuvre <- vehicle_manoeuvre[,c(1:2)]
names(vehicle_manoeuvre)[2] <- "manoeuvre_label"
head(vehicle_manoeuvre)

veh_man <- read.table("./Vehicle_Manoeuvre/part-00000", sep=",", header=TRUE, as.is=TRUE)
head(veh_man)

veh_man <- merge(x=veh_man, y=vehicle_manoeuvre,
                  by.x="Vehicle_Manoeuvre", by.y = "code")

veh_man$manoeuvre_label <- factor(veh_man$manoeuvre_label, 
                                 levels = veh_man$manoeuvre_label[order(-veh_man$count)])

ggplot(data=veh_man,
       aes(x=manoeuvre_label, y=count, order=desc(count))) +
  geom_bar(fill = "#06436d", colour = "grey", stat="identity") +
  xlab("") + ylab("Accidents") + ggtitle("Manoeuvres involved in Accidents") +
  scale_y_continuous(labels = comma) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10, angle = 45, hjust = 1),
        legend.text = element_text(size=12),
        legend.justification = "left") 

## Manoeuvre Associations
man_type_asso <- read.table("./Vehicle_Manoeuvre_assoc/part-00000", sep=",", header=TRUE, as.is=TRUE)
head(man_type_asso)

man_type_asso <- merge(x=man_type_asso, y=vehicle_manoeuvre,
                       by.x="Vehicle_Manoeuvre1", by.y = "code")
names(man_type_asso)[4] <- "Vehicle_Manoeuvre1"

man_type_asso <- merge(x=man_type_asso, y=vehicle_manoeuvre,
                       by.x="Vehicle_Manoeuvre2", by.y = "code")
names(man_type_asso)[5] <- "Vehicle_Manoeuvre2"

man_type_asso <- man_type_asso[,c(3:5)]
man_type_asso <- man_type_asso[order(-man_type_asso$accidents),]

man_type_asso$key1 <- paste(man_type_asso$Vehicle_Manoeuvre1,man_type_asso$Vehicle_Manoeuvre2, sep = "-")
man_type_asso$key2 <- paste(man_type_asso$Vehicle_Manoeuvre2,man_type_asso$Vehicle_Manoeuvre1, sep = "-")

man_type_asso2 <- merge(x=man_type_asso[,c("accidents", "key1")], y=man_type_asso[,c("accidents", "key2")],
                        by.x="key1", by.y = "key2")

man_type_asso2$total_accidents <- man_type_asso2$accidents.x + man_type_asso2$accidents.y

head(man_type_asso2)

man_type_asso2 <- man_type_asso2[order(-man_type_asso2$total_accidents),]
man_type_asso2_top10 <- man_type_asso2[1:10,]

man_type_asso2_top10$key1 <- factor(man_type_asso2_top10$key1, 
                                    levels = man_type_asso2_top10$key1[order(-man_type_asso2_top10$total_accidents)])

man_type_asso2_top5 <- man_type_asso2_top10[c(1,3,5,7,9),]

ggplot(data=man_type_asso2_top5,
       aes(x=key1, y=total_accidents, order=desc(total_accidents))) +
  geom_bar(fill = "#06436d", colour = "grey", stat="identity") +
  xlab("") + ylab("Accidents") + ggtitle("Top Manoeuvre Associations involved in Accidents") +
  scale_y_continuous(labels = comma) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10, angle = 45, hjust = 1),
        legend.text = element_text(size=12),
        legend.justification = "left") 

## Weather
weather <- read.table("../Data_doc/7752_road-accident-safety-data-guide/Weather_Table_1.csv",
                                sep=",", header=TRUE, as.is=TRUE)
weather <- weather[,c(1:2)]
names(weather)[2] <- "weather_label"
head(weather)

weather_dat <- read.table("./weather/part-00000", sep=",", header=TRUE, as.is=TRUE)
head(weather_dat)

weather_dat <- merge(x=weather_dat, y=weather,
                 by.x="Weather_Conditions", by.y = "code")

weather_dat$weather_label <- factor(weather_dat$weather_label, 
                                  levels = weather_dat$weather_label[order(-weather_dat$accidents)])

ggplot(data=weather_dat,
       aes(x=weather_label, y=accidents, order=desc(accidents))) +
  geom_bar(fill = "#06436d", colour = "grey", stat="identity") +
  xlab("") + ylab("Accidents") + ggtitle("Accidents by Weather Conditions") +
  scale_y_continuous(labels = comma) +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(size=10, angle = 45, hjust = 1),
        legend.text = element_text(size=12),
        legend.justification = "left") 

#######################################################################################
## Map of Accidents
#######################################################################################
data_all_year_agg <- read.table("../accidents_data_all_years_aggregated.csv",
                                sep=",", header=TRUE, as.is=TRUE)
head(data_all_year_agg)
summary(data_all_year_agg)

data_all_year_agg_more_select <- subset(data_all_year_agg, total_accidents >= 20)
head(data_all_year_agg_more_select)
summary(data_all_year_agg_more_select)

pal_acc <- colorNumeric(
  palette = colorRamp(c("tan2","orangered3","red", "red4")),
  domain = data_all_year_agg_more_select$total_accidents
)

leaflet(data_all_year_agg_more_select) %>% 
  addTiles(options = providerTileOptions(opacity = 0.2)) %>%
  addCircleMarkers(
    ~long, ~lat, 
    radius = 4,
    color = ~pal_acc(total_accidents),
    popup=paste("Latitude:", data_all_year_agg_more_select$lat, "<br>", 
                "Longitude:", data_all_year_agg_more_select$long, "<br>",
                "Total Accidents:", data_all_year_agg_more_select$total_accidents, "<br>",
                "Total Casualties:", data_all_year_agg_more_select$total_casualties),
    stroke = FALSE, fillOpacity = 1
    #clusterOptions = markerClusterOptions()
  ) %>%
  addLegend("bottomright", pal = pal_acc, values = data_all_year_agg_more_select$total_accidents,
            title = "Total Accidents",
            opacity = 1) 

#######################################################################################
## Map of Accidents Adjusted for traffic data
#######################################################################################

aadf_major <- read.csv("../AADF-data-major-roads.csv",
                       sep=",", header=TRUE, as.is=TRUE)

aadf_minor <- read.csv("../AADF-data-minor-roads.csv",
                       sep=",", header=TRUE, as.is=TRUE)

aadf_all <- rbind(aadf_major[,c("AADFYear", "CP", "S.Ref.E", "S.Ref.N", "Road", "FdCar")],
                  aadf_minor[,c("AADFYear", "CP", "S.Ref.E", "S.Ref.N", "Road", "FdCar")])

proj4string <- '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'
xy <- data.frame(x=aadf_all$S.Ref.E,y=aadf_all$S.Ref.N)
pj <- project(xy, proj4string, inverse=TRUE)
latlon <- data.frame(lat=pj$y, lon=pj$x)
aadf_all$latitude <- latlon$lat
aadf_all$longitude <- latlon$lon
aadf_all$long <- round(as.numeric(aadf_all$longitude), 3)
aadf_all$lat <- round(as.numeric(aadf_all$latitude), 3)
aadf_all <- subset(aadf_all, AADFYear >= 2009)
head(aadf_all)

traffic_info <- summaryBy(FdCar ~ long + lat, data = aadf_all, FUN = mean)
head(traffic_info)

head(data_all_year_agg_more_select)

traff_acc <- merge(x=traffic_info, y=data_all_year_agg_more_select,
                    by=c("lat","long"), all.y = TRUE)

traff_acc$crash_rate <- (1000000*traff_acc$total_accidents)/(365*6*traff_acc$FdCar.mean)

head(traff_acc)
summary(traff_acc)

quantile(traff_acc$FdCar.mean, prob = seq(0, 1, length = 21), type = 5, na.rm = TRUE)
traff_acc <- subset(traff_acc, FdCar.mean > 1000)

pal_dang <- colorNumeric(
  palette = colorRamp(c("tan2","orangered3","red", "red4")),
  domain = traff_acc$crash_rate
)

leaflet(traff_acc) %>% 
  addTiles(options = providerTileOptions(opacity = 0.2)) %>%
  addCircleMarkers(
    ~long, ~lat, 
    radius = 4,
    color = ~pal_dang(crash_rate),
    popup=paste("Latitude:", traff_acc$lat, "<br>", 
                "Longitude:", traff_acc$long, "<br>",
                "Daily Car Traffic:", round(traff_acc$FdCar.mean,0), "<br>",
                "Total Accidents:", traff_acc$total_accidents, "<br>",
                "Crash Rate:", round(traff_acc$crash_rate,3), "<br>",
                "Total Casualties:", traff_acc$total_casualties),
    stroke = FALSE, fillOpacity = 1
  ) %>%
  addLegend("bottomright", pal = pal_dang, values = traff_acc$crash_rate,
            title = "Crash Rate",
            opacity = 1) 

#######################################################################################
## Trips Passing near Accident Black Spots
#######################################################################################
trips_passing_danger_zones_proc <- read.table("../trips_passing_danger_zones_proc/part-00000",
                                              sep=",", header=TRUE, as.is=TRUE)

head(trips_passing_danger_zones_proc)
summary(trips_passing_danger_zones_proc)

unique(subset(trips_passing_danger_zones_proc, danger_zones > 10)[,c("tripId", "danger_zones")])

thetrip <- '1459627567-1005290992' 
thedata <- subset(trips_passing_danger_zones_proc, tripId == thetrip)

pal_speed <- colorNumeric(
  palette = "YlOrRd",
  domain = thedata$speed
)

leaflet(thedata) %>% 
  addTiles(options = providerTileOptions(opacity = 0.2)) %>%
  addCircleMarkers(
    ~lng, ~lat, 
    radius = 3,
    popup = ~as.character(tripId),
    color = ~pal_speed(speed),
    stroke = FALSE, fillOpacity = 1
  ) %>%
  addCircleMarkers(
    data = data_all_year_agg_more_select,
    ~long, 
    ~lat, 
    radius = 3,
    color = "blue",
    stroke = FALSE, fillOpacity = 1,
    popup = paste("Total Accidents:", data_all_year_agg_more_select$total_accidents)) %>%
  addLegend("bottomright", pal = pal_speed, values = thedata$speed,
            title = "Speed",
            opacity = 1) 