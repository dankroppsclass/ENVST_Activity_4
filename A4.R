install.packages(c("dplyr", "lubridate", "ggplot2"))
library(dplyr)
library(lubridate)
library(ggplot2)

weather <- read.csv("/cloud/project/campus_weather.csv",
                    na.strings = "#N/A")

metaDat <- read.csv("/cloud/project/meter_weather_metadata.csv",
                    na.strings = "#N/A")

sensorLog <- read.csv("/cloud/project/Sensor log.csv",
                      na.strings = "#N/A")

# parse date
weather$dateF <- mdy_hm(weather$Date)
weather$dateET = mdy_hm(weather$Date, tz="America/New_York" )

#weatherCheck = weather %/%
 # filter(is.na(weather$dateET))

# create a month column
weather$doy <- yday(weather$dateF)

# create a year column
weather$year <- year(weather$dateF)

# examine precipitation using a bar plot
ggplot(data=weather[weather$doy > 121 & weather$doy < 274 ,],
       aes(x=dateF,
           y=Precip))+
  geom_col(color="royalblue4")+
  theme_classic()

#interval data: look at first 2 observations as interval
# Time 1 %--% Time 2
weather$dateF[2] %--% weather$dateF[3]
int_length(weather$dateF[2] %--% weather$dateF[3])

test = weather$dateF[1:10]
test[-1]

#x is a date vector
timeCheck900 = function(x){
  intervals = x[-length(x)] %--% x[-1]
  interval_times = int_length(intervals)
  intervals[interval_times != 900]
}

timeCheck900(weather$dateF)



soilFiles <- list.files("/cloud/project/Soil")
soilList = list()

# start an empty list
for(i in 1:length(soilFiles)){
  soilList[[i]] <- read.csv(paste0("/cloud/project/Soil/", soilFiles[i]))
}

str(soilList)

soilData = do.call("rbind", soilList)

# calculate moving average
airMA = numeric()

for(i in 8:length(weather$AirTemp)){
  airMA[i] = mean(weather$AirTemp[(i-7):i])
}

airMA[1:20]

weather$airMA = airMA


#Prompt 2

# examine precipitation using a bar plot
ggplot(data=weather[weather$doy > 121 & weather$doy < 182 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_col(color="royalblue4")+
  theme_classic()

# Homework

#Q1: ensure that there are no issues with the bird excrement or frozen precipitation.
#Exclude any precipitation that occurs when the air temperature is below zero.
#Check that no precipitation measurements are used if the X and Y level observations are more than 2 degrees.

# add a column to weather:
weather$precip.QC = ifelse(weather$doy >= 121 & weather$doy <= 181 & weather$year == 2021, 
                            # evaluate if the doy is between May 1 and June 30 2021
                            NA, # value if true
                            weather$Precip) # value if false: uses original precipitation observation

#when temp is below 0 celcius
weather$FreezeFlag <- ifelse(weather$AirTemp <= 0, # check if at or below zero
                             1, # if true: set flag to 1
                             0) # if false: set flag to zero
#Exclude freezing days from precip data
weather$precip.QC = ifelse(weather$FreezeFlag == 1, #when flagged
                           NA, #set NA as edited precip if true
                           weather$Precip) #if false, leave precip alone

#add x and y filter
weather$xyFlag = ifelse(weather$XLevel >= 2 | weather$XLevel <= -2 | weather$YLevel >= 2 | weather$YLevel <= -2,
                        1,
                        0)
weather$precip.QC = ifelse(weather$xyFlag == 1,
                        NA,
                        weather$Precip)

sum(is.na(weather$precip.QC))

#Q2

weather$BattFlag = ifelse(weather$BatVolt <= 8.5,
                        1,
                        0)
#Q3

RealityCheck <- function(AirTemp, SolRad) {
  flag <- rep("OK", length(AirTemp))
  Bad_Temp <- AirTemp < -35 | AirTemp > 49
  Bad_SolRad <- SolRad < 0 | SolRad > 1750
  flag[Bad_Temp | Bad_SolRad] <- "Unrealistic"
  return(data.frame(AirTemp = AirTemp, SolRad = SolRad, Flag = flag))
}

WeatherCheck <- RealityCheck(weather$AirTemp, weather$SolRad)


#Q4
ggplot(data=weather[weather$doy >= 1 & weather$doy <= 90 & weather$year == 2021,],
       aes(x=dateF,
           y=AirTemp))+
  geom_line(color="royalblue4")+
  theme_classic()

#Q5
#first filter March April and group days
MarchAprilRain = weather %>%
  filter(year==2021) %>%
  filter(doy >= 60, doy <= 120) %>%
  group_by(doy) %>%
  summarise(totPrecip = sum(Precip), minTemp=min(AirTemp))

#then set up for loop
precipNew = as.numeric(NA)

for(i in 2: nrow(MarchAprilRain)){
  precipNew[i] = ifelse(MarchAprilRain$minTemp[i] <= 1.7 | MarchAprilRain$minTemp[i-1] <= 1.7,
                        NA,
                        MarchAprilRain$totPrecip[i])
}

MarchAprilRain$precipNew = precipNew

sum(!is.na(MarchAprilRain$precipNew))


#Q6

soilFiles <- list.files("/cloud/project/Soil")
soilList = list()

# start an empty list
for(i in 1:length(soilFiles)){
  soilList[[i]] <- read.csv(paste0("/cloud/project/Soil/", soilFiles[i]))
}

str(soilList)

soilData = do.call("rbind", soilList)

soilData$date = ymd_hm(soilData$Timestamp) #convert format using lubridate
timeCheck2 = function(x, interval_s){ 
  intervals = x[-length(x)] %--% x[-1] #interval function
  interval_times = int_length(intervals) #converts interval time to seconds
  return(intervals[interval_times != interval_s])} #return intervals that dont match expected time

soilIssues = timeCheck2(soilData$date, 3600)
print(soilIssues)

