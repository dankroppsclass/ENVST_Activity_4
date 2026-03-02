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




