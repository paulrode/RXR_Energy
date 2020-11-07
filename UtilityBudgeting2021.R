# Enviroment 
# Loading and preprocessing the data
#Set up enviroment for R scrip  
# Packages for tidyverse 
library("tidyverse")
library("gridExtra")
library("lubridate")
# Package for building tables in markdown and notebook 
library("knitr")
library("kableExtra") 
library("xtable")
# Packages for forecasting
library("fable")
library("tsibble")
library("forecast")
library("tsibbledata")
# Packages for reading excel and html files and XML
library("openxlsx")
library("XML")
# Parkage for using data tables for very large data operations
#library("data.table")
#Package for reading fixed width tables
library("utils")
# Packages for reading data through API's 
library("httr")
library("jsonlite")
# Package for performing inquires with SQL databases 
library("sqldf")
#Package for reading and writing to jpeg files
library("jpeg")

# Set proper working Dir 
if (!getwd() == "C:/Users/paulr/Documents/RXR_Energy") {setwd("C:/Users/paulr/Documents/RXR_Energy")}

# Set proper working Dir
if (!getwd() == "C:/Users/paulr/Documents/RXR_Energy") {setwd("./RXR_Energy")}

# Check for data directory and if one is not present then make it
if (!file.exists("data")) {
  dir.create("data")
}

#Read in data and fix formats

alldata <- read_csv("./data/MASTER METERING DATA(2).csv", col_names = TRUE, col_types = "cccccddcd")
alldata$`Start Date`<- mdy(alldata$`Start Date`)
alldata$`End Date` <- mdy(alldata$`End Date`)
alldata %>% mutate(Month = month(`Start Date`), Period = (alldata$`End Date`- alldata$`Start Date`))  -> alldata

# Summeraze data by building and utility to make a lookup table
alldata %>% group_by(Building, `Meter Type`, `Start Date`, `End Date`) %>% summarise(Usage = sum(Usage)) %>% mutate(Usage_Day = Usage / as.numeric(`End Date` - `Start Date`)) -> alldata

#Set up main utility data collection table 
UtilityData <- data.frame(building = "builidng", datekey = "January 2017", kWh = 1, date = ymd("2017-01-05"), HDD = 1, CDD = 1, varriable = 1 )
UtilityData[-1,] -> UtilityData

# Bring in DD and then join days per month to get a dataframe with DD/day to then combine with the main alldata
hdd <- read_csv("./data/NYSERDA HDD.csv")
colnames(hdd)[1] <- "Month"
hdd %>% select(1:5) %>% gather(key = "key", value = "HDD", -1) %>%
  mutate(datekey = paste(Month,key)) %>% select(datekey, HDD, -Month,-key) -> hdd
hdd[is.na(hdd)] = 0 # get rid of NA's

cdd <- read_csv("./data/NYSERDA CDD.csv")
colnames(cdd)[1] <- "Month"
cdd %>% select(1:5) %>% gather(key = "key", value = "CDD", -1) %>%
  mutate(datekey = paste(Month,key)) %>% select(datekey, CDD, -Month,-key) -> cdd
cdd[is.na(cdd)] = 0 # get rid of NA's

#Make a unique building and meter type vectors
buildings <- unique(alldata$Building)
units <- data.frame("type" = unique(alldata$`Meter Type`), "unit" = c("kWh", "MLbs", "hcf", "hcf", "hcf", "gal", "Therms", "hcf"))

# Create a df with all days of the year for normarilizing useage per month
for(j in 1:length(buildings)) {
  
  # Make meter type vector
  alldata %>% filter(Building == buildings[j]) -> Types
  unique(Types$`Meter Type`) -> Types
  
  for(l in 1:length(Types)) {
    
    filter(alldata, Building == buildings[j] & `Meter Type` == Types[l]) ->   Elect
    
    alldata_perday <-  data.frame(dates = seq((Elect$`End Date`[1]), (Elect$`End Date`[length(Elect$`End Date`)]), by = "day"))  
    
    # Using filters and brackets look up kWh/day for each day of the year 
    upd <- data.frame(UsagePerDay = seq(1:length(alldata_perday$dates)))
    
    for(i in 1:length(alldata_perday$dates)) {
      date <- alldata_perday$dates[i]
      filter(Elect, date > `Start Date` & date <= `End Date`) -> Elect1
      sum(Elect1[6]) -> upd[i,1]
    }
    
    upd[1,1] <- Elect[1,6]
    cbind(alldata_perday, upd) -> dailyElect
    dailyElect %>% group_by(year(dates), month(dates)) %>% summarise(useage = sum(UsagePerDay)) %>%
      mutate("date" = ymd(paste(`year(dates)`, `month(dates)`, 15, sep = '-')))  ->
      monthlyElect
    monthlyElect %>% mutate("month" = month(date, label = TRUE, abbr = FALSE)) %>%
      mutate("building" = buildings[j]) %>% mutate("type" = Types[l]) -> monthlyElect
    colnames(monthlyElect) = c("year", "month1", "useage", "date", "month", "building", "type")
    monthlyElect <- as.data.frame(monthlyElect)
    monthlyElect %>% mutate(datekey = paste(`month`,`year`)) -> monthlyElect
    monthlyElect %>% select(building, type, datekey, useage, date) -> monthlyElect
    monthlyElect %>% left_join(hdd, by = "datekey" ) -> monthlyElect
    monthlyElect %>% left_join(cdd, by = "datekey") -> monthlyElect
    monthlyElect[-1,] -> monthlyElect
    monthlyElect %>% mutate(varriable = useage - min(monthlyElect$useage)) -> monthlyElect
    rbind(UtilityData, monthlyElect) -> UtilityData 
    
  }}

#add units
UtilityData %>% left_join(units) -> UtilityData

#Trim out covid months 
UtilityData %>% filter(date < ymd("2020-03-15")) -> UtilityDataNoCovid

k <- 4

# Do a time series forcast using Fabel's Arima function
UtilityDataNoCovid %>%filter(building == building[k] & type == "Electric") %>% select(date, useage, HDD, CDD)  -> Elect
tsibble(Elect) -> ElectTS
ElectTS %>% ARIMA() -> electFIT

#################################################
# Fable analysis
#################################################

# table specification is index (date), demand, Workday, Temperture. 
#I will use index, consumption, Workday, DD = cdd-hdd
UtilityData %>% filter(building == "237 Park" & type == "Electric") %>% mutate("DD" = CDD - HDD) %>% mutate("WorkDay" = wday(date)) %>% select(date, useage, DD, WorkDay) %>% as.tibble() -> UtilityTS

UtilityTS %>% ARIMA(useage ~ CDD + HDD,  ic = c("aicc", "aic", "bic"))

model(ARIMA(elec, useage ~ DD))


# Do a stright plot of consumption 
UtilityDataNoCovid %>% filter(type == "Electric") %>%
  ggplot(aes(date, varriable, color = "green")) +
  geom_line() +
  facet_grid(building~.)

# Plot the linear model of summer months and CDD
UtilityDataNoCovid %>% ggplot( aes(CDD, varriable, color = "green")) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=TRUE) +
  facet_grid(building~.)

k <- 2
#Heating
UtilityDataNoCovid %>% filter(building == buildings[k] & type == "Steam") %>% filter(month(date) < 4 | month(date) > 10  ) %>% ggplot( aes(HDD, useage, color = "blue")) +    geom_point() +
  geom_smooth(method=lm , color="red", se=TRUE) 

#Cooling 
UtilityDataNoCovid %>% filter(building == buildings[k] & type == "Steam") %>% filter(month(date) > 5 & month(date) < 9  ) %>% ggplot( aes(CDD, useage, color = "black")) +    geom_point() +
  geom_smooth(method=lm , color="blue", se=TRUE) 
