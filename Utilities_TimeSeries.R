###############################################################################
#   Enviroment Setup                                                          #
###############################################################################

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
library("feasts")
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

###############################################################################
                            # Read in data
###############################################################################
#Read in data and fix formats

alldata <- read_csv("./data/MASTER METERING DATA(2).csv", col_names = TRUE, col_types = "ccc??ddcdc")
alldata$`Start Date`<- mdy(alldata$`Start Date`)
alldata$`End Date` <- mdy(alldata$`End Date`)
colnames(alldata)[2] <- "Type"
units <- data.frame("Type" = unique(alldata$Type), "unit" = c("kWh", "MLbs", "hcf", "hcf", "hcf", "gal", "Therms", "hcf"))
alldata <- alldata %>% select(-Units) %>% left_join(units, by = "Type")

load("./data/Utilitydata") # To be used later for normarilizing over a true month. 
load("./data/UtilitydataNoCovid") #Same as above.

# Summeraze data by building and utility to make a lookup table
UseagePerDay <-  alldata %>% group_by(Building, `Type`, `Start Date`, `End Date`) %>% 
  summarise(Usage = sum(Usage)) %>% 
  mutate(days = as.numeric(`End Date` - `Start Date`), Usage_Day = Usage / days)

# Bring in DD 
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




###############################################################################
                             # Lood at 1330 AoA                                            
###############################################################################

elect1330nocovid <- UseagePerDay %>% filter(Building == "1330 AoA" & `Type` == "Electric")  %>%
  mutate(index = yearmonth(`End Date`)) %>% filter(`Start Date` < ymd("2020-02-15")) %>%
  arrange(`End Date`)

qplot(`End Date`, Usage, data = elect1330nocovid, geom = "line")


elec1330 <- as_tsibble(elect1330nocovid, index =index, key = c(Building, Type), regular = TRUE)
str(elec1330)
glimpse(elec1330)

elec1330 %>%
  filter(`Start Date` < ymd("2020-02-15")) %>% 
  model(
    ets = ETS(box_cox(Usage, 0.3)),
    arima = ARIMA(log(Usage)),
    snaive = SNAIVE(Usage)
  ) %>%
  forecast(h = "1 years") %>% 
  autoplot(elec1330, level = NULL)

###############################################################################
                            # Using normarilized dataframe
###############################################################################

elec1330 <- as_tsibble(UtilityData, index = date, key = c(building, type), regular = TRUE)
autoplot(elec1330, useage)
glimpse(elec1330)

elec1330 %>%  filter(`date` < ymd("2020-02-15")) %>% 
  model(
    ets = ETS(box_cox(useage, 0.3)),
    arima = ARIMA(log(useage)),
    snaive = SNAIVE(useage)
  ) %>%
  forecast(h = "1 years") %>% 
  autoplot(elec1330, level = NULL)
