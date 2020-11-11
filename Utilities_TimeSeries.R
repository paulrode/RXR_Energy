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

#Read in data and fix formats

alldata <- read_csv("./data/MASTER METERING DATA(2).csv", col_names = TRUE, col_types = "ccc??ddcdc")
alldata$`Start Date`<- mdy(alldata$`Start Date`)
alldata$`End Date` <- mdy(alldata$`End Date`)

# Summeraze data by building and utility to make a lookup table
UseagePerDay <-  alldata %>% group_by(Building, `Meter Type`, `Start Date`, `End Date`) %>% 
  summarise(Usage = sum(Usage)) %>% 
  mutate(Usage_Day = Usage / as.numeric(`End Date` - `Start Date`), span = interval(ymd(`Start Date`),ymd(`End Date`)) )


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
units <- data.frame("type" = unique(alldata$`Meter Type`), "unit" = c("kWh", "MLbs", "hcf", "hcf", "hcf", "gal", "Therms", "hcf"))




###############################################################################
#                 Lood at 1330 AoA                                            #
###############################################################################

elect450 <- UseagePerDay %>% filter(Building == "450 Lex" & `Meter Type` == "Electric") %>% 
  mutate(date.index = yearmonth(`End Date`)) %>% filter(`Start Date` < ymd("2020-02-15"))

qplot(`End Date`, Usage, data = elect450, geom = "line")


TSelect450 <- as_tsibble(elect450, index = `date.index`, regular = TRUE)

TSelect450 %>% autoplot(Usage)

  fit <- TSelect1330 %>% 
  model(auto_ets = ETS(Usage))
fit
report(fit)
glance(fit)

fc <- fit %>% 
  forecast(h = "2 years")
fc

fc %>% 
  autoplot(TSelect1330)
