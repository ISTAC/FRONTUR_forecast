
# Title: Forecast tourist movements in the border of the Canary Islands (FRONTUR)
# Description: This script obtain forecast tourist movements in the border of the Canary Islands 
#              employing State Space Models
# Version: 0.1.0
# Author: Elisa M. Jorge González <elisajg0@gmail.com>


######################
# LOADING R PACKAGES #
######################
# To read xls and xlsx files
library(readxl)
# To export xls and xlsx files
library(xlsx)
# To execute State Space Models
library(KFAS)
# To read json
library(jsonlite)
# To database treatment
library(dplyr)
# To data treatment
library(varhandle)


#######################
# LOADING R FUNCTIONS #
#######################
# Function to get data from JSON
source("R/json_data.R", encoding = 'UTF-8') 
# Function to get forecast
source("R/forecast.R", encoding = 'UTF-8') 


#######################
# READ DATA FROM JSON #
#######################
# Url of Total tourist movements in the border of the Canary Islands
CAN_json     <- "http://www.gobiernodecanarias.org/istac/jaxi-istac/tabla.do?accion=jsonMtd&uuidConsulta=d17050d0-1e73-450c-8648-62390c625b17"
# Url of tourist movements in the border by islands
ISLANDS_json <- "http://www.gobiernodecanarias.org/istac/jaxi-istac/tabla.do?accion=jsonMtd&uuidConsulta=5fd35c5b-26dc-4aaa-975d-9254c6dee11e"   

# Get data from json
# Total tourist movements in the border of the Canary Islands data
CAN <- json_data(CAN_json , "TOTAL")
# Tourist movements in the border by islands data
ISLANDS <- json_data(ISLANDS_json, "ISLANDS")

# Movements in the border of the Canary Islands data
DATA <- cbind(as.character(CAN$Date),as.numeric_version(CAN$Value),ISLANDS)
colnames(DATA) <- c("DATE","CAN",colnames(ISLANDS))

# Number of available data
m <- nrow(DATA)


####################################
# READ DATA FROM XLS OR XLSX FILES #
####################################
# Exogenous variables
Calendar <- read.xlsx("data/Calendar.xls", sheetIndex = 1)
# Number of available date
d <- nrow(Calendar)


#########################
# TIME SERIES STRUCTURE #
#########################
# Data to forecast
Data_ts <- as.data.frame(ts(subset(DATA, select = -DATE), start = c(2010,01), frequency = 12))
Data_ts <- ts(rbind(subset(DATA, select = -DATE), matrix(NA, ncol = ncol(subset(DATA, select = -DATE)), 
                                                         nrow = 6)), 
              start = c(2010,01), 
              frequency = 12)
# Exogenous variables
Calendar_ts <- ts(Calendar[,2:ncol(Calendar)], 
                  start = c(2010,01), 
                  end = time(Data_ts)[nrow(Data_ts)], 
                  frequency = 12)


################
# GET FORECAST #
################
Z <- get_forecast(Data_ts,Calendar_ts,m)


######################
# EXTRACT THE VALUES #
######################
# Forecast values
Prediction <- round(Z$Prediction,0)
colnames(Prediction) <- colnames(DATA)[2:ncol(DATA)]
# MAPE values
MAPE <- Z$MAPE


##########
# OUTPUT #
##########
# Input data
write.xlsx(DATA, file = "output/Forecast.xls", sheetName = "Data", append = FALSE)
# Forecast data
Date_f   <- as.character(Calendar[(nrow(DATA) + 1):nrow(Data_ts),1])
Forecast <- cbind(Date_f,Prediction[(nrow(DATA) + 1):nrow(Data_ts),])
write.xlsx(Forecast, file = "output/Forecast.xls", sheetName = "Forecast", append = TRUE)
# MAPE values
colnames(MAPE) <- colnames(DATA)[2:ncol(DATA)]
write.xlsx(MAPE, file = "output/Forecast.xls", sheetName = "MAPE", append = TRUE)

