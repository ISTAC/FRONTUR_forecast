# Data from JSON
#
# Author: Elisa M. Jorge González <elisajg0@gmail.com>

#' @title Get data from JSON
#' @description Get data from ISTAC website of tourist movements in the border (FRONTUR)
#' @param X Url 
#' @param Y Type of data 
#          "TOTAL"   -> tourist movements in the border in total
#          "ISLANDS" -> tourist movements in the border by islands
#' @return DATA Data obtained from url
#' @example 
#'  json_data(myurl, "TOTAL")
#'  jdon_data(myurl, "ISLANDS")


json_data <- function(X,Y){
  
  # DATA 
  # Read data from url
  Data <- fromJSON(url(X))
  # Get value, territory and date data
  Data <- Data$data
  # Get dimension codes
  t <- t(as.data.frame(Data$dimCodes))
  # Paste data and dimension codes in a data.frame
  Data <- cbind(Data,t) 
  
  # SELECT DATA BY TERRITORY
  if (Y == "TOTAL")
  {# Select the total of tourist
    Data <- subset(Data, Data[,3] == "T") 
    # Sort in ascending order
    Data <- arrange(Data, Data[,4])
    colnames(Data) <- c("Value","Code","Territory","Date")
    # Return data obtained from json
    return(Data)}
  if (Y == "ISLANDS")
  {# Select the total of tourist
    Data <- subset(Data, Data[,3] == "T" & Data[,4] == "T")
    # Sort in ascending order
    Data <- arrange(Data, Data[,5])
    # Get and save Lanzarote data
    ACE <- subset(Data, Data[,6] == "ES708")
    # Get and save Fuerteventura data
    FUE <- subset(Data, Data[,6] == "ES704") 
    # Get and save Gran Canaria data
    LPA <- subset(Data, Data[,6] == "ES705") 
    # Get and save Tenerife data
    TF  <- subset(Data, Data[,6] == "ES709")
    # Save data as data.frame
    DATA <- cbind(as.numeric_version(ACE[,1]), as.numeric_version(FUE[,1]),
                  as.numeric_version(LPA[,1]), as.numeric_version(TF[,1]))
    colnames(DATA) <- c("ACE", "FUE", "LPA", "TF")
    # Return data obtained from json
    return(DATA)}
  
}
