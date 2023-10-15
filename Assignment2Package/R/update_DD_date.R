#'@title Updating to Decimal Degrees and %Y-%m-%d Date format
#'@description
#'This function takes the input of a specified data frame and it's respective Longitude, Latitude, and Date values. It changes Decimal Minute Longitude and Latitude values to Decimal Degrees. It also changes dates in the %d-%b-%y format to %Y-%m-%d. 
#' 
#'@param df The name of the specified data frame
#'@param Longitude Longitude in decimal minute format
#'@param Latitude Latitude in decimal minute format
#'@param Date Date in "%d-%b-%y" format
#'@export 

update_DD_date <- function(df, Longitude, Latitude, Date){ 
  
  degree <- as.numeric(sapply(strsplit(Longitude, "°"), "[[", 1))
  minute <- sapply(strsplit(Longitude, "°"), "[[", 2)
  minute <- as.numeric(gsub("'W", "", minute))/60
  new.long <- round(-degree - minute, 3)
  
  degree2 <- as.numeric(sapply(strsplit(Latitude, "°"), "[[", 1))
  minute2 <- sapply(strsplit(Latitude, "°"), "[[", 2)
  minute2 <- as.numeric(gsub("'N", "", minute2))/60
  new.lat <- round(degree2 + minute2, 3)
  
  TheReal <- format((as.Date(Date, format = "%d-%b-%y")), format = "%Y-%m-%d")
  final.df <- cbind(new.long, new.lat, TheReal)
  return(final.df)
}
