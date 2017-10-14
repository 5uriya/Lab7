visualize_airport_delays <- function()
  
{
  
  
  
  flights1 <- nycflights13::flights
  
  airports1 <- nycflights13::airports
  
  
  
  
  mean_data1 <- dplyr::summarise(dplyr::group_by(flights1, dest), M = mean(arr_delay))
  
  
  library(dplyr)
  
  
  comdat <- inner_join(airports1,mean_data1, by = c("faa" =  "dest"))
  
  
  
  library(ggplot2)
  
  plot1<- ggplot(comdat, aes(x=comdat$lon, y=comdat$lat)) +
    
    geom_point(na.rm = TRUE) +
    
    labs(title="Average Flight Delays",
         
         x="Latitude", y="Longitude")
  
  return(plot1)
  
  
  
}

visualize_airport_delays()