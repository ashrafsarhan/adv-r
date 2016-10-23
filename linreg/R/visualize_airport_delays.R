#' @title Visualization of airports of flights delay 
#' @description visualizes the mean delay of flights for different airports by longitude and latitude
#' @return A map of airports.
#' @export


# 1.1.5 Handling large datasets with dplyr
visualize_airport_delays <- function(){
  library(tidyr)
  library(dplyr)
  library(base)
  library(stats)

  library(nycflights13)
  data("flights")
  data("airports")
  
  # subset varibales(col)
  flights <- dplyr::select(flights, dest, dep_delay, arr_delay)
  airports <- dplyr::select(airports, faa, name, lat, lon)
   
  # add new col of sum of dep_delay & arr_delay
  flights <- dplyr::mutate(flights, total_delay = dep_delay + arr_delay)
  
  # calculate the mean delay for each airport
  flights <- dplyr::select(flights, dest, total_delay)
  grp_flights <- dplyr::group_by(flights, dest)
  agg_flights <-dplyr::summarise(grp_flights, mean_delay = mean(total_delay, na.rm=TRUE))
  names(agg_flights)[1] <- "faa"
  
  # join 2 datasets
  joins <- dplyr::inner_join(agg_flights, airports, by="faa")
  joins <- filter(joins, faa != "LGA")           # remove row, NA in  faa = LGA
  
  # change negtive mean_delay to 0 of 3 observations 
  joins <- dplyr::arrange(joins, mean_delay)
  joins[c(1,2,3), 2] <- c(0,0,0)
  
  lon_scale <- c(min(joins$lon), max(joins$lon))
  lat_scale <- c(min(joins$lat), max(joins$lat))

  # plot a map of joins
  library(ggplot2)
  library(rworldmap)
  library(ggalt)
  library(ggthemes)
  
  world <-  map_data("world")
  gg <- ggplot() +
    coord_cartesian(xlim=(lon_scale * 1.1), ylim=(lat_scale * 1.1)) +
    geom_map(data = world, map = world,
             aes(x = long, y = lat, map_id=region),
             color="white", fill="lightblue", size=0.05, alpha=1/4) + 
    geom_point(data=joins, 
               aes(x=lon, y=lat, color=mean_delay, size=sqrt(mean_delay))) + 
    ggtitle("Map of flights delay of US airports")
  gg

  
}

visualize_airport_delays()

