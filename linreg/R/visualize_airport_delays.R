# 1.1.5 Handling large datasets with dplyr
visualize_airport_delays <- function(){
  library(tidyr)
  library(dplyr)
  library(base)
  library(stats)

  library(nycflights13)
  data("flights")
  data("airports")

  # Data Wrangling of flights
  # Select columns by name
  delay_dep <- as.data.frame(select(flights, dep_delay, origin))
  colnames(delay_dep) <- c("delay_minutes", "airport")
  delay_arr <- as.data.frame(select(flights, arr_delay, dest))
  colnames(delay_arr) <- c("delay_minutes", "airport")

  # dplyr::bind_rows
  delay_data <- bind_rows(delay_dep, delay_arr)
  airport_unique <- unique(delay_data$airport)

  # create a empty data frame
  delay_select <- data.frame(airport = c(), mean_delay = c())

  for(i in airport_unique){
    #Extract rows that meet logical criteria.
    airport <- filter(delay_data, airport == i)
    delay <- as.vector(airport$delay_minutes)
    mean <- mean(delay, na.rm = TRUE)
    data1 <- data.frame(airport = i, mean_delay = mean)
    delay_select <- bind_rows(delay_select, data1)
  }
  delay_select
  #107 airports


  # Data Wrangling of airports
  airports_data <- as.data.frame(select(airports, faa, lat, lon))

  # create a empty data frame
  airports_new <- data.frame(faa = c(), lat = c(), lon = c())

  for(i in airport_unique) {
    #Extract rows that meet logical criteria.
    airports_filter <- filter(airports_data, faa == i)
    airports_new <- bind_rows(airports_new, airports_filter)
  }
  airports_new 
  #103 airports


  # select 103 airports from 107 airports in delay_new
  delay_new <- data.frame()  
  for(i in airports_new$faa){
    #Extract rows that meet logical criteria.
    data2 <- filter(delay_select, airport == i)
    delay_new <- bind_rows(delay_new, data2)
  }
  delay_new


  tit <- data.frame(airports_new$faa,airports_new$lat, 
                    airports_new$lon, delay_new$mean_delay)
  colnames(tit) <- c("airport","lat", "lon", "mean_delay" )

  # 3D Scatterplot with Coloring and Vertical Drop Lines
  library(scatterplot3d) 
  scatterplot3d(tit$lat,
                tit$lon,
                tit$mean_delay, 
                xlab = "longitude",
                ylab = "latitude",
                zlab = "mean of delay time (minutes)",
                pch=16, highlight.3d=TRUE, type="h", 
                main="3D Scatterplot of airport delay")
}

visualize_airport_delays()
