## ---- echo = FALSE-------------------------------------------------------
suppressMessages(library(nycflights13))
suppressMessages(library(dplyr))
data(weather)
data(flights)

# Pick needed variables
delay_d <- select(flights,time_hour,origin,dep_delay)
delay_d <- filter(delay_d,dep_delay>0)
# Subset JFK airport
delay_d_jfk <- filter(delay_d,origin == "JFK")
head(delay_d_jfk)

## ----echo = FALSE--------------------------------------------------------
# Weather data
weather_d <- select(weather,time_hour,origin, wind_speed,precip,visib,month)

# Subset JFK airport
weather_d_jfk <- filter(weather_d, origin == "JFK")

# Match datasets by time_hour variable

delay_d_complete <- inner_join(delay_d_jfk, weather_d_jfk, by = "time_hour")
delay_d_complete <- select(delay_d_complete,-origin.y,-origin.x)

# Keep row that have a positive value on dep_delay
#delay_d_complete <- filter(delay_d_complete,dep_delay>0)

delay_d_complete <- as.data.frame(delay_d_complete)
delay_d_complete$month <- as.factor(delay_d_complete$month)

head(delay_d_complete)

## ---- echo = FALSE-------------------------------------------------------
suppressMessages(library(lattice))
suppressMessages(library(ggplot2))
suppressMessages(library(caret))

set.seed(123456789)
test_rows <- createDataPartition(y = delay_d_complete$month,
                                 p = 0.05,
                                 list = FALSE)
test_sample <- delay_d_complete[test_rows,]


## ----echo=FALSE----------------------------------------------------------
head(test_sample)

## ---- echo=FALSE---------------------------------------------------------
# Remove observation that got into the test sample
delay_d_complete <- delay_d_complete[-test_rows,]

train_rows <- createDataPartition(y = delay_d_complete$month,
                                  p = 0.80,
                                  list = FALSE)
train_sample <- delay_d_complete[train_rows,]

head(train_sample)


## ---- echo = FALSE-------------------------------------------------------
# Remove observation from that got into train sample

delay_d_complete <- delay_d_complete[-train_rows,]

validation_rows <- createDataPartition(y = delay_d_complete$month,
                                          p = 0.15,
                                          list = FALSE)
validation_sample <- delay_d_complete[validation_rows,]

head(validation_sample)

## ---- echo = TRUE--------------------------------------------------------
train_f <- function(data,lambda,formula){
  
  count <- seq(from = 0,to = lambda, by = 0.05)
  save <- data.frame("lambda" = count)
  y <- as.character(formula)[2]
  
  for(i in 1:length(count)){
    browser()
    call <- ridgereg(formula,data,lambda = count[i])
    save$RMSE[i] <- sqrt(mean((data[,y] - call$fitted_values)^2))
    print(length(data[,y]))
    anyNA(data[,y])
    print(length(call$fitted_values))
    anyNA(call$fitted_values)
  }
  return(save[which.min(save$RMSE),])
}

call_f <- formula(dep_delay ~ wind_speed+precip+visib)
train_f(data = validation_sample,lambda = 10,formula = call_f)


## ---- echo = TRUE--------------------------------------------------------
prediction <- ridgereg(data = test_sample,lambda = 2,formula = call_f)
head(prediction$fitted_values)

## ---- echo = TRUE--------------------------------------------------------
train_f(data = test_sample,lambda = 2,formula = call_f)

