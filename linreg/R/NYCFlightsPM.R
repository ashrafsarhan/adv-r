library(caret)
library(mlbench)
library("nycflights13")
flights_data <- flights
weather_data <- weather
all_data <- plyr::join(flights_data, weather_data, by=c('origin', 'year', 'time_hour'), type='inner')
trgt_data <- all_data[,c('flight', 'origin', 'dest', 'arr_delay', 'distance', 'wind_speed')]

#Get rid of NA rows (Missing values) 
trgt_data <- na.omit(trgt_data)

#Get rid of negative rows 
negrows <- which(trgt_data$arr_delay < 0)
trgt_data <- trgt_data[-negrows,]

#Partition data set in training 80%, validation 15%, testing 5%.
inTraining <- createDataPartition(trgt_data$arr_delay, p=.80, list=FALSE)
training <- trgt_data[ inTraining,]
cat('training: ', nrow(training))
notTraining <- trgt_data[-inTraining,]
inValidation <- createDataPartition(notTraining$arr_delay, p=.15, list=FALSE)
validation <- notTraining[inValidation,]
cat('validation: ', nrow(validation))
testing <- notTraining[-inValidation,]
cat('testing: ', nrow(testing))

#Centering and Scaling
preProcValues <- preProcess(trgt_data[,4:6], method = c("center", "scale"))
training <- predict(preProcValues, training)
validation <- predict(preProcValues, validation)
testing <- predict(preProcValues, testing)

#Set lamada grid
lambdaGrid <- seq(1,5,length=10)
#Set search grid for lamada parameters
tuneGrid= expand.grid(lambda=lambdaGrid) 
# define training control
trControl <- trainControl(method="repeatedCV", number=5)
# Fit a ridge regression model using the validation dataset
set.seed(100)
model1 <- train(arr_delay ~ distance+wind_speed, data = validation, method = "ridge", 
               tuneGrid=tuneGrid, trControl=trControl)
# summarize results
print(model1)

# Predict the test set using the optimal lamada
set.seed(100)
model2 <- train(arr_delay ~ distance+wind_speed, data = training, method = "ridge", 
                tuneGrid=expand.grid(lambda=5))
pred <- predict(model2, newdata = testing)
# summarize results
print(model2)