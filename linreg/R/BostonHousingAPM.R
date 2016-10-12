library(caret)
library(mlbench)
data(BostonHousing)
str(BostonHousing)
set.seed(nrow(BostonHousing))

# define an 80%/20% train/test split of the dataset
split=0.80
inTrain <- createDataPartition(BostonHousing$medv, p = split, list = FALSE)
training <- BostonHousing[inTrain,]
cat('training: ', nrow(training))
testing <- BostonHousing[-inTrain,]
cat('testing: ', nrow(testing))

#Centering and Scaling
preProcValues <- preProcess(training, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, testing)

