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

# Fit a linear regression model with forward selection of covariates
model <- train(medv ~ ., data = training, method = "leapForward")
plot(model)
medvPred <- predict(model, newdata = testing)
# summarize results
print(model)
rslt <- cbind(actual = testing$medv, predict = medvPred)

#Perform k-fold cross validation
# define training control
train_control <- trainControl(method="cv", number=10, savePredictions = TRUE)
# train the model
model <- train(medv ~ ., data = training, trControl=train_control, method = "leapForward")
plot(model)
medvPred <- predict(model, newdata = testing)
# summarize results
print(model)
rslt <- cbind(actual = testing$medv, predict = medvPred)

