library(caret)
library(mlbench)
data(BostonHousing)
str(BostonHousing)

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
set.seed(7)
model1 <- train(medv ~ ., data = training, method = "leapForward")
medvPred1 <- predict(model1, newdata = testing)
# summarize results
print(model1)
rslt1 <- cbind(actual = testing$medv, predict = medvPred1)

#Perform k-fold cross validation
# define training control
train_control <- trainControl(method="cv", number=10, savePredictions = TRUE)
# train the model
set.seed(7)
model2 <- train(medv ~ ., data = training, trControl=train_control, method = "leapForward")
plot(model2)
medvPred2 <- predict(model2, newdata = testing)
# summarize results
print(model2)
rslt2 <- cbind(actual = testing$medv, predict = medvPred2)

#Evaluation
summary(model1)
summary(model2)

#Creating caret custom models using ridgereg()
rregm <- list(type= 'Regression', library='linreg', loop=NULL)
prm <- data.frame(parameter = c('formula', 'data', 'lambda'),
                  class = c("formula", 'data.frame', 'vector'),
                  label = c('formula', 'data', 'lambda'))
rregm$parameters <- prm
#TODO Having an issue with the grid function
grid = function(x, y, len = NULL, search = "grid")  {
  if(search == "grid") {
    out <- expand.grid(lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)))
  } else {
    out <- data.frame(lambda = 10^runif(len, min = -5, 1))
  }
  out
}
rregm$grid <- grid
rregmFit <- function(formula, data, lambda = 0){
  linreg::rrfit(formula, data, lambda = 0)
}
rregm$fit <- rregmFit
rregmPred <- function(model, newdata) {
  linreg::rrpred(model, newdata)
  }
rregm$predict <- rregmPred
rregmProb <- function(model, newdata) {
  linreg::rrpred(model, newdata)
}
rregm$prob <- rregmProb
# Fit a linear regression model with the custom model
set.seed(7)
model <- train(medv ~ ., data = training, method = rregm)
medvPred <- predict(model, newdata = testing)
# summarize results
print(model)
rslt <- cbind(actual = testing$medv, predict = medvPred)

