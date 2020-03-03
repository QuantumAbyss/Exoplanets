#'
#' Exoplanet Star Classification
#' Data source: https://www.kaggle.com/keplersmachines/kepler-labelled-time-series-data
#'

### INITIALIZATION --------------------------------------------------------------------------------
.x <- c("data.table", "dplyr", "tidyr", "ggplot2", "DMwR",
        "randomForest")
lapply(.x, library, character.only=T)

setwd("~/1) Projects/Exoplanets/")

### DATA READ -------------------------------------------------------------------------------------

train <- fread("./exoTrain.csv")
test <- fread("./exoTest.csv")

### EXPLORE ---------------------------------------------------------------------------------------
table(train$LABEL)
cols <- sample(2:ncol(train), 5)
summary(train[, ..cols])

### PLOTTING --------------------------------------------------------------------------------------
piece <- train[sample(1:nrow(train),3),] %>% 
  mutate(starNum = 1:nrow(.)) %>%
  gather("key", "value", FLUX.1:FLUX.3197) %>%
  mutate(obs = gsub(".*\\.","", key))
ggplot(piece, mapping=aes(x=obs,y=value)) + geom_point() + facet_wrap(~starNum,ncol=1)
ggplot(piece, mapping=aes(x=value)) + geom_histogram(bins=200) + facet_wrap(~starNum,ncol=1)

### CLEANING --------------------------------------------------------------------------------------

trainX <- data.table(scale(train[,-1]))
trainY <- train$LABEL
testX <- data.table(scale(test[,-1]))
testY <- test$LABEL
# rawr <- trainS[1,] %>% gather("key", "value", FLUX.1:FLUX.3197)

pca <- prcomp(trainX)
pcVar <- (pca$sdev)^2
percVar <- pcVar / sum(pcVar)
plot(cumsum(percVar)[1:30], xlab="Principal Component", ylab="Percent of Variance", type="b")
cumsum(percVar)[1:30]

components <- 30

trainClean <- data.table(LABEL=as.factor(trainY),
                         pca$x[,1:components])
balanced <- trainClean
# balanced <- SMOTE(LABEL ~ ., data=trainClean,
#              perc.over = 100, perc.under=200)
# balanced <- bind_rows(filter(trainClean, LABEL==2),
#                       sample_n(filter(trainClean,LABEL==1), 37))

### MODELING --------------------------------------------------------------------------------------

rfModel <- randomForest(LABEL ~ ., data=balanced)

trainClean$PRED <- predict(rfModel, trainClean)
confMatrix <- table(trainClean$LABEL, trainClean$PRED)
confMatrix
data.table(R2=mean(testClean$PRED==testClean$LABEL),
           accuracy = (confMatrix[1,1]+confMatrix[2,2])/sum(confMatrix),
           recall = confMatrix[1,1]/(confMatrix[1,1]+confMatrix[2,1]),
           precision=confMatrix[1,1]/(confMatrix[1,1]+confMatrix[1,2])) %>%
  mutate(fone = 2*((precision*recall)/(precision+recall)))


testClean <- data.table(predict(pca, newdata=testX))[,1:components]
testClean$PRED <- predict(rfModel, testClean)
testClean$LABEL <- as.factor(testY)
confMatrix <- table(testClean$LABEL, testClean$PRED)
confMatrix

testClean$PRED <- knn(balanced[,-1], testClean, cl=balanced$LABEL, k=5)
testClean$LABEL <- as.factor(testY)
confMatrix <- table(testClean$LABEL, testClean$PRED)
confMatrix


models <- lapply(1:20, function(x) {
  mod <- randomForest(LABEL ~ ., mtry=x, data=balanced)
  trainClean$PRED <- predict(rfModel, trainClean)
  confMatrix <- table(trainClean$LABEL, trainClean$PRED)
  data.table(varVal=x,
             accuracy = (confMatrix[1,1]+confMatrix[2,2])/nrow(trainClean),
             recall = confMatrix[1,1]/(confMatrix[1,1]+confMatrix[2,1]),
             precision=confMatrix[1,1]/(confMatrix[1,1]+confMatrix[1,2])) %>%
    mutate(fone = 2*((precision*recall)/(precision+recall))) %>%
    return()
}) %>% rbindlist()

testClean <- data.table(predict(pca, newdata=testX))[,1:components]
testClean$PRED <- predict(rfModel, testClean)
testClean$LABEL <- as.factor(testY)
confMatrix <- table(testClean$LABEL, testClean$PRED)
confMatrix
data.table(R2=mean(testClean$PRED==testClean$LABEL),
           accuracy = (confMatrix[1,1]+confMatrix[2,2])/nrow(trainClean),
           recall = confMatrix[1,1]/(confMatrix[1,1]+confMatrix[2,1]),
           precision=confMatrix[1,1]/(confMatrix[1,1]+confMatrix[1,2])) %>%
  mutate(fone = 2*((precision*recall)/(precision+recall)))

testClean$PRED <- knn(balanced[,-1], testClean, cl=balanced$LABEL, k=5)
testClean$LABEL <- as.factor(testY)
confMatrix <- table(testClean$LABEL, testClean$PRED)
confMatrix
data.table(R2=mean(testClean$PRED==testClean$LABEL),
           accuracy = (confMatrix[1,1]+confMatrix[2,2])/nrow(trainClean),
           recall = confMatrix[1,1]/(confMatrix[1,1]+confMatrix[2,1]),
           precision=confMatrix[1,1]/(confMatrix[1,1]+confMatrix[1,2])) %>%
  mutate(fone = 2*((precision*recall)/(precision+recall)))


diffcomp <- lapply(5:50, function(x) {
  components <- x
  
  trainClean <- data.table(LABEL=as.factor(train$LABEL),
                           pca$x[,1:components])
  balanced <- SMOTE(LABEL ~ ., data=trainClean,
                    perc.over = 100, perc.under=200)
  
  rfModel <- randomForest(LABEL ~ ., data=balanced)
  
  
  testClean <- data.table(predict(pca, newdata=testS))[,1:components]
  testClean$PRED <- predict(rfModel, testClean)
  testClean$LABEL <- as.factor(test$LABEL)
  confMatrix <- table(testClean$LABEL, testClean$PRED)
  confMatrix
  data.table(accuracy = (confMatrix[1,1]+confMatrix[2,2])/nrow(trainClean),
             recall = confMatrix[1,1]/(confMatrix[1,1]+confMatrix[2,1]),
             precision=confMatrix[1,1]/(confMatrix[1,1]+confMatrix[1,2])) %>%
    mutate(fone = 2*((precision*recall)/(precision+recall))) %>%
    return()
  
}) %>% rbindlist()













