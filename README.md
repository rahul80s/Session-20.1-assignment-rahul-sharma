# Session-20.1-assignment-rahul-sharma
Acagild session 20.1 assignment 

Problem Statement

1. Use the below given data set

Data Set

2. Perform the below given activities:

a. Create classification model using different random forest models

b. Verify model goodness of fit

c. Apply all the model validation techniques

d. Make conclusions

e. Plot importance of variables


Ans 2 ->

trainingRaw = read.csv("Wearable Computing_weight.csv")

NAsTrain = apply(trainingRaw, 2, function(x) {sum(is.na(x))}) 

col2keep = (NAsTrain<0.1*nrow(trainingRaw)) 

trainData = trainingRaw[, col2keep]   

trainData = trainData[ , -grep("abc",names(trainData))]

namesTrain = names(trainData)

dim(trainData)

finalTestRaw = read.csv("Wearable Computing_weight.csv")

NAsTest = apply(finalTestRaw, 2, function(x) {sum(is.na(x))})

col2keep = NAsTest<0.1*nrow(finalTestRaw) 

finalTest = finalTestRaw[, col2keep]

namesTest = names(finalTest)

colTrain2keep = sapply(namesTrain, function(x){x %in% namesTest}) 

trainData = trainData[, colTrain2keep] 

trainData$classe = trainingRaw$classe 

inTrain = createDataPartition(trainData$classe, p=0.4, list=FALSE)

training = trainData[inTrain, ]

testing = trainData[-inTrain, ]

ctrl = trainControl(method="cv", number=3)

models = c("svmLinear", "randomforest", "gbm", "nb", "lda") 

print(models[1])

modSVM = train(training$classe ~ ., data=training, method=models[1], trControl = ctrl)

print(models[2])

modRF = train(classe ~ ., data=training, method=models[2], trControl=ctrl, prox=T, ntree=100)

print(models[3])

modGBM = train(classe ~ ., data=training, method=models[3], trControl=ctrl, verbose=FALSE)

print(models[4])

modNB = train(classe ~ ., data=training, method=models[4], trControl=ctrl)

print(models[5])

modLDA = train(classe ~ ., data=training, method=models[5], trControl=ctrl)

predSVM = predict(modSVM, newdata=testing)

confusionMatrix(predSVM, testing$classe)

predRF = predict(modrandomforest, newdata=testing)

confusionMatrix(predrandomforest, testing$classe)

print("Variables importance in model")

vi = as.data.frame(varImp(modRF$finalModel))

predGBM = predict(modGBM, newdata=testing)

confusionMatrix(predGBM, testing$classe)

predNB = predict(modNB, newdata=testing)

confusionMatrix(predNB, testing$classe)

predLDA = predict(modLDA, newdata=testing)

confusionMatrix(predLDA, testing$classe)

finalTestPred = list()

finalTestPred$SVM = predict(modSVM, newdata=finalTest)

finalTestPred$randomforest = predict(modrandomforest, newdata=finalTest)

finalTestPred$GBM = predict(modGBM, newdata=finalTest)

finalTestPred$NB = predict(modNB, newdata=finalTest)

finalTestPred$LDA = predict(modLDA, newdata=finalTest)

finalTestPredDF = as.data.frame(finalTestPred)

write.table(finalTestPredDF, file="finalTest.csv", sep=",", row.names=FALSE)

#Out of 5 methods, the prediction performance is evaluated from the training set, testing set, and final test set. 
The out of sample accuracy is slight worse than the error from the cross validation. Overall, the random forest is the best tool to use.
