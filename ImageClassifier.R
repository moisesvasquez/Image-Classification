# Classification Models
library(h2o)
library(kernlab)
library(tidyverse)
library(caTools)
library(naivebayes)
library(randomForest)

# Import File
Data <- read.csv("images.csv")
Data <- Data[,-1]
# Use all CPU threads
h2o.init(nthreads = -1)

SPLIT_RATIO = .85
# H2o Split
h2oCars <- as.h2o(Data)
carsSplit <- h2o.splitFrame(data = h2oCars, ratios = SPLIT_RATIO)
h2oTrain <- carsSplit[[1]]
h2oTest <- carsSplit[[2]]

# Regular Split
sample <- sample.split(Data$Label, SplitRatio = SPLIT_RATIO)
train <- subset(Data, sample == TRUE)
test <- subset(Data, sample == FALSE)

# DEEP LEARNING
cat("~~~~~ DEEP LEARNING ~~~~~","\n")

h2oDL <- h2o.deeplearning(
  x = colnames(h2oTrain),
  y = c("Label"),
  training_frame = h2oTrain,
  hidden = c(16, 16, 16),
  epochs = 100,
  seed = 12345,
  nfolds = 4
)

pred <- h2o.predict(h2oDL, h2oTest)
result_DLh2o <- data.frame(Actual = as.vector(h2oTest$Label), Prediction = as.vector(pred$predict))
result_DLh2o$Model <- "Deep Learning"
result_DLh2o$Accurate <- ifelse(result_DLh2o$Actual == result_DLh2o$Prediction,1,0)
# TP,TN, FP, FN
TPDl <- subset(result_DLh2o, Actual == "Car" & Prediction == "Car" )
TNDl <- subset(result_DLh2o, Actual == "No_Car" & Prediction == "No_Car")
FPDl <- subset(result_DLh2o, Actual == "No_Car" & Prediction == "Car")
FNDl <- subset(result_DLh2o, Actual == "Car" & Prediction == "No_Car")

# Accuracy
accuracyDL <- (nrow(TPDl)) / (nrow(h2oTest))
cat("Deep Learning Accuracy:" , accuracyDL, "\n")

# Precision
precisionDL <- (nrow(TPDl)) / (nrow(TPDl) + nrow(FPDl))
cat("Deep Learning Precision:" , precisionDL, "\n")

# Recall
recallDL <- (nrow(TPDl)) / (nrow(TPDl) + nrow(FNDl))
cat("Deep Learning Recall:" , recallDL, "\n")

# Performance
rfPerformance <- h2o.performance(h2oDL, h2oTest)

# Auc
h2oDlAUC <- h2o.auc(rfPerformance)
cat("Deep Learning AuC:", h2oDlAUC, "\n")


# RANDOM FOREST
cat("~~~~~ RANDOM FOREST ~~~~~","\n")
forest <- randomForest(Label ~ ., data = train)
predictionForest <- predict(forest, test, type = "class")
result_RFh2o <- data.frame(Actual = test$Label, Prediction = predictionForest)
result_RFh2o$Model <- "Random Forest"
result_RFh2o$Accurate <- ifelse(result_RFh2o$Actual == result_RFh2o$Prediction,1,0)
# TP,TN, FP, FN
TPRf <- subset(result_RFh2o, Actual == "Car" & Prediction == "Car" )
TNRf <- subset(result_RFh2o, Actual == "No_Car" & Prediction == "No_Car")
FPRf <- subset(result_RFh2o, Actual == "No_Car" & Prediction == "Car")
FNRf <- subset(result_RFh2o, Actual == "Car" & Prediction == "No_Car")

# Accuracy
accuracyRf <- (nrow(TPRf)) / (nrow(test))
cat("Random Forest Accuracy:" , accuracyRf, "\n")

# Precision
precisionRf <- (nrow(TPRf)) / (nrow(TPRf) + nrow(FPRf))
cat("Random Forest Precision:" , precisionRf, "\n")

# Recall
recallRf <- (nrow(TPRf)) / (nrow(TPRf) + nrow(FNRf))
cat("Random Forest Recall:" , recallRf, "\n")

#Auc
AUCRF <- auc(roc(predictionForest,test$Label))
cat("Random Forest AUC:" , AUCRF, "\n")

# NAIVE BAYES
cat("~~~~~ NAIVE BAYES ~~~~~","\n")

nbModel <-
  naive_bayes(Label ~ ., data = train)

nbPred <- predict(nbModel,test)
result_NBh2o <- data.frame(Actual = test$Label, Prediction = nbPred)
result_NBh2o$Model <- "Naive Bayes"
result_NBh2o$Accurate <- ifelse(result_NBh2o$Actual == result_NBh2o$Prediction,1,0)

# TP,TN, FP, FN
TPNB <- subset(result_NBh2o, Actual == "Car" & Prediction == "Car" )
TNNB <- subset(result_NBh2o, Actual == "No_Car" & Prediction == "No_Car")
FPNB <- subset(result_NBh2o, Actual == "No_Car" & Prediction == "Car")
FNNB <- subset(result_NBh2o, Actual == "Car" & Prediction == "No_Car")

# Accuracy
accuracyNB <- (nrow(TPNB)) / nrow(test)
cat("Naive Bayes Accuracy:" , accuracyNB, "\n")

# Precision
precisionNB <- (nrow(TPNB)) / (nrow(TPNB) + nrow(FPNB))
cat("Naive Bayes Precision:" , precisionNB, "\n")

# Recall
recallNB <- (nrow(TPNB)) / (nrow(TPNB) + nrow(FNNB))
cat("Naive Bayes Recall:" , recallNB, "\n")

#Auc
AUCNB <- auc(roc(nbPred,test$Label))
cat("Naive Bayes AUC:" , AUCNB, "\n")

# SVM
cat("~~~~~ SUPPORT VECTOR MACHINE ~~~~~","\n")

modelSVM <- ksvm(Label ~ ., data = train, kernel = "rbfdot",C = 5)

predSVM <- predict(modelSVM,test)
result_SVM <- data.frame(Actual = test$Label, Prediction = predSVM)
result_SVM$Model <- "SVM"
result_SVM$Accurate <- ifelse(result_SVM$Actual == result_SVM$Prediction,1,0)
# TP,TN, FP, FN
TPSVM <- subset(result_SVM, Actual == "Car" & Prediction == "Car" )
TNSVM <- subset(result_SVM, Actual == "No_Car" & Prediction == "No_Car")
FPSVM <- subset(result_SVM, Actual == "No_Car" & Prediction == "Car")
FNSVM <- subset(result_SVM, Actual == "Car" & Prediction == "No_Car")

# Accuracy
accuracySVM <- (nrow(TPSVM)) / (nrow(test))
cat("Support Vector Machine Accuracy:" , accuracySVM, "\n")

# Precision
precisionSVM <- (nrow(TPSVM)) / (nrow(TPSVM) + nrow(FPSVM))
cat("Support Vector Machine Precision:" , precisionSVM, "\n")

# Recall
recallSVM <- (nrow(TPSVM)) / (nrow(TPSVM) + nrow(FNSVM))
cat("Support Vector Machine Recall:" , recallSVM, "\n")

#Auc
AUCSVM <- auc(roc(predSVM,test$Label))
cat("Support Vector Machine AUC:" , AUCSVM, "\n")

# HYPOTESIS TESING
cat("~~~~~ HYPOTESIS TESING ~~~~~","\n")

for_Anova <- rbind(result_SVM,result_NBh2o,result_RFh2o,result_DLh2o)
AccuracyAOV <- aov(Accurate ~ Model, data = for_Anova)
print(summary(AccuracyAOV))
cat("As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the accuracies of the models","\n")
cat("Tukey Test to see which models are significantly different","\n")
TukeyAccuracies <- TukeyHSD(AccuracyAOV)
print(TukeyAccuracies)
