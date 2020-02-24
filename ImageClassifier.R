# Classification Models
library(h2o)
library(kernlab)
library(tidyverse)
library(caTools)

# Import File
Data <- read.csv("images.csv")

# Use all CPU threads
h2o.init(nthreads = -1)

SPLIT_RATIO = .75
h2oCars <- as.h2o(Data)
carsSplit <- h2o.splitFrame(data = h2oCars, ratios = SPLIT_RATIO)
h2oTrain <- carsSplit[[1]]
h2oTest <- carsSplit[[2]]

# DEEP LEARNING

h2oDL <- h2o.deeplearning(
  x = colnames(h2oTrain),
  y = c("Label"),
  training_frame = h2oTrain,
  hidden = c(16, 8),
  epochs = 100,
  seed = 12345,
  nfolds = 3
)

pred <- h2o.predict(h2oDL, h2oTest)
result_DLh2o <- data.frame(Actual = as.vector(h2oTest$Label), Prediction = as.vector(pred$predict))

# TP,TN, FP, FN
TPDl <- subset(result_DLh2o, Actual == "Car" & Prediction == "Car" )
TNDl <- subset(result_DLh2o, Actual == "No_Car" & Prediction == "No_Car")
FPDl <- subset(result_DLh2o, Actual == "No_Car" & Prediction == "Car")
FNDl <- subset(result_DLh2o, Actual == "Car" & Prediction == "No_Car")

# Accuracy
accuracyDL <- (nrow(TPDl)) / (nrow(TPDl) + nrow(FPDl))
cat("Deep Learning Accuracy:" , accuracyDL, "\n")

# Precision
precisionDL <- (nrow(TPDl)) / (nrow(h2oTest))
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
rfModel <-
  h2o.randomForest(y = "Label",
                   training_frame = h2oTrain,
                   validation_frame = h2oTest)

rfPred <- h2o.predict(rfModel, h2oTest)
result_RFh2o <- data.frame(Actual = as.vector(h2oTest$Label), Prediction = as.vector(rfPred$predict))

# TP,TN, FP, FN
TPRf <- subset(result_RFh2o, Actual == "Car" & Prediction == "Car" )
TNRf <- subset(result_RFh2o, Actual == "No_Car" & Prediction == "No_Car")
FPRf <- subset(result_RFh2o, Actual == "No_Car" & Prediction == "Car")
FNRf <- subset(result_RFh2o, Actual == "Car" & Prediction == "No_Car")

# Accuracy
accuracyRf <- (nrow(TPRf)) / (nrow(TPRf) + nrow(FPRf))
cat("Random Forest Accuracy:" , accuracyRf, "\n")

# Precision
precisionRf <- (nrow(TPRf)) / (nrow(h2oTest))
cat("Random Forest Precision:" , precisionRf, "\n")

# Recall
recallRf <- (nrow(TPRf)) / (nrow(TPRf) + nrow(FNRf))
cat("Random Forest Recall:" , recallRf, "\n")

# Performance
rfPerformance <- h2o.performance(rfModel, h2oTest)

# Auc
h2oRfAUC <- h2o.auc(rfPerformance)
cat("Random Forest AuC:", h2oRfAUC, "\n")

# NAIVE BAYES

nbModel <-
  h2o.naiveBayes(y = "Label",
                 training_frame = h2oTrain,
                 validation_frame = h2oTest)

nbPred <- h2o.predict(nbModel, h2oTest)
result_RFh2o <- data.frame(Actual = as.vector(h2oTest$Label), Prediction = as.vector(nbPred$predict))

# TP,TN, FP, FN
TPRf <- subset(result_RFh2o, Actual == "Car" & Prediction == "Car" )
TNRf <- subset(result_RFh2o, Actual == "No_Car" & Prediction == "No_Car")
FPRf <- subset(result_RFh2o, Actual == "No_Car" & Prediction == "Car")
FNRf <- subset(result_RFh2o, Actual == "Car" & Prediction == "No_Car")

# Accuracy
accuracyRf <- (nrow(TPRf)) / (nrow(TPRf) + nrow(FPRf))
cat("Naive Bayes Accuracy:" , accuracyRf, "\n")

# Precision
precisionRf <- (nrow(TPRf)) / (nrow(h2oTest))
cat("Naive Bayes Precision:" , precisionRf, "\n")

# Recall
recallRf <- (nrow(TPRf)) / (nrow(TPRf) + nrow(FNRf))
cat("Naive Bayes Recall:" , recallRf, "\n")

# Performance
rfPerformance <- h2o.performance(nbModel, h2oTest)

# Auc
h2oRfAUC <- h2o.auc(rfPerformance)
cat("Naive Bayes AuC:", h2oRfAUC, "\n")

# SVM
# Regular Split
sample <- sample.split(Data$Label, SplitRatio = .75)
train <- subset(Data, sample == TRUE)
test <- subset(Data, sample == FALSE)



modelSVM <- ksvm(Label ~ ., data = train, kernel = "rbfdot")

predSVM <- predict(modelSVM,test)
result_SVM <- data.frame(Actual = test$Label, Prediction = predSVM)

# TP,TN, FP, FN
TPRf <- subset(result_SVM, Actual == "Car" & Prediction == "Car" )
TNRf <- subset(result_SVM, Actual == "No_Car" & Prediction == "No_Car")
FPRf <- subset(result_SVM, Actual == "No_Car" & Prediction == "Car")
FNRf <- subset(result_SVM, Actual == "Car" & Prediction == "No_Car")

# Accuracy
accuracyRf <- (nrow(TPRf)) / (nrow(TPRf) + nrow(FPRf))
cat("Gaussian RBF Kernel Accuracy:" , accuracyRf, "\n")

# Precision
precisionRf <- (nrow(TPRf)) / (nrow(h2oTest))
cat("Gaussian RBF Kernel Precision:" , precisionRf, "\n")

# Recall
recallRf <- (nrow(TPRf)) / (nrow(TPRf) + nrow(FNRf))
cat("Gaussian RBF Kernel Recall:" , recallRf, "\n")


