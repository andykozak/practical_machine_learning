library(caret)
library(knitr)


set.seed(55404)

# loading test and training data from file

sensordata.train <- read.csv("pml-training.csv", stringsAsFactors=FALSE)
sensordata.test <- read.csv("pml-testing.csv", stringsAsFactors=FALSE)

# data filtering function removing not relevant and empty or NA columns

datafilter <- function(datafile) {
  
  # keep only predictor variables, remove ones which we know that are not relevant 
  index.remove <- which(colnames(datafile) %in% c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window"))
  datafile <- datafile[, -index.remove]
  
  # Keep only columns with complet values - no NAs or empty ones
  index.keep <- !sapply(datafile, function(x) (any(is.na(x),any(x=="")) ))
  datafile <- datafile[, index.keep]
  
  return(datafile)
}
  

# filtering and convertint classe to factor for training data set
sensordata.train <- datafilter(sensordata.train)
sensordata.train$classe <- factor(sensordata.train$classe)
sensordata.test <- datafilter(sensordata.test)

# Model creation by using traincontrol for crossvalidation by using 3 fold validation (3 subsets)
   
control <- trainControl(method = "cv", number = 3, allowParallel = TRUE, verboseIter = TRUE)

 
# first model with Random forest approach
model1 <- train(classe ~ ., data = sensordata.train, method = "rf", trControl = control)

# second model with K-nearest neoghbour algoritm 
model2 <- train(classe ~ ., data = sensordata.train, method = "knn", trControl = control)



# Lets now compare cross-validation accuracy of 2 selected models
print (round(max(model1$results$Accuracy),3))
print (round(max(model2$results$Accuracy),3))



# Lets evaluate concordance of two models on test data
sensorprediction1 <- predict(model1, sensordata.test)
sensorprediction2 <- predict(model2, sensordata.test)

outputdata <- data.frame(RandomForest=sensorprediction1,KNN=sensorprediction2,IsMatching=sensorprediction1==sensorprediction2)
print (outputdata)


# export results to the file in coursera format for submission

  n = length(sensorprediction1)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(sensorprediction1[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  
  }




