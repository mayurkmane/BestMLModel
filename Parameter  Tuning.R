# 1. Set Working Directory

# 2. Import the Dataset
Data <- read.csv("C:/..../Social_Network_Ads.csv")

# Import Required Libraries
library(caret)
library(doMC)   # configure multicore
registerDoMC(cores=8)

# Data Preparation

# Finding best model with best parameters

bestmodel <- function(data, formula, type){
  
  # Algorithms to train
  algorithms <- c("svmLinear","svmPoly", "xgbTree","nnet", "rf")
  
  # prepare training scheme
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  
  # Empty Data Frame for Model
  results <- data.frame(Model = c("svmLinear","svmPoly", "xgbTree","nnet", "rf"), Metrics = NA, Parameters = NA)
  
  for(i in algorithms){
    
    cat("#############################",i,"#############################,\n")
    
    # train the model
    model <- train(formula, data=data, method= i, trControl=control, verboseIter = 1)
    
    # Save the maximum accuracy and best parameters
    if( type == "classification"){
      
      results$Metrics[results$Model == i] <- max(model$results["Accuracy"])
      results$Parameters[results$Model == i] <- list(model$bestTune)
      
    }else{
      
      results$Metrics[results$Model == i] <- min(model$results["RMSE"])
      results$Parameters[results$Model == i] <- list(model$bestTune)
      
    }
    cat("\n########################################################################\n")
  }
  
  results <- results[order(results$Metrics, decreasing = T),]
  
  results
}

ModelStats <- bestmodel(Data, Purchased~., "regression")

# Modeling

# Evaluation
