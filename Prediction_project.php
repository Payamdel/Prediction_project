 Prediction-task-1
# Prediction Assignment Writeup 
library(caret)
 
# Read in the training and test sets by using read.csv function. Because the initial data sets contain coloumns mostly filled with blank cells, missining values, and even possible white spaces, it would be desirable to exclude all of them from the data as they wont actually explain the variation among the observations. To this end, all of the blank cells and even cells that might include space (" ") will be interpreted as "NA" as "na.strings charactor vector" is applid to the data sets.

training <- read.csv(file="pml-training.csv", header=T, na.strings=c("NA",""," "))
dim(training)
testing  <- read.csv(file="pml-testing.csv", header=T, na.strings=c("NA",""," "))
dim(testing)

# Second, all the NAs columns are found in the data and the names of those varaibles (V) are stored:
V_training <-  colnames(training[ , colSums(is.na(training)) == 0])
V_testing <-  colnames(testing [ , colSums(is.na(testing )) == 0])
  
# Once the variables of interest are identified, a new training and test sets are created using only those variables
New_training <- training[V_training]
dim (New_training)
New_testing <- testing[V_testing]
dim (New_testing)
 

# Next, all the six first columns that containing additional information about the observations are removed as they are not of great interesst for the prediction model:
Final_training <- New_training [,-(1:6)]
dim(Final_training)
Final_testing <- New_testing [,-(1:6)]
dim(Final_testing)
 
# Having done all the above preprocessing, now the data contains only 54 variables that will be used for training and testing the model.
# The training data now is divided into a sub-training and sub-testing data sets where the percentage of training data that goes to sub-training data is 60%. This makes it possible to test the model and check the predictive performance of the model.
set.seed (555)
trainIndex = createDataPartition(Final_training$classe, p = 0.6, list = FALSE)
sub_training = Final_training[trainIndex, ]
sub_testing = Final_training[-trainIndex, ]

# Now the model is trained using only the sub_training data set. Random forest function in caret is applied on the sub_training data where a 5 fold cross validation strategy is used to decrease the risk for overfitting. One could even apply a 10 fold cross validation strategy as the number of observations are huge but that would be a bit more computationally expensive. The number of trees in RF is set to 1000 to increase the stability and strength of the analysis. I choose Random forest as the classifier of choice because it is one of the best in accuracy among current algorithms, it is fast and effective in handeling large data sets with lots of varables and observations. It provides estimates on variables that have the most predictive power in the classification, etc.

trControl = trainControl(method = "cv", number = 5)
rf_Model <- train (sub_training$classe ~ ., data=sub_training, method= "rf", ntree = 1000, importance = TRUE, trControl = trControl)
rf_Model
# final model output: the out-of-bag (OOB) estimate of error rate is 0.35% which is quite low
rf_Model$finalModel

# Plot the important variables
varImpPlot(rf_Model$finalModel, sort = TRUE, scale=TRUE)
# Calculate and display the important variables
varImp(rf_Model, scale = TRUE)
 
# Prediction on the cases in the sub_testing data set:
pred <- predict(rf_Model, newdata = sub_testing)
 
# Create a confusion matrix to provide a set of performance statistics. The RF accuracy on the test set is 0.9978 which is perfect.
confusionMatrix(pred, sub_testing$classe) 
 
# Prediction Assignment Submission: Instructions Help Center:
# First copy and paste the provided function which can be used to create text files that have one character each with the predicted label for the corresponding problem ID

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

# Apply the machine learning algorithm that is built to each of the 20 test cases in the testing data set:
Pred_test <- predict(rf_Model, Final_testing)
Pred_test
pml_write_files(Pred_test)
 
 
 
