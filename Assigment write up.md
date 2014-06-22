Practical Machine Learning Assignment
========================================================

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, the aim is to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants to predict the behaviour of 20 test cases. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.

The machine learning method used for this assignment is random forest with out-of-bag error estimate. 

The following text explains random forest:
"In random forests, there is no need for cross-validation or a separate test set to get an unbiased estimate of the test set error. It is estimated internally, during the run, as follows:

Each tree is constructed using a different bootstrap sample from the original data. About one-third of the cases are left out of the bootstrap sample and not used in the construction of the kth tree.

Put each case left out in the construction of the kth tree down the kth tree to get a classification. In this way, a test set classification is obtained for each case in about one-third of the trees. At the end of the run, take j to be the class that got most of the votes every time case n was oob. The proportion of times that j is not equal to the true class of n averaged over all cases is the oob error estimate. This has proven to be unbiased in many tests."

Load library

```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

Load data

```r
train_data<-read.csv("pml-training.csv",na.strings=c("NA",""))
```

Clean_data and remove unused information
Assuming columns with NA or empty rows are unused variables

```r
NAs <- apply(train_data,2,function(x) {sum(is.na(x))}) 
clean_data <- train_data[,which(NAs == 0)]
remove <- grep("timestamp|X|user_name|new_window|num_window",names(train_data))
clean_data <- clean_data[,-remove]
```

Hold out 20% sample for validation

```r
inTrain<-createDataPartition(y=clean_data$classe,p=0.8,list=FALSE)
clean_data_train<-clean_data[inTrain,]
clean_data_validate<-clean_data[-inTrain,]
```

Model - using random forest

```r
set.seed(12345) 
model_rf<-train(classe~ . , data = clean_data_train, method = 'rf', trControl=trainControl(method='oob'))
```

```
## Loading required package: randomForest
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```r
model_rf
```

```
## Random Forest 
## 
## 15699 samples
##    52 predictors
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Out of Bag Resampling 
## 
## Summary of sample sizes:  
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa
##   2     1         1    
##   30    1         1    
##   50    1         1    
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 27.
```

Assess accuracy and out of sample error

```r
pred <- predict(model_rf, clean_data_validate)
confusionMatrix(pred,clean_data_validate$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1116    3    0    0    0
##          B    0  756    1    0    1
##          C    0    0  682    6    2
##          D    0    0    1  636    0
##          E    0    0    0    1  718
## 
## Overall Statistics
##                                         
##                Accuracy : 0.996         
##                  95% CI : (0.994, 0.998)
##     No Information Rate : 0.284         
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.995         
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             1.000    0.996    0.997    0.989    0.996
## Specificity             0.999    0.999    0.998    1.000    1.000
## Pos Pred Value          0.997    0.997    0.988    0.998    0.999
## Neg Pred Value          1.000    0.999    0.999    0.998    0.999
## Prevalence              0.284    0.193    0.174    0.164    0.184
## Detection Rate          0.284    0.193    0.174    0.162    0.183
## Detection Prevalence    0.285    0.193    0.176    0.162    0.183
## Balanced Accuracy       0.999    0.998    0.997    0.994    0.998
```

Show variable importance

```r
varImp(model_rf)
```

```
## rf variable importance
## 
##   only 20 most important variables shown (out of 52)
## 
##                      Overall
## roll_belt             100.00
## pitch_forearm          59.34
## yaw_belt               54.04
## roll_forearm           44.29
## magnet_dumbbell_y      43.67
## pitch_belt             43.33
## magnet_dumbbell_z      42.98
## accel_dumbbell_y       22.45
## roll_dumbbell          18.49
## magnet_dumbbell_x      17.16
## accel_forearm_x        17.06
## accel_dumbbell_z       14.84
## magnet_belt_z          14.42
## magnet_belt_y          13.58
## total_accel_dumbbell   13.48
## magnet_forearm_z       13.05
## accel_belt_z           12.83
## gyros_belt_z           11.46
## yaw_arm                10.65
## magnet_belt_x           9.22
```

Load and clean test dataset

```r
test_data<-read.csv("pml-testing.csv",na.strings=c("NA",""))
NAs <- apply(test_data,2,function(x) {sum(is.na(x))}) 
clean_test_data <- test_data[,which(NAs == 0)]
remove <- grep("timestamp|X|user_name|new_window|num_window",names(test_data))
clean_test_data <- clean_test_data[,-remove]
```

Output prediction

```r
predictions <- predict(model_rf, clean_test_data)
predictions
```

Write out to file

```r
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictions)
```
