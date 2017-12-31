# Practical-Machine-Learning
Peer-graded Assignment


Preparing Platform and Loading Libraries
# Loading Libraries
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(knitr)

#Setting Seed
set.seed(123456789)

Loading and Processing Data
# Loading Training Data
trainDataURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
allTrainingData <- read.csv(url(trainDataURL),na.strings=c("NA","DIV/0!",""))
dim(allTrainingData)
[1] 19622   160

#Loading Testing Data
testDataURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testingData <- read.csv(url(testDataURL),na.strings=c("NA","DIV/0!",""))
dim(testingData)
[1]  20 160

# Partitioning  all Training Data to Training Data and Validating Data
partTraniningData <- createDataPartition(allTrainingData$classe, p=0.6, list=FALSE)
trainingData <- allTrainingData[partTraniningData , ]
validatingData <- allTrainingData[-partTraniningData , ]
dim(trainingData)
[1] 11776   160
dim (validatingData)
[1] 7846  160

Data Cleansing and Preparing
# Filtering out unrelated features (e.g. first feature)
trainingData<- trainingData[c(-1)]
dim(trainingData)
[1] 11776   159
validatingData <- validatingData[c(-1)]
dim(validatingData)
[1] 7846  159
testingData <- testingData[c(-1)]
dim(testingData) 
[1]  20 159

#Filtering out features with more than 70% NA values
cleanedData <-trainingData
	for( i in 1:length(trainingData)){
		if(sum( is.na(trainingData[,i])) / nrow(trainingData)>= 0.7){
			for(j in 1:length(cleanedData)){
				if(length(grep(names(trainingData[i]),names(cleanedData)[j])) ==1){
					cleanedData <- cleanedData[ , -j]
				}
			}
		}
	}
dim(cleanedData)
[1] 11776    59
trainingData <- cleanedData
dim(trainingData)
[1] 11776    59
rm(cleanedData)

validatingData <- validatingData[colnames(trainingData)]
dim(validatingData)
[1] 7846   59
testingData <- testingData[colnames(trainingData[,-59])]
dim(testingData)
[1] 20 58

# Coerce the data into the same type
for(i in 1:length(testingData)){
	for(j in 1:length(trainingData)){
		if(length (grep(names(trainingData[i]), names(testingData)[j])) ==1) {
			class(testingData[j]) <- class(trainingData[i])
		}
	}
}
dim(testingData)
[1] 20 58
testingData <- rbind(trainingData[2,-59] ,testingData)
dim(testingData)
[1] 21 58
testingData<- testingData[-1,]
dim(testingData)
[1] 20 58

Training Data using Decision Tree Model
dtModel <- rpart(classe~. , data=trainingData,method= "class")
fancyRpartPlot(dtModel) 
 

#Validating trained Decision Tree Model on validatingData
validationPredictDT <- predict(dtModel, validatingData, type="class")
conMatDT <- confusionMatrix(validationPredictDT ,validatingData$classe)
conMatDT 
$positive
NULL

$table
          Reference
Prediction    A    B    C    D    E
         A 2147   73   10    2    0
         B   61 1270  100   57    0
         C   24  162 1238  128   55
         D    0   13   20 1046  186
         E    0    0    0   53 1201

$overall
      Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
     0.8796839      0.8478685      0.8722802      0.8868043      0.2844762 
AccuracyPValue  McnemarPValue 
     0.0000000            NaN 

$byClass
         Sensitivity Specificity Pos Pred Value Neg Pred Value Precision
Class: A   0.9619176   0.9848593      0.9619176      0.9848593 0.9619176
Class: B   0.8366271   0.9655499      0.8534946      0.9609940 0.8534946
Class: C   0.9049708   0.9430380      0.7703796      0.9791633 0.7703796
Class: D   0.8133748   0.9666159      0.8268775      0.9635314 0.8268775
Class: E   0.8328710   0.9917239      0.9577352      0.9634405 0.9577352
            Recall        F1 Prevalence Detection Rate Detection Prevalence
Class: A 0.9619176 0.9619176  0.2844762      0.2736426            0.2844762
Class: B 0.8366271 0.8449767  0.1934744      0.1618659            0.1896508
Class: C 0.9049708 0.8322689  0.1743564      0.1577874            0.2048177
Class: D 0.8133748 0.8200706  0.1639052      0.1333163            0.1612287
Class: E 0.8328710 0.8909496  0.1837879      0.1530716            0.1598267
         Balanced Accuracy
Class: A         0.9733884
Class: B         0.9010885
Class: C         0.9240044
Class: D         0.8899953
Class: E         0.9122975

$mode
[1] "sens_spec"

$dots
list()

attr(,"class")
[1] "confusionMatrix"

plot(conMatDT$table , col = conMatDT$byClass, main=paste("Decision Tree Confusion Matrix: Accuracy =", round(conMatDT$overall['Accuracy'], 4)))
 



Training data using Random Forest Model
rfModel <- randomForest(classe~., data= trainingData)
#Validating trained Random Forest Model on validatingData
validationPredictRF <- predict (rfModel, validatingData, type="class")
conMatRF <- confusionMatrix(validationPredictRF ,validatingData$classe)
conMatRF
$positive
NULL

$table
          Reference
Prediction    A    B    C    D    E
         A 2232    3    0    0    0
         B    0 1515    4    0    0
         C    0    0 1364    0    0
         D    0    0    0 1286    0
         E    0    0    0    0 1442

$overall
      Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
     0.9991078      0.9988715      0.9981626      0.9996412      0.2844762 
AccuracyPValue  McnemarPValue 
     0.0000000            NaN 

$byClass
         Sensitivity Specificity Pos Pred Value Neg Pred Value Precision
Class: A   1.0000000   0.9994656      0.9986577      1.0000000 0.9986577
Class: B   0.9980237   0.9993679      0.9973667      0.9995258 0.9973667
Class: C   0.9970760   1.0000000      1.0000000      0.9993829 1.0000000
Class: D   1.0000000   1.0000000      1.0000000      1.0000000 1.0000000
Class: E   1.0000000   1.0000000      1.0000000      1.0000000 1.0000000
            Recall        F1 Prevalence Detection Rate Detection Prevalence
Class: A 1.0000000 0.9993284  0.2844762      0.2844762            0.2848585
Class: B 0.9980237 0.9976951  0.1934744      0.1930920            0.1936018
Class: C 0.9970760 0.9985359  0.1743564      0.1738465            0.1738465
Class: D 1.0000000 1.0000000  0.1639052      0.1639052            0.1639052
Class: E 1.0000000 1.0000000  0.1837879      0.1837879            0.1837879
         Balanced Accuracy
Class: A         0.9997328
Class: B         0.9986958
Class: C         0.9985380
Class: D         1.0000000
Class: E         1.0000000

$mode
[1] "sens_spec"

$dots
list()

attr(,"class")
[1] "confusionMatrix"


plot(rfModel)
 



plot(conMatRF$table,col=conMatDT$byClass , main= paste("Random Forest Confusion Matrix: Accuracy =" , round (conMatRF$overall['Accuracy'],4)))
 



Training data using Generalized Boosted Regression
#Cross Validation
crossValid=trainControl(method="repeatedcv", number=5,allowParallel=TRUE,verboseIter=TRUE)

# Training data using Generalized Boosted Regression
gbrModel <- train(classe~. , data=trainingData, method= "gbm", trControl= crossValid, verbose= FALSE)
+ Fold1.Rep1: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
- Fold1.Rep1: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
+ Fold1.Rep1: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
- Fold1.Rep1: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
+ Fold1.Rep1: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
- Fold1.Rep1: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
+ Fold2.Rep1: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
- Fold2.Rep1: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
+ Fold2.Rep1: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
- Fold2.Rep1: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
+ Fold2.Rep1: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
- Fold2.Rep1: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
+ Fold3.Rep1: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
- Fold3.Rep1: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
+ Fold3.Rep1: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
- Fold3.Rep1: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
+ Fold3.Rep1: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
- Fold3.Rep1: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
+ Fold4.Rep1: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
- Fold4.Rep1: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
+ Fold4.Rep1: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
- Fold4.Rep1: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
+ Fold4.Rep1: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
- Fold4.Rep1: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
+ Fold5.Rep1: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
- Fold5.Rep1: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
+ Fold5.Rep1: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
- Fold5.Rep1: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
+ Fold5.Rep1: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
- Fold5.Rep1: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
Aggregating results
Selecting tuning parameters
Fitting n.trees = 150, interaction.depth = 3, shrinkage = 0.1, n.minobsinnode = 10 on full training set

#Validating trained Generalized Boosted Regression Model on validatingData
validationPredictGBR <- predict (gbrModel, validatingData)
conMatGBR <- confusionMatrix(validationPredictGBR ,validatingData$classe)
conMatGBR
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 2229    3    0    0    0
         B    3 1512    5    0    0
         C    0    2 1356    6    0
         D    0    1    7 1278    2
         E    0    0    0    2 1440

Overall Statistics
                                          
               Accuracy : 0.996           
                 95% CI : (0.9944, 0.9973)
    No Information Rate : 0.2845          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.995           
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            0.9987   0.9960   0.9912   0.9938   0.9986
Specificity            0.9995   0.9987   0.9988   0.9985   0.9997
Pos Pred Value         0.9987   0.9947   0.9941   0.9922   0.9986
Neg Pred Value         0.9995   0.9991   0.9981   0.9988   0.9997
Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
Detection Rate         0.2841   0.1927   0.1728   0.1629   0.1835
Detection Prevalence   0.2845   0.1937   0.1738   0.1642   0.1838
Balanced Accuracy      0.9991   0.9974   0.9950   0.9961   0.9992



plot(gbrModel , ylim=c(0.9, 1))
 



Predicting Results on the Test Data
predictTestDT<- predict(dtModel, testingData, type="class")
predictTestDT
1 2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
 B  A  C  A  A  E  D  C  A  A  B  C  B  A  E  E  A  B  B  B 
Levels: A B C D E

predictTestRF <- predict(rfModel, testingData, type="class")
predictTestRF 
1 2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
 B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
Levels: A B C D E

predictTestGBR <- predict(gbrModel, testingData)
predictTestGBR
[1] B A B A A E D B A A B C B A E E A B B B
Levels: A B C D E
