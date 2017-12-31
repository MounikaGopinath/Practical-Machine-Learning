# Loading Libraries
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(rattle)
library(knitr)

#Setting Seed
set.seed(123456789)

# Loading Data
trainDataURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"

testDataURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

allTrainingData <- read.csv(url(trainDataURL),na.strings=c("NA","DIV/0!",""))

testingData <- read.csv(url(testDataURL),na.strings=c("NA","DIV/0!",""))

dim(allTrainingData)

dim(testingData)

# Partitioning  all Training Data to Training Data and Validating Data
partTraniningData <- createDataPartition(allTrainingData$classe, p=0.6, list=FALSE)

trainingData <- allTrainingData[partTraniningData , ]

validatingData <- allTrainingData[-partTraniningData , ]

dim(trainingData)

dim (validatingData)

# Data Cleansing and Preparing
# Filtering out unrelated features (e.g. first feature)
trainingData<- trainingData[c(-1)]

dim(trainingData)

validatingData <- validatingData[c(-1)]

dim(validatingData)

testingData <- testingData[c(-1)]

dim(testingData) 

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

trainingData <- cleanedData

dim(trainingData)

rm(cleanedData)

validatingData <- validatingData[colnames(trainingData)]

dim(validatingData)

testingData <- testingData[colnames(trainingData[,-59])]

dim(testingData)

# Coerce the data into the same type
for(i in 1:length(testingData)){
	for(j in 1:length(trainingData)){
		if(length (grep(names(trainingData[i]), names(testingData)[j])) ==1) {
			class(testingData[j]) <- class(trainingData[i])
		}
	}
}

dim(testingData)

testingData <- rbind(trainingData[2,-59] ,testingData)
dim(testingData)

testingData<- testingData[-1,]

dim(testingData)

# Training data using Decision Tree Model
dtModel <- rpart(classe~. , data=trainingData,method= "class")

fancyRpartPlot(dtModel)

#Validating trained Decision Tree Model on validatingData
validationPredictDT <- predict(dtModel, validatingData, type="class")

conMatDT <- confusionMatrix(validationPredictDT ,validatingData$classe)

conMatDT 


plot(conMatDT$table , col = conMatDT$byClass, main=paste("Decision Tree Confusion Matrix: Accuracy =", round(conMatDT$overall['Accuracy'], 4)))


#Cross Validation
crossValid=trainControl(method="cv", number=3,allowParallel=TRUE,verboseIter=TRUE)


# Training data using Random Forest Model
#rfModel <- randomForest(classe~., data= trainingData)
rfModel <- train(classe~.,data=trainingData,method="rf",trControl=crossValid)


#Validating trained Random Forest Model on validatingData
validationPredictRF <- predict (rfModel, validatingData)

conMatRF <- confusionMatrix(validationPredictRF ,validatingData$classe)

conMatRF

plot(rfModel)

plot(conMatRF$table,col=conMatDT$byClass , main= paste("Random Forest Confusion Matrix: Accuracy =" , round (conMatRF$overall['Accuracy'],4)))
