library(plyr)
library(dplyr)

train_file_url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_file_url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

train_file_name = "pml-training.csv"
test_file_name = "pml-testing.csv"

if (!file.exists(train_file_name)){
  download.file(train_file_url, train_file_name)
}

if (!file.exists(test_file_name)){
  download.file(test_file_url, test_file_name)
}

pml_training <- read.table("pml-training.csv", header=TRUE, sep=",", as.is = TRUE)
pml_testing <- read.table("pml-testing.csv", header=TRUE, sep=",", as.is = TRUE)

for (i in 8:159){
  if (class(pml_training[,i])=="character"){
    pml_training[,i] <- as.numeric(pml_training[,i]) 
    pml_testing[,i] <- as.numeric(pml_testing[,i]) 
  }
}

summary_columns <- grep("(max)|(min)|(kurtosis)|(skewness)|(amplitude)|(var)|(avg)|(stddev)", names(pml_training))


set.seed(2583)
library(caret)

filtered_pml_training <- pml_training[, -(c(1:7, summary_columns))]
filtered_pml_testing <- pml_testing[, -(c(1:7, summary_columns,160))]
inTrain <- createDataPartition(y=filtered_pml_training$classe, p=0.8, list=FALSE)

training <- filtered_pml_training[inTrain,]
cross_validation <- filtered_pml_training[-inTrain,]

x <- training[,-53]
y <- training[, 53]

library(parallel)
library(doParallel)

cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method="cv", number=10, allowParallel = TRUE)

#modFitRf <- train(x,y,  method="rf", data=training, trControl=fitControl)
modFitGbm <-train(x, y,  method="gbm", verbose=FALSE, trControl = fitControl)

stopCluster(cluster)
registerDoSEQ()

#save(modFitRf, file = "courseraModelRf.rda")
save(modFitGbm, file = "courseraModelGbm.rda")
#pred <- predict(modFit, cross_validation)

predGbm <- predict(modFitGbm, cross_validation)
confMxGbm <- confusionMatrix(predGbm, cross_validation$classe)


load("courseraModelRf.rda")
predRf <- predict(modFitRf, cross_validation)
confMxRf <- confusionMatrix(predRf, cross_validation$classe)

library(ggplot)
ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "purple3") +
  theme_bw() + theme(legend.position = "none")


prediction <- predict(modFitRf, filtered_pml_testing)
