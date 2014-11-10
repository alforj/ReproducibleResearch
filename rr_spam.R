## Week 1

## Download kernlab package for spam dataset
install.packages('kernlab')
library('kernlab')
data(spam)
str(spam[,1:5])

## Perforrm subsampling
set.seed(3435)
trainIndicator <- rbinom(4601, size = 1, prob = 0.5)
table(trainIndicator)
trainSpam <- spam[trainIndicator == 1, ]
testSpam <- spam[trainIndicator == 0, ]

##Exploratory Analyis
names(trainSpam)
head(trainSpam)
table(trainSpam$type)
plot(trainSpam$capitalAve ~ trainSpam$type)
# use log to deal with different scales between variable (adding 1 because there are a lot of zeros in the data
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)
# Paris plot to show relationships between different predictors
plot(log10(trainSpam[,1:4] + 1))
# Clustering
# Initial hCluster analysis
hCluster <-  hclust(dist(t(trainSpam[,1:57])))
plot(hCluster)
# Updated hCluster analysis with Log (clustering can be sensitive to different scales in the data
hClusterUpdated <- hclust(dist(t(log(trainSpam[,1:55] + 1))))
plot(hClusterUpdated)

## Statistical Prediction/Modeling

# Can we determine whether an email is spam base on a single variable?
trainSpam$numType <- as.numeric(trainSpam$type) - 1
costFunction <- function(x, y) sum(x != (y > 0.5))
cvError <- rep(NA, 55)
library(boot)
for (i in 1:55) {
        lmFormula <- reformulate(names(trainSpam)[i], response = "numType")
        glmFit <- glm(lmFormula, family = "binomial", data = trainSpam)
        cvError[i] <- cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}

## Which predictor has minimum cross-validated error?
names(trainSpam)[which.min(cvError)]
# [1] "charDollar"

## Use the best model (above) from the group
predictionModel <- glm(numType ~ charDollar, family = "binomial", data = trainSpam)

## Get predictions on the test set
predictionsTest <- predict(predictionModel, testSpam)
predictedSpam <- rep("nonspam", dim(testSpam)[1])

## Clarify as 'spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"

## Classification Table
table(predictedSpam, testSpam$type)

# predictedSpam nonspam spam
# nonspam       1346    458
# spam          61      449

## Eror Rate
(61+458) / (1346 + 458 + 61 + 449)

# [1] 0.2242869
## ~22% error rate between predicted and actual spam values