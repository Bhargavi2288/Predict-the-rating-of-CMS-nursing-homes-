#Install required packages
install.packages("hmeasure")
# load the data
library(MASS) 
library(class) 
library(hmeasure)
library(ROCR)
provider_data<-read.csv("ProviderInfo_September2017.csv")
# Remove the missing data 
provider_data<-na.omit(provider_data)
#Look at the dimensions of the data
dim(provider_data)
# Split overall_rating into two parts <3(Bad) and >=3(Good)
provider_data$overall_rating<-cut(provider_data$overall_rating,2,include.lowest = FALSE,labels = c("0","1"))
#typecast overall rating as numerical
provider_data$overall_rating=as.numeric(levels(provider_data$overall_rating)[provider_data$overall_rating])
provider_data$overall_rating<-as.factor(provider_data$overall_rating)
# Recode INHOSP as 0 and 1
provider_data$INHOSP=recode(provider_data$INHOSP,"'YES'=1;else=0")
#typecast INHOSP  as numerical
provider_data$INHOSP=as.numeric(levels(provider_data$INHOSP)[provider_data$INHOSP])
provider_data$INHOSP<-as.factor(provider_data$INHOSP)
# Convert WEIGHTED_ALL_CYCLES_SCORE to Numeric
provider_data$WEIGHTED_ALL_CYCLES_SCORE=as.numeric(provider_data$WEIGHTED_ALL_CYCLES_SCORE)

# Group levels for Ownership into "ForProfit","NonProfit" and "Government"
oTypes <- list(
  ForProfit = c("For profit - Corporation","For profit - Limited Liability company","For profit - Individual","For profit - Partnership"),
  Government  = c("Government - City","Government - County","Government - Hospital district","Government - City/county","Government - Federal","Government - State"),
  NonProfit = c("Non profit - Church related", "Non profit - Corporation" ,"Non profit - Other")
)
for (i in 1:length(oTypes)) 
  levels(provider_data$OWNERSHIP)[levels(provider_data$OWNERSHIP)%in%oTypes[[i]]] <- names(oTypes)[i]
provider_data$OWNERSHIP<-recode(provider_data$OWNERSHIP,"'ForProfit'=1;'Government'=2;'NonProfit'=0")
#Typecast Ownership as numeric
provider_data$OWNERSHIP=as.numeric(levels(provider_data$OWNERSHIP)[provider_data$OWNERSHIP])
provider_data$OWNERSHIP<-as.factor(provider_data$OWNERSHIP)
xpvdrdata <- provider_data[,c(23,10,11,12,14,44,48,73,79)]
# split it into training and test
set.seed(1)
train <- sample(length(provider_data$overall_rating)*0.75)
xpvdrdata.train <- xpvdrdata[train,]
xpvdrdata.test <- xpvdrdata[-train,]
ytrain <- provider_data$overall_rating[train]
ynew <- provider_data$overall_rating[-train]
true.class<-xpvdrdata.test[,1]

# train k-NN classifier
class.knn1 <- knn(train=xpvdrdata.train[,-1], test=xpvdrdata.test[,-1],
                  cl=xpvdrdata.train$overall_rating, k=1, prob=TRUE, use.all=TRUE)
class.knn3 <- knn(train=xpvdrdata.train[,-1], test=xpvdrdata.test[,-1],
                  cl=xpvdrdata.train$overall_rating, k=3, prob=TRUE, use.all=TRUE)
class.knn5 <- knn(train=xpvdrdata.train[,-1], test=xpvdrdata.test[,-1],
                  cl=xpvdrdata.train$overall_rating, k=5, prob=TRUE, use.all=TRUE)
class.knn7 <- knn(train=xpvdrdata.train[,-1], test=xpvdrdata.test[,-1],
                  cl=xpvdrdata.train$overall_rating, k=7, prob=TRUE, use.all=TRUE)
class.knn9 <- knn(train=xpvdrdata.train[,-1], test=xpvdrdata.test[,-1],
                  cl=xpvdrdata.train$overall_rating, k=9, prob=TRUE, use.all=TRUE)
data.frame(ynew,class.knn1,class.knn3,class.knn5,class.knn7,class.knn9)[1:10,]
## calculate the proportion of correct classifications
pcorrn1=100*sum(ynew==class.knn1)/length(ynew)
pcorrn3=100*sum(ynew==class.knn3)/length(ynew)
pcorrn5=100*sum(ynew==class.knn5)/length(ynew)
pcorrn7=100*sum(ynew==class.knn7)/length(ynew)
pcorrn9=100*sum(ynew==class.knn9)/length(ynew)
#K=3 give the best proportion of correct classification
#------------------------_ROC CURVE-----------------------------------------------------------------
#lets plot and ROC for K-nearest neighbor
scores.knn1 <- attr(class.knn1,"prob")
scores.knn3 <- attr(class.knn3,"prob")
scores.knn5 <- attr(class.knn5,"prob")
scores.knn7 <- attr(class.knn7,"prob")
scores.knn9 <- attr(class.knn9,"prob")
# this is necessary because k-NN by default outputs
# the posterior probability of the winning class
scores.knn1[class.knn1=="0"] <- 1-scores.knn1[class.knn1=="0"] 
scores.knn3[class.knn3=="0"] <- 1-scores.knn3[class.knn3=="0"] 
scores.knn5[class.knn5=="0"] <- 1-scores.knn5[class.knn5=="0"] 
scores.knn7[class.knn7=="0"] <- 1-scores.knn7[class.knn7=="0"] 
scores.knn9[class.knn9=="0"] <- 1-scores.knn9[class.knn9=="0"] 
# run the HMeasure function on the data frame of scores

results1 <- HMeasure(true.class,scores.knn1)
results3 <- HMeasure(true.class,scores.knn3)
results5 <- HMeasure(true.class,scores.knn5)
results7 <- HMeasure(true.class,scores.knn7)
results9 <- HMeasure(true.class,scores.knn9)
# produce the ROC 
par(mfrow=c(2,3))

plotROC(results1,which=1)
rect(0,1.1,1,1.7,xpd=TRUE,col="white",border = "white")
title("1-Nearest Neighbor ROC CURVE")
plotROC(results3,which=1)
rect(0,1.1,1,1.7,xpd=TRUE,col="white",border = "white")
title("3-Nearest Neighbor ROC CURVE")
plotROC(results5,which=1)
rect(0,1.1,1,1.7,xpd=TRUE,col="white",border = "white")
title("5-Nearest Neighbor ROC CURVE")
plotROC(results7,which=1)
rect(0,1.1,1,1.7,xpd=TRUE,col="white",border = "white")
title("7-Nearest Neighbor ROC CURVE")
plotROC(results9,which=1)
rect(0,1.1,1,1.7,xpd=TRUE,col="white",border = "white")
title("9-Nearest Neighbor ROC CURVE")
#--------------------------------------------------------------------------------------------------------------------------

