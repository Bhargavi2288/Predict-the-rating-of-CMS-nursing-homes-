#Install Required Packages
install.packages("binr")
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)
install.packages("ggthemes",dependencies = TRUE)
install.packages('ggmosaic', dependencies = TRUE)
install.packages('ggplots', dependencies = TRUE)
install.packages('ROCR', dependencies = TRUE)
library(binr)
library(car)
library(Rcpp)
library(ggplot2)
library(data.table)
library(ggthemes)
library(ggmosaic)
library(lattice)
library(ROCR)
par(mfrow=c(2,2))
provider_data<-read.csv("ProviderInfo_September2017.csv")
# Remove the missing data 
provider_data<-na.omit(provider_data)
#Look at the dimensions of the data
dim(provider_data)
# Split overall rating in 2 levels 0 for Bad and 1 for Good
provider_data$overall_rating<-cut(provider_data$overall_rating,2,include.lowest = FALSE,labels = c(0,1))
provider_data$overall_rating=as.numeric(levels(provider_data$overall_rating)[provider_data$overall_rating])

# Recode INHOSP as 0 and 1
provider_data$INHOSP=recode(provider_data$INHOSP,"'YES'=1;else=0")
# Recode SFF as 0 and 1
provider_data$SFF=recode(provider_data$SFF,"'Y'=1;else=0")
# Convert WEIGHTED_ALL_CYCLES_SCORE to Numeric
provider_data$WEIGHTED_ALL_CYCLES_SCORE=as.numeric(provider_data$WEIGHTED_ALL_CYCLES_SCORE)
# Reduce levels for Ownership to "ForProfit","NonProfit" and "Government"
oTypes <- list(
  ForProfit = c("For profit - Corporation","For profit - Limited Liability company","For profit - Individual","For profit - Partnership"),
  Government  = c("Government - City","Government - County","Government - Hospital district","Government - City/county","Government - Federal","Government - State"),
  NonProfit = c("Non profit - Church related", "Non profit - Corporation" ,"Non profit - Other")
)
for (i in 1:length(oTypes)) 
  levels(provider_data$OWNERSHIP)[levels(provider_data$OWNERSHIP)%in%oTypes[[i]]] <- names(oTypes)[i]
# Plot onwership against overall rating
t1 <- table(provider_data$OWNERSHIP,provider_data$overall_rating)
ggplot(data = provider_data) +
  ggmosaic::geom_mosaic(aes( x = ggmosaic::product(overall_rating, OWNERSHIP), fill=factor(overall_rating)), na.rm=TRUE) +    theme(axis.text.x=element_text(angle=-25, hjust= .1)) +
  labs(x=" Overall Rating vs Ownership ", title='f(overall_rating, Ownership)')  + guides(fill=guide_legend(title = "Overall Rating", reverse = TRUE))

# Take subset of data for regression purpose
#Dependent or Response variable is Overall_Rating
xpvdrdata <- provider_data[,c(10,11,12,14,44,48,73,79)]
## creating training and prediction datasets
## select 75% rows for estimation and 25% for testing
set.seed(1)
train <- sample(length(provider_data$overall_rating)*0.75)
xtrain <- xpvdrdata[train,]
xnew <- xpvdrdata[-train,]
ytrain <- provider_data$overall_rating[train]
ynew <- provider_data$overall_rating[-train]
# Training Data Frame
settrain<-data.frame(overall_rating=ytrain,xtrain)
# Run logistic regression on training data set
prvdrRating=glm(overall_rating~.,family=binomial, data=settrain)
summary(prvdrRating)

# Predcition in validation data set
# Validation Data Frame
setnew<-data.frame(overall_rating=ynew,xnew)
#Prediction in the validation data set
pvalid <- predict(prvdrRating, newdata=setnew,type="response")
#pvalid <- predict(prvdrRating, newdata=data.frame(overall_rating=ynew,xnew),type="response")
predValid<-data.frame(ynew,pvalid)
#Calculate the misclassifciation rate in validation data set
gg1=floor(pvalid+(1/2))
ttt=table(ynew,gg1)
validation_MR=(ttt[1,2]+ttt[2,1])/length(predValid$ynew)
validation_MR
# Prediction in Training Data Set
ptrain <- predict(prvdrRating, newdata=settrain,type="response")
predTrain<-data.frame(ytrain,ptrain)
#Calculate the misclassifciation rate in validation data set
gg2=floor(ptrain+(1/2))
ttt2=table(ytrain,gg2)
train_MR=(ttt2[1,2]+ttt2[2,1])/length(predTrain$ytrain)
train_MR
#------------------- ROCR PLOTS---------------------------------------------------------------
# ROCR for In-Sample cases
predictions=ptrain
labels=ytrain
data=data.frame(predictions,labels)
data
## pred: function to create prediction objects
pred <- prediction(data$predictions,data$labels)
pred
## perf: creates the input to be plotted
## sensitivity and one minus specificity (the false positive rate)
perf <- performance(pred, "sens", "fpr")
plot(perf,main="ROC CURVE-TRAINING SET")
## ROC for validation cases
predictions=pvalid
labels=ynew
data=data.frame(predictions,labels)
pred <- prediction(data$predictions,data$labels)
perf <- performance(pred, "sens", "fpr")
plot(perf,main="ROC CURVE-VALIDATION SET")
#------------------- Lift PLOTS---------------------------------------------------------------
# Training Set
predTrain=predTrain[order(ptrain,decreasing=TRUE),]
## order cases in test set according to their success prob
## actual outcome shown next to it
## overall success (delay) prob in the evaluation data set
xbar=mean(ytrain)
xbar
## calculating the lift
## cumulative 1’s sorted by predicted values
## cumulative 1’s using the average success prob from
## evaluation set
n2=floor(length(predTrain$ytrain))
axis=dim(n2)
ax=dim(n2)
ay=dim(n2)
axis[1]=1
ax[1]=xbar
ay[1]=predTrain[1,2]
for (i in 2:n2) {
  axis[i]=i
  ax[i]=xbar*i
  ay[i]=ay[i-1]+predTrain[i,2]
}

aaa=cbind(predTrain[,1],predTrain[,2],ay,ax)
aaa[1:100,]

plot(axis,ay,xlab="number of cases",ylab="number of successes", main="Lift: Training Set")
points(axis,ax,type="l")
#-----------------------------------------------------------------------------------------------
# Validation Set
predValid=predValid[order(pvalid,decreasing=TRUE),]
## order cases in test set according to their success prob
## actual outcome shown next to it
## overall success (delay) prob in the evaluation data set
xbar=mean(ynew)
xbar
## calculating the lift
## cumulative 1’s sorted by predicted values
## cumulative 1’s using the average success prob from
## evaluation set
n2=floor(length(predValid$ynew))
axis=dim(n2)
ax=dim(n2)
ay=dim(n2)
axis[1]=1
ax[1]=xbar
ay[1]=predValid[1,2]
for (i in 2:n2) {
  axis[i]=i
  ax[i]=xbar*i
  ay[i]=ay[i-1]+predValid[i,2]
}
aaa=cbind(predValid[,1],predValid[,2],ay,ax)
aaa[1:100,]
plot(axis,ay,xlab="number of cases",ylab="number of successes", main="Lift: Validation Set")
points(axis,ax,type="l")