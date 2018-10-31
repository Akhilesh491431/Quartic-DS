library(mice)
library(VIM)
library(dplyr)
library(moments)
library(xgboost)
library(mlr)
library(randomForest)
#loading train and test datasets
traindf<-read.table(file.choose(),header=TRUE,sep=",",as.is=TRUE)
testdf<-read.table(file.choose(),header=TRUE,sep=",",as.is=TRUE)
as.factor(traindf$cat2)
#understanding data
colnames(traindf)
summary(traindf)
str(traindf)
#datatype changing
catcols<-c("cat1","cat2","cat3","cat4","cat5","cat6","cat7","cat8","cat9","cat10","cat11","cat12","cat13","cat14")
traindf[catcols]<-lapply(traindf[catcols],factor)
traindf$id<-as.factor(traindf$id)
traindf$target<-as.factor(traindf$target)
dercols<-c("der4","der5","der6","der7","der8","der9","der15","der16","der17","der18","der19")
#dercols<-c("der15","der16","der17","der18","der19")
traindf[dercols]<-lapply(traindf[dercols],factor)
table(traindf$target)/nrow(traindf) # biased data only 3 percent in target variable are of value 1
colSums(is.na(traindf)) # checking missing values
#num18-107909 num19-5 num20-1 num22-42667
#cat1-217 cat2-83 cat3-5814 cat4-107 cat5-5 cat6-411792 cat8-266928 cat10-11503 cat12-570
#creating mice plot to understand pattern in missing values 
#train_miss<-aggr(traindf,plot=TRUE)
#since missing values in num19,num20,cat1,cat2,cat4,cat5 are less removing the observations for which these columns has missing values
ind1<-which(is.na(traindf$num19))
traindf<-traindf[-ind1,]
ind2<-which(is.na(traindf$num20))
traindf<-traindf[-ind2,]
ind3<-which(is.na(traindf$cat1))
traindf<-traindf[-ind3,]
ind4<-which(is.na(traindf$cat2))
traindf<-traindf[-ind4,]
ind5<-which(is.na(traindf$cat4))
traindf<-traindf[-ind5,]
ind6<-which(is.na(traindf$cat5))
traindf<-traindf[-ind6,]
ind7<-which(is.na(traindf$cat12))
traindf<-traindf[-ind7,]
#missing value imputation
#num18
summary(traindf$num18)
107707/nrow(traindf)
skewness(traindf$num18,na.rm=TRUE) #1.065
indnum18<-which(is.na(traindf$num18))
traindf$num18[indnum18]<-median(traindf$num18,na.rm=TRUE)
#num22
summary(traindf$num22)
skewness(traindf$num22,na.rm=TRUE)
indnum22<-which(is.na(traindf$num22))
traindf$num22[indnum22]<-median(traindf$num22,na.rm=TRUE)
#cat6
summary(traindf$cat6)
table(traindf$cat6,traindf$target)
411537/nrow(traindf) # 69 percent missing values So removing varaible
traindf<-select(traindf,-cat6)
#cat8
table(traindf$cat8,traindf$target)
266810/nrow(traindf) # 44 percent missing values so removing varibale
traindf<-select(traindf,-cat8)
#cat3 
summary(traindf$cat3)
indcat3<-which(is.na(traindf$cat3))
traindf$cat3[indcat3]<-"0"
#cat10
summary(traindf$cat10)
indcat10<-which(is.na(traindf$cat10))
traindf$cat10[indcat10]<-"1"
train<-select(traindf,-id,-cat14)
rf<-randomForest(formula=target~.,data=train,ntree=100,mtry=3,importance=TRUE,type="classification")
varimp<-as.data.frame(importance(rf))
varimp<-varimp[order(-varimp$MeanDecreaseAccuracy),]
impcols<-rownames(varimp)[1:32]
impcols[33]<-"target"
train<-select(train,impcols)
#creating dummy variables\
traindummies<-createDummyFeatures(train,cols=c("cat9","cat4","cat12","cat3","cat7","cat2","cat5","cat10","cat11","cat1","cat13","der8","der18"))
#dividing train dataset to validate
set.seed(1234)
rows<-1:nrow(traindummies)
no <- sample(rows,nrow(traindummies)*0.75)
train1 <- traindummies[no,]
labelstrain<-as.numeric(as.character(train1$target))
train1<-select(train1,-target)
test <- traindummies[-no,]
labeltest<-as.numeric(as.character(test$target))
test<-select(test,-target)
#XG-BOOST
model1<-xgboost(data=data.matrix(train1),label = labelstrain,nrounds=60,verbose=1,
                print_every_n = 2,eta=0.2,objective="binary:logistic",max_depth=20)
y<-predict(model1,data.matrix(test))
y_0_1<-ifelse(y>0.5,1,0)
testtable<-table(labeltest,y_0_1)
y_train<-predict(model1,data.matrix(train1))

y_train_0_1<-ifelse(y_train>0.5,1,0)
traintable<-table(labelstrain,y_train_0_1)
#train
(traintable[1,1]+traintable[2,2])/(traintable[1,1]+traintable[2,2]+traintable[1,2]+traintable[2,1]) #accu-0.98388
(traintable[2,2])/(traintable[1,2]+traintable[2,2]) #prec-1
(traintable[2,2])/(traintable[2,1]+traintable[2,2]) # recall-0.79
#test
(testtable[1,1]+testtable[2,2])/(testtable[1,1]+testtable[2,2]+testtable[1,2]+testtable[2,1]) #accu-0.9633
(testtable[2,2])/(testtable[1,2]+testtable[2,2]) #prec-0.72
(testtable[2,2])/(testtable[2,1]+testtable[2,2]) # recall-0.0016
##############
model2<-xgboost(data=data.matrix(train1),label = labelstrain,nrounds=50,verbose=1,
                print_every_n = 2,eta=0.2,objective="binary:logistic",max_depth=20,colsample_bytree=0.5)
y<-predict(model2,data.matrix(test))
y_0_1<-ifelse(y>0.5,1,0)
testtable<-table(labeltest,y_0_1)
y_train<-predict(model2,data.matrix(train1))

y_train_0_1<-ifelse(y_train>0.5,1,0)
traintable<-table(labelstrain,y_train_0_1)
#train
(traintable[1,1]+traintable[2,2])/(traintable[1,1]+traintable[2,2]+traintable[1,2]+traintable[2,1]) #accu-0.98388
(traintable[2,2])/(traintable[1,2]+traintable[2,2]) #prec-1
(traintable[2,2])/(traintable[2,1]+traintable[2,2]) # recall-0.61
#test
(testtable[1,1]+testtable[2,2])/(testtable[1,1]+testtable[2,2]+testtable[1,2]+testtable[2,1]) #accu-0.9631
(testtable[2,2])/(testtable[1,2]+testtable[2,2]) #prec-0.69
(testtable[2,2])/(testtable[2,1]+testtable[2,2]) # recall-0.0016

#finalmodel
train_y<-as.numeric(as.character(traindummies$target))
traindummies<-select(traindummies,-target)
finalmodel<-xgboost(data=data.matrix(traindummies),label = train_y,nrounds=50,verbose=1,
                            print_every_n = 2,eta=0.2,objective="binary:logistic",max_depth=20,colsample_bytree=0.5)


final_predicted_y<-predict(finalmodel,data.matrix(traindummies))
final_labelled_y<-ifelse(final_predicted_y>0.4,1,0)
table(train_y,final_labelled_y)

#prediction on test dataset
testdfids<-testdf$id
testdf<-select(testdf,impcols[1:32])
#daattypechanging
testcols<-c("cat9","cat4","cat12","cat3","cat7","cat2","cat5","cat10","cat11","cat1","cat13","der8","der18")
testdf[testcols]<-lapply(testdf[testcols],factor)
colSums(is.na(testdf))
#missing value imputation
#cat4
table(testdf$cat4)
indcat4<-which(is.na(testdf$cat4))
testdf$cat4[indcat4]<-"11"
#num22
indnum22<-which(is.na(testdf$num22))
testdf$num22[indnum22]<-median(testdf$num22,na.rm=TRUE)
#num18
indnum18<-which(is.na(testdf$num18))
testdf$num18[indnum18]<-median(testdf$num18,na.rm=TRUE)
#cat12
table(testdf$cat12)
indcat12<-which(is.na(testdf$cat12))
testdf$cat12[indcat12]<-"2"
#cat3
table(testdf$cat3)
indcat3<-which(is.na(testdf$cat3))
testdf$cat3[indcat3]<-"0"
#cat2
table(testdf$cat2)
indcat2<-which(is.na(testdf$cat2))
testdf$cat2[indcat2]<-"0"
#cat5,cat10,cat1
#cat5
table(testdf$cat5)
indcat5<-which(is.na(testdf$cat5))
testdf$cat5[indcat5]<-"1"
#cat10
table(testdf$cat10)
indcat10<-which(is.na(testdf$cat10))
testdf$cat10[indcat10]<-"1"
#cat1
table(testdf$cat1)
indcat1<-which(is.na(testdf$cat1))
testdf$cat1[indcat1]<-"1"
#dummies creation
testdummies<-createDummyFeatures(testdf,cols=c("cat9","cat4","cat12","cat3","cat7","cat2","cat5","cat10","cat11","cat1","cat13","der8","der18"))
testdummies<-select(testdummies,-der8.1)
names(testdummies)
testprediction<-predict(finalmodel,data.matrix(testdummies))
names(traindummies)
testpredictionlabels<-ifelse(testprediction>0.4,1,0)
finaloutput<-data.frame(id=testdfids,target=testprediction)
write.csv(finaloutput,"/home/techm/akhilesh/quartic-ds/predicted_output.csv",row.names = FALSE)
