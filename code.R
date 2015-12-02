dataset <- read.csv("~/Desktop/columbia/inference/project/dataset.csv")

#1 Restructure Variables
##create competition exist variable
dataset$date1<-as.Date(dataset$Date)
for (i in 1:nrow(dataset)){
  if (!dataset$CompetitionOpenSinceMonth[i] %in% 1:12) {
    dataset$Compexist[i]<-0
    } else if (dataset$CompetitionOpenSinceMonth[i]<10) {
    tmp1<-paste(dataset$CompetitionOpenSinceYear[i],paste(0,dataset$CompetitionOpenSinceMonth[i],sep = ""),sep="-")
    tmp2<-paste(tmp1,"-01",sep="")
    dataset$Compopen[i]<-as.Date(tmp2)
    ifelse (as.Date(tmp2)<dataset$date1[i],dataset$Compexist[i]<-1,dataset$Compexist[i]<-0)
    } else if (dataset$CompetitionOpenSinceMonth[i]>9) {
    tmp1<-paste(dataset$CompetitionOpenSinceYear[i],dataset$CompetitionOpenSinceMonth[i],sep="-")
    tmp2<-paste(tmp1,"-01",sep="")
    dataset$Compopen[i]<-as.Date(tmp2)
    ifelse (as.Date(tmp2)<dataset$date1[i],dataset$Compexist[i]<-1,dataset$Compexist[i]<-0)
    }
}
##sale date variable-month, year, yr_month
dataset$date_m<-as.factor(format(dataset$date1,"%m"))
dataset$date_d<-as.factor(format(dataset$date1,"%y"))
dataset$date_ym<-as.factor(format(dataset$date1,"%y-%m"))

dataset$X<-NULL;dataset$Compopen<-NULL
dataset$Store=as.factor(dataset$Store);dataset$DayOfWeek=as.factor(dataset$DayOfWeek)
dataset$Open=as.factor(dataset$Open);dataset$Promo=as.factor(dataset$Promo);dataset$SchoolHoliday=as.factor(dataset$SchoolHoliday)
dataset$CompetitionOpenSinceMonth=as.factor(dataset$CompetitionOpenSinceMonth);dataset$CompetitionOpenSinceYear=as.factor(dataset$CompetitionOpenSinceYear)
dataset$Promo2=as.factor(dataset$Promo2);dataset$Promo2SinceWeek=as.factor(dataset$Promo2SinceWeek)
dataset$Promo2SinceYear=as.factor(dataset$Promo2SinceYear);dataset$Compexist=as.factor(dataset$Compexist)
dataset$date1=as.factor(dataset$date1)

str(dataset)
dataset0<-dataset
#write.csv(dataset0,file="~/Desktop/columbia/inference/project/dataset0.csv")

#2 Features Selection: boosting tree
require(xgboost)
require(methods)
require(data.table)
require(magrittr)
for (i in names(dataset0)) {
  if (class(dataset0[[i]])=="factor" | class(dataset0[[i]])=="integer") {
    dataset0[[i]] <- as.numeric(dataset0[[i]])
  }
}
str(dataset0)
dataset0<-as.matrix(dataset0)

numberOfClasses <- max(y) + 1
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = numberOfClasses)
cv.nround <- 5
cv.nfold <- 3
bst= xgb.cv(param=param, data =dataset0, label = y, 
                nfold = cv.nfold, nrounds = cv.nround)
names <- dimnames(dataset0)[[2]]
importance_matrix <- xgb.importance(names, model = bst)
xgb.plot.importance(importance_matrix[1:10,])

#3 Linear regression
set.seed(123)
#######################NEED TO CHANGE VAIRABLES FROM LAST PART##################
train_index<-sample(c(1:nrow(dataset0)),nrow(dataset0)*0.8)
train<-dataset0[train_index,]
test<-dataset0[-train_index,]

glmnet.fit<-cv.glmnet(train[,-c("Sales")],train$Sales,type.measure='mse',alpha=1)
summary(glmnet.fit)
glm.pred<-predict(glmnet.fit,test[,-c("Sales")],s=glmnet.fit$lambda.min)
auc(test$Sales,glm.pred)
glm.pred<-as.data.frame(glm.pred)
names(glm.pred)<-"pred"
glm<-as.data.frame(cbind(glm.pred$pred,test$Sales))
colnames(glm)<-c('pred','control')
RMSE(glm$pred,glm$control)




