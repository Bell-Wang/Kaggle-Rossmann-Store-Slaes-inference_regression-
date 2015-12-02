store <- read.csv("~/Desktop/columbia/inference/project/store.csv")
train <- read.csv("~/Desktop/columbia/inference/project/train.csv")
trainset<-train[sample(1:nrow(train),nrow(train)*0.5),]
write.csv(trainset,file="~/Desktop/columbia/inference/project/trainset.csv")
library(dplyr)
dataset<-left_join(trainset,store,by="Store")
write.csv(dataset,file="~/Desktop/columbia/inference/project/dataset.csv")
