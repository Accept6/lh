library(e1071)
setwd("C:/Users/Accept/Desktop/R/lqm")
x=read.table("wine.txt");
y=read.table("labels.txt");
wine <- as.matrix(x)    #将数据框转换为矩阵
wine_labels <- as.matrix(y)
a <- c(1:30,60:95,131:153);    
train_wine  <- wine[a,];
train_wine_labels <- wine_labels[a];
b <- c(31:59,96:130,154:178);
test_wine <- wine[b,];
test_wine_labels <- wine_labels[b];
d <- c(1:30,60:95,131:153,31:59,96:130,154:178);
dataset <-wine[d,];
scalefun <- function(train)
{(train-min(train))/(max(train)-min(train))
}
dataset_scale <- apply(dataset,2,scalefun);
train_wine_scale <- dataset_scale[1:89,];
test_wine_scale <- dataset_scale[90:178,];
#wine_svm <- svm(train_wine_scale,train_wine_labels,type='C-classification',kernel='polynomial',C=3,scaled=c())
wine_svm <- svm(train_wine_scale,train_wine_labels,type='C-classification',kernel='sigmoid',C=9,scaled=c())
#attributes(wine_svm);
#plot(wine_svm,data=train_wine);
predict_labels = predict(wine_svm,test_wine_scale);
predict_labels <- as.numeric( predict_labels )
table <- table(test_wine_labels,predict_labels)
sum(diag(prop.table(table)))
plot(test_wine_labels,pch=c(1),ann=FALSE);
par(new=T)
plot(predict_labels,pch=c(3),col="red",ann=FALSE);
legend(2.5,3,c("实际测试集分类","预测测试集分类"),pch=c(1,3),col=c("black","red"));
title(main="测试集的实际分类和预测分类图",xlab="测试集样本",ylab="类别标签")

