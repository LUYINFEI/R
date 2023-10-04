#设置工作目录
setwd("E:/GABP")
windowsFonts(myFont1 = windowsFont("Times New Roman"))  # 设定文字字体"Times New Roman"
element_text(family='Times_New_Roman', size = 13, face='bold')
#加载包
library(VIM)
library(randomForest)
library(pROC)
library(caret)
library(tidyverse)
#GABPS<- read.csv("0822_2.csv",header = TRUE)#加载文件
#GABPS$group <- factor(GABPS$group) #定义变量
#pillar::glimpse(GABPS)    #查看每列数据的数据类型和变量值。
 
save(GABPS,file = "GABPS_0822_插补数据.Rdata")
load(file = "GABPS_0822_插补数据.Rdata")

write.csv(GABPS,file="E:/GABP/GABPS_0822_rfImpute.csv",quote=F,row.names = F)

#去掉目的变量
#GABPS=select(GABPS, -SerumGABP )
GABPS$group <- factor(GABPS$group) #定义变量
pillar::glimpse(GABPS)    #查看每列数据的数据类型和变量值。
##存在缺失值的数据集插补（随机森林法），得到最优的样本拟合值。

#检查缺失值
aggr(GABPS,numbers=TRUE,ylab=c("Histogram of missing GABPS","Pattern")) 
#write.csv(GABPS,file="E:/A.csv",quote=F,row.names = F)

#设置种子数目
set.seed(20220)
#划分训练测试集
trainlist <- createDataPartition(GABPS$group,    #a vector of outcomes
                                 p = 0.7,    #训练数据占比，随机抽取
                                 list = F)
trainset <- GABPS[trainlist, ]    #抽取出训练集
testset <- GABPS[-trainlist, ]    #与训练集相对的30%作为测试集

#随机森林
GABPS_rf <- randomForest(formula = group ~ .,
                         data = trainset, 
                         importance = T,
                         iter = 5,    #迭代次数。
                         ntree = 2000,
                         proximity=TRUE,   
                         na.action = na.omit)    #去除缺失值
GABPS_rf
save(GABPS_rf,file = "GABPS_0822_2.Rdata")
load(file = "GABPS_0822_2.Rdata")

save(GABPS_rf,file = "GABPS_0822_1.Rdata")

plot(GABPS_rf)

GABPS_x <- GABPS_rf$importance
varImpPlot(GABPS_rf, main = "variable importance")
par(family = "Times New Roman")
varImpPlot(GABPS_rf, 
           n.var = min(10, nrow(GABPS_rf$importance)),
           main = "Top 10 - Variable importance")
GABPS_x_importance<-data.frame(GABPS_x)
write.csv(GABPS_x_importance,file="E:/GABP/GABPS_YES_importance1.csv",quote=T,row.names = T)
#第2种ROC
#使用测试集评估
rf_predict2 <- predict(GABPS_rf,type='prob',newdata = testset)##生成预测值
rf_predict2
rf_predict2[,2]
#取算法所得阳性概率作ROC图
roc2 <- roc(testset$group,   #实际观测值，DM和DN
            rf_predict2[,2])    #预测值，须为数值，1=DM，2=DN；这个值也可以是指标
roc2
plot(roc2, print.auc=T, auc.polygon=T, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=T,
     auc.polygon.col="skyblue", 
     print.thres=T)
round(auc(roc2),3)##AUC
round(ci(roc2),3)##95%CI
#第1种ROC
rf_predict1 <- predict(GABPS_rf,type='response',newdata = testset)##生成预测值
rf_predict1

roc1 <- roc(testset$group,   #实际观测值，DM和DN
               as.numeric(rf_predict1))    #预测值，须为数值，1=DM，2=DN；这个值也可以是指标
roc1
par(family = "Times New Roman")    #设置字体，否则无法显示中文
plot(roc1, print.auc = TRUE,
     xlim = c(1, 0),
     auc.polygon=TRUE, 
     grid=c(0.1, 0.2),
     grid.col=c("green", "red"), 
     max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", 
     print.thres=TRUE,
     main='ROC of Random Forest model')
round(auc(roc1),3)##AUC
round(ci(roc1),3)##95%CI

#训练集自身测试
rf_predict3 <- predict(GABPS_rf, newdata = trainset)
rf_predict3
obs_p_ran <- data.frame(prob = rf_predict3, obs = trainset$group)
obs_p_ran
table(trainset$group, rf_predict3, dnn = c("True value","Predict value"))
ran_roc <- roc(trainset$group,   #实际观测值，DM和DN
               as.numeric(rf_predict3)) 
#训练集混淆矩阵
rf.cf3 <- caret::confusionMatrix(as.factor(rf_predict3), 
                                 as.factor(trainset$group))
rf.cf3
rf.cf3$table
fourfoldplot(rf.cf3$table, color = c("cyan", "pink"),
             std = c("margins"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

#使用测试集评估模型
rf_predict1 <- predict(GABPS_rf,type='response',newdata = testset)##生成预测值
rf_predict1#输出值为预测的等级，即DN和DM
#测试集混淆矩阵
rf.cf <- caret::confusionMatrix(as.factor(rf_predict1), 
                                as.factor(testset$group))
rf.cf
rf.cf$table
fourfoldplot(rf.cf$table, color = c("cyan", "pink"),
             std = c("margins"),
             conf.level = 0, margin = 1, main = "Confusion Matrix" )
x1 <-rf.cf$table
fourfoldplot(x1)

#------------------------------------------------------------
setwd("E:/GABP")
#加载包
library(VIM)
library(randomForest)
library(pROC)
library(caret)
library(tidyverse)
GABPS<- read.csv("A1.csv",header = TRUE)#加载文件
#去掉目的变量
GABPS=select(GABPS, -SerumGABP)
SerumGABP_A 
GABPS3=select(GABPS2, -SerumGABP_A)
GABPS<-GABPS3
GABPS$group <- factor(GABPS$group) #定义变量
pillar::glimpse(GABPS)    #查看每列数据的数据类型和变量值。
##存在缺失值的数据集插补（随机森林法），得到最优的样本拟合值。

#检查缺失值
aggr(GABPS,numbers=TRUE,ylab=c("Histogram of missing GABPS","Pattern")) 
#write.csv(GABPS,file="E:/A.csv",quote=F,row.names = F)

#设置种子数目
set.seed(20220)
set.seed(20222)

#划分训练测试集
trainlist <- createDataPartition(GABPS$group,    #a vector of outcomes
                                 p = 0.7,    #训练数据占比，随机抽取
                                 list = F)
trainset <- GABPS[trainlist, ]    #抽取出训练集
testset <- GABPS[-trainlist, ]    #与训练集相对的30%作为测试集

#随机森林
GABPS_rf <- randomForest(formula = group ~ .,
                         data = trainset, 
                         importance = T,
                         iter = 5,    #迭代次数。
                         ntree = 2000,
                         proximity=TRUE,   
                         na.action = na.omit)    #去除缺失值
GABPS_rf
save(GABPS_rf,file = "GABPS_0822_no_2.Rdata")

plot(GABPS_rf)

GABPS_x <- GABPS_rf$importance
varImpPlot(GABPS_rf, main = "variable importance")
par(family = "Times New Roman")
varImpPlot(GABPS_rf, 
           n.var = min(10, nrow(GABPS_rf$importance)),
           main = "Top 10 - Variable importance")
GABPS_x_importance<-data.frame(GABPS_x)
write.csv(GABPS_x_importance,file="E:/GABP/GABPS_YES_importance1.csv",quote=T,row.names = T)
#第2种ROC
#使用测试集评估
rf_predict2 <- predict(GABPS_rf,type='prob',newdata = testset)##生成预测值
rf_predict2
rf_predict2[,2]
#取算法所得阳性概率作ROC图
roc2 <- roc(testset$group,   #实际观测值，DM和DN
            rf_predict2[,2])    #预测值，须为数值，1=DM，2=DN；这个值也可以是指标
roc2
plot(roc2, print.auc=T, auc.polygon=T, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=T,
     auc.polygon.col="skyblue", 
     print.thres=T)
round(auc(roc2),3)##AUC
round(ci(roc2),3)##95%CI
#第1种ROC
rf_predict1 <- predict(GABPS_rf,type='response',newdata = testset)##生成预测值
rf_predict1

roc1 <- roc(testset$group,   #实际观测值，DM和DN
            as.numeric(rf_predict1))    #预测值，须为数值，1=DM，2=DN；这个值也可以是指标
roc1
par(family = "Times New Roman")    #设置字体，否则无法显示中文
plot(roc1, print.auc = TRUE,
     xlim = c(1, 0),
     auc.polygon=TRUE, 
     grid=c(0.1, 0.2),
     grid.col=c("green", "red"), 
     max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", 
     print.thres=TRUE,
     main='ROC of Random Forest model')
round(auc(roc1),3)##AUC
round(ci(roc1),3)##95%CI

#训练集自身测试
rf_predict3 <- predict(GABPS_rf, newdata = trainset)
rf_predict3
obs_p_ran <- data.frame(prob = rf_predict3, obs = trainset$group)
obs_p_ran
table(trainset$group, rf_predict3, dnn = c("True value","Predict value"))
ran_roc <- roc(trainset$group,   #实际观测值，DM和DN
               as.numeric(rf_predict3)) 
#训练集混淆矩阵
rf.cf3 <- caret::confusionMatrix(as.factor(rf_predict3), 
                                 as.factor(trainset$group))
rf.cf3
rf.cf3$table
fourfoldplot(rf.cf3$table, color = c("cyan", "pink"),
             std = c("margins"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

#使用测试集评估模型
rf_predict1 <- predict(GABPS_rf,type='response',newdata = testset)##生成预测值
rf_predict1#输出值为预测的等级，即DN和DM
#测试集混淆矩阵
rf.cf <- caret::confusionMatrix(as.factor(rf_predict1), 
                                as.factor(testset$group))
rf.cf
rf.cf$table
fourfoldplot(rf.cf$table, color = c("cyan", "pink"),
             std = c("margins"),
             conf.level = 0, margin = 1, main = "Confusion Matrix" )
x1 <-rf.cf$table
fourfoldplot(x1)
#---------------------------------
#变量相关性
options(digits=2) 
data <- read.csv("A.csv",header = TRUE,row.names = 1)
data = read.csv("A.csv",header = TRUE)
x <- cor(data)
GABPS.cor
library(corrgram) 
corrgram(data, order=TRUE, lower.panel=panel.shade, 
         upper.panel=panel.pie, text.panel=panel.txt, 
         main="")
library(corrplot)
corrplot(x, method="circle", col=col)
corrplot(x, method="pie")
corrplot(x, method="color")
corrplot(x, method="number")

corrplot(x, type="upper")
corrplot(x, type="lower")

col<- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(20)
corrplot(x, type="upper", order="hclust", col=col)


library(RColorBrewer)
corrplot(M, type="upper", order="hclust", 
         col=brewer.pal(n=8, name="RdBu"))

corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="PuOr"))

#修改背景
corrplot(M, type="upper", order="hclust", col=c("black", "white"),
         bg="lightblue")

#修改字体
corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(x, method = "shade", type = {"lower"}, shade.col = NA, 
         tl.col = "black", tl.srt = 0, col = col(200), addCoef.col = "black", 
         cl.pos = NULL, order = "AOE")
