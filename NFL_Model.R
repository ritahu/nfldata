setwd("/Users/user/Desktop/Classes/STAT 425/Projects/")
load("NFL_Data.Rdata")

## Packages needed
library(car)
library(corrplot)

## Data Preparation
Train<-NFL_Data[NFL_Data$Season!=2015,]
Test<-NFL_Data[NFL_Data$Season==2015,]
Train.y<-as.numeric(as.character((Train$pct)))
Test.y<-as.numeric(as.character((Test$pct)))
## Response Transformation (0,1) to (-Inf,Inf)
Train.Y<-log(Train.y/(1-Train.y))
Test.Y<-log(Test.y/(1-Test.y))
## 161 pct=1
## 222 pct=0
## Transform
Train.Y[Train.Y==Inf]=3
Train.Y[Train.Y==-Inf]=-3
Test.Y[Test.Y==Inf]=3
Test.Y[Test.Y==-Inf]=-3
Train.X<-Train[,c(3:4,6:27)]
Test.X<-Test[,c(3:4,6:27)]
## Normal
par(mfrow=c(1,2))
qqnorm(Train.Y)
hist(Train.Y)
## numerical matrix transformation
index<-which(names(Train.X)%in%"Pass.YD")
n=dim(Train.X)[1]
p=dim(Train.X)[2]
train.X<-matrix(rep(NA,n*p),ncol=p)
for(i in 1:24){
  train.X[,i]<-as.numeric(as.character((Train.X[,i])))
}
n=dim(Test.X)[1]
p=dim(Test.X)[2]
test.X<-matrix(rep(NA,n*p),ncol=p)
for(i in 1:24){
  test.X[,i]<-as.numeric(as.character((Test.X[,i])))
}
test.data<-data.frame(test.X)
colnames(test.data)=names(Test.X)
Team.test<-data.frame(Test$Team,Test.y)
Test_rank<-data.frame(Team.test[order(Team.test$Test.y,decreasing=T),],rank=1:32)

## make up training dataframe for model
dataxy<-data.frame(train.X,Train.Y)
colnames(dataxy)=c(names(Train.X),"Train.Y")

## alias
Linear_model1<-lm(Train.Y~.,data=dataxy)
alias.model=alias(Linear_model1)
data=dataxy[,-which(colnames(dataxy)%in%rownames(alias.model$Complete))]

## transform_back function
transform_back<-function(x){
  exp(x)/(exp(x)+1)
}

## Model1: OLS (BenchMark)
OLS_model<-lm(Train.Y~.,data=data)
OLS_predict<-predict(OLS_model,newdata=test.data)
Pred.y.OLS<-transform_back(OLS_predict)
Team.pred<-data.frame(Test$Team,Pred.y.OLS)
Pred_rank_OLS<-data.frame(Team.pred[order(Team.pred$Pred.y.OLS,decreasing=T),],rank=1:32)

## Model2: VIF
kick.var<-NA
i=1
for (i in 1:dim(data)[2]-1){
  Linear_model_VIF<-lm(Train.Y~.,data=data)
  if(sum(vif(Linear_model_VIF)>=10)!=0){
    kick.var<-c(kick.var, which.max(vif(Linear_model_VIF)))
    data=data[,-as.numeric(which.max(vif(Linear_model_VIF)))]
    i=i+1
  }
}
## summary(Linear_model_VIF)
## DoubleCheck we already kick out all the VIF>10 variables
## vif(Linear_model_VIF)
## the variables we kick out
## kick.var
## corrplot
## corrplot(cor(data))
## Test
Linear_predict_VIF<-predict(Linear_model_VIF,newdata=test.data)
Pred.y.VIF<-transform_back(Linear_predict_VIF)
Team.pred<-data.frame(Test$Team,Pred.y.VIF)
Pred_rank_VIF<-data.frame(Team.pred[order(Team.pred$Pred.y.VIF,decreasing=T),],rank=1:32)

## Model3: PCA
PCA_Model<-prcomp(train.X, center = TRUE, scale. = TRUE) 
PCA_sum=summary(PCA_Model)
keepPC.number<-which(PCA_sum$importance[3,]>0.9)[1]
new.x<-matrix(rep(NA,nrow(train.X)*as.numeric(keepPC.number)),ncol=as.numeric(keepPC.number))
for(i in 1:10){
  new.x[,i]<-scale(train.X)%*%PCA_Model$rotation[,i]
}
newdataxy<-data.frame(new.x,Train.Y)
colnames(newdataxy)=c(paste("PC",1:10,sep=""),"Train.Y")
PCA_regression<-lm(Train.Y~.,data=newdataxy)
PCA_regression_sum<-summary(PCA_regression)
PCA_index<-as.numeric(which(PCA_regression_sum$coefficients[,4]<0.05)-1)
pc.important<-PCA_Model$rotation[,PCA_index]
explain<-list(NULL)
length(explain)<-7
for(i in 1:7){
  name<-names(Train.X)[abs(pc.important[,i])>=0.3]
  coef<-pc.important[,i][abs(pc.important[,i])>=0.3]
  PC.coef<-PCA_regression_sum$coef[PCA_index+1,1][i]
  explain[[i]]<-list(name=name,coef=coef,PC.coef=PC.coef)
}
## Test
PCA_predict<-cbind(rep(1,32),scale(test.X)%*%pc.important)%*%PCA_regression_sum$coef[c(1,PCA_index+1),1]
Pred.y.pca<-transform_back(PCA_predict)
Team.pred.pca<-data.frame(Test$Team,Pred.y.pca)
Pred_rank_pca<-data.frame(Team.pred.pca[order(Team.pred.pca$Pred.y.pca,decreasing=T),],rank=1:32)

## Model4: Random Forest
library(randomForest)
n<-dim(train.X)[1]
set.seed(183)
nfold = 10
infold = sample(rep(1:nfold, length.out=n))
ntree<-c(1,5,10,50,100,150,200,300,400,500,800,1000)
xgrid = expand.grid(x1 = 1:10, x2 = ntree)
rf.10fold.error<-function(fc){
  train<-which(infold != fc[1])
  rf.fit = randomForest(train.X[train,], Train.Y[train], ntree = fc[2])
  mean((Train.Y[-train]-predict(rf.fit, train.X[-train,]))^2)
}
system.time(errormatrix<-matrix(unlist(apply(as.matrix(xgrid),1,rf.10fold.error)),ncol=10,byrow=T))
result<-unlist(apply(errormatrix,1,mean))
## Variable Importance
rf.fit = randomForest(train.X, Train.Y, ntree = as.numeric(xgrid[which.min(result)*10,][2]),importance=T)
round(importance(rf.fit,type=2), 4)
## Test
Tree_predict<-predict(rf.fit, test.X)
Pred.y.tree<-transform_back(Tree_predict)
Team.pred.tree<-data.frame(Test$Team,Pred.y.tree)
Pred_rank_tree<-data.frame(Team.pred.tree[order(Team.pred.tree$Pred.y.tree,decreasing=T),],rank=1:32)

## Model5: Lasso
library(glmnet)
##ml.lasso <-glmnet(train.X, Train.Y, alpha = 1)
ml.cv.lasso <- cv.glmnet(train.X, Train.Y, alpha = 1, nfolds = 10)
##coef(ml.cv.lasso)
#plot(ml.lasso,"lambda")
#plot(ml.cv.lasso)
#ml.lasso.coef <- coef(ml.cv.lasso, s = "lambda.min", exact = TRUE)
## Test
Lasso_predict<-predict(ml.cv.lasso, newx = test.X, s = "lambda.min")
Pred.y.lasso<-transform_back(as.numeric(Lasso_predict))
Team.pred.lasso<-data.frame(Test$Team,Pred.y.lasso)
Pred_rank_lasso<-data.frame(Team.pred.lasso[order(Team.pred.lasso$Pred.y.lasso,decreasing=T),],rank=1:32)

## Model6-10
## Model Selection with AIC BIC Cp
## stepwise with AIC
OLS_model<-lm(Train.Y~.,data=data)
stepwise.aic.model=step(OLS_model, direction="both",trace=0)  # AIC
stepwise_aic_predict<-predict(stepwise.aic.model, newdata=test.data, interval="prediction" )[,1]
Pred.y.swAIC<-transform_back(stepwise_aic_predict)
Team.pred.swAIC<-data.frame(Test$Team,Pred.y.swAIC)
Pred_rank_swAIC<-data.frame(Team.pred.swAIC[order(Team.pred.swAIC$Pred.y.swAIC,decreasing=T),],rank=1:32)

## stepwise with BIC
stepwise.bic.model=step(OLS_model, direction="both", k=log(dim(data)[1]),trace=0)  # BIC 
stepwise_bic_predict<-predict(stepwise.bic.model, newdata=test.data, interval="prediction" )[,1]
Pred.y.swBIC<-transform_back(stepwise_bic_predict)
Team.pred.swBIC<-data.frame(Test$Team,Pred.y.swBIC)
Pred_rank_swBIC<-data.frame(Team.pred.swBIC[order(Team.pred.swBIC$Pred.y.swBIC,decreasing=T),],rank=1:32)

## best subset
library(leaps)
RSSleaps=regsubsets(data[,-16],data$Train.Y)
sumleaps=summary(RSSleaps,matrix=T)
## model size (including intercept)
msize=apply(sumleaps$which,1,sum)

## best subset with Cp
best.p.cp<-which.min(sumleaps$cp)
## the varibles keep
tm1<-paste("Train.Y","~",sep="")
tm2<-paste(names(which(sumleaps$which[best.p.cp,]))[-1],collapse = "+")
fam<-formula(paste(tm1,tm2))
ml.cp<-lm(fam, data=data)
subset_cp_predict<-predict(ml.cp, newdata=test.data, interval="prediction" )[,1]
Pred.y.ssCP<-transform_back(subset_cp_predict)
Team.pred.ssCP<-data.frame(Test$Team,Pred.y.ssCP)
Pred_rank_ssCP<-data.frame(Team.pred.ssCP[order(Team.pred.ssCP$Pred.y.ssCP,decreasing=T),],rank=1:32)

## best subset with AIC
best.p.aic<-which.min(dim(data)[1]*log(sumleaps$rss/dim(data)[1]) + 2*msize)
tm1<-paste("Train.Y","~",sep="")
tm2<-paste(names(which(sumleaps$which[best.p.aic,]))[-1],collapse = "+")
fam<-formula(paste(tm1,tm2))
ml.aic<-lm(fam, data=data)
subset_aic_predict<-predict(ml.aic, newdata=test.data, interval="prediction" )[,1]
Pred.y.ssAIC<-transform_back(subset_aic_predict)
Team.pred.ssAIC<-data.frame(Test$Team,Pred.y.ssAIC)
Pred_rank_ssAIC<-data.frame(Team.pred.ssAIC[order(Team.pred.ssAIC$Pred.y.ssAIC,decreasing=T),],rank=1:32)

## best subset with BIC
best.p.bic<-which.min(sumleaps$bic)
tm1<-paste("Train.Y","~",sep="")
tm2<-paste(names(which(sumleaps$which[best.p.bic,]))[-1],collapse = "+")
fam<-formula(paste(tm1,tm2))
ml.bic<-lm(fam, data=data)
subset_bic_predict<-predict(ml.bic, newdata=test.data, interval="prediction" )[,1]
Pred.y.ssBIC<-transform_back(subset_bic_predict)
Team.pred.ssBIC<-data.frame(Test$Team,Pred.y.ssBIC)
Pred_rank_ssBIC<-data.frame(Team.pred.ssBIC[order(Team.pred.ssBIC$Pred.y.ssBIC,decreasing=T),],rank=1:32)

## Save result
Model_pred_result<-cbind(Test_rank,Pred_rank_OLS,Pred_rank_VIF,Pred_rank_pca,
                         Pred_rank_tree,Pred_rank_lasso,
                         Pred_rank_swAIC,Pred_rank_swBIC,
                         Pred_rank_ssCP,Pred_rank_ssAIC,Pred_rank_ssAIC)
save(Model_pred_result,file="Model_pred_result.Rdata")
library(xlsx)
write.xlsx(Model_pred_result, "Model_pred_result.xlsx")

## RMSE
load("Model_pred_result.Rdata")
err<-function(k){
  test.pct<-rep(NA,32)
  for(i in 1:32){
    test.pct[i]<-Model_pred_result[,2][Model_pred_result[,1]%in%Model_pred_result[,3*k+1][i]]
  }
  mean((test.pct-Model_pred_result[,3*k+2])^2)
}
#err(1)##OLS
#err(2)##VIF
#err(3)##PCA
#err(4)##RF
#err(5)##Lasso
#err(6)##stepwise with AIC
#err(7)##stepwise with BIC
#err(8)##subset with Cp
#err(9)##subset with AIC
#err(10)##subset with BIC
error<-NA
for(i in 1:10){
  error<-c(error,err(i))
}
Test.error<-data.frame(matrix(error[-1],nrow=1))
names(Test.error)<-c("OLS","VIF","PCA","RF","Lasso","swAIC","swBIC","ssCp","ssAIC","ssBIC")
kable(Test.error[,1:5])
kable(Test.error[,6:10])
save(Test.error,file="Test.error.Rdata")
