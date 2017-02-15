#load data
train_data <- read.csv("~/Documents/Education/Centrale_ESSEC/Machine_Learning/Kaggle_Project/data/train.csv", row.names=1)

#plot histograms
par(mfrow=c(4,2))
par(mar = rep(2, 4))
hist(train_data$season,main = "season")
hist(train_data$weather,main = "weather")
hist(train_data$hum,main = "humidity")
hist(train_data$holiday,main = "holiday")
hist(train_data$workingday,main = "working day")
hist(train_data$temp,main = "temp")
hist(train_data$atemp,main = "apparent temp")
hist(train_data$windspeed, main="wind speed")

par(mfrow=c(1,1))
hist(train_data$cnt,main = "bicycle count")

#independent vs dependent boxplots
par(mfrow=c(3,2))
par(mar = rep(2, 4))
boxplot(train_data$cnt~train_data$hr,xlab="hour", ylab="bycicle count", main = "Count vs Hour")
boxplot(train_data$cnt~train_data$weekday,xlab="week day", ylab="bycicle count", main = "Count vs Day")
boxplot(train_data$cnt~train_data$mnth,xlab="month", ylab="bycicle count", main = "Count vs Month")
boxplot(train_data$cnt~train_data$holiday,xlab="holiday", ylab="bycicle count", main = "Count vs Holiday")
boxplot(train_data$cnt~train_data$season,xlab="season", ylab="bycicle count", main = "Count vs Season")
boxplot(train_data$cnt~train_data$weathersit,xlab="weather", ylab="bycicle count", main = "Count vs Weather")

#correlation matrix
library(ggplot2)
library(GGally)

ggpairs(raw_data,columns=c(2:14),ggplot2::aes(colour=yr),main="Correlation Matrix")
ggpairs(train_data,columns=c(2:14),title="Correlation Matrix")

#PCA
library(FactoMineR)
library(ggplot2)

out_pca=PCA(train_data[,c(2:13)],scale.unit=TRUE,ncp=12,graph=TRUE)
summary(out_pca)

eigenv=out_pca$eig$eigenvalue
eigenv_cum=cumsum(out_pca$eig$eigenvalue)/sum(eigenv)
cp=1:length(eigenv)
ev=data.frame(cp=cp,eigenv=eigenv)
ev_cum=data.frame(cp=cp,eigenv_cum=eigenv_cum)

plot(ev$cp,ev$eigenv,type="h",lwd=50,lend="butt",xlab="Principal components",ylab="Eigenvalues",main="Screeplot")

#inertia
plot(ev_cum$cp,ev_cum$eigenv_cum,type="h",lwd=50,lend="butt",xlab="Principal components",ylab="Total inertia resumed",main="Total inertia resumed / Principal components")

# inertia with ggplot:
ggplot(data=ev_cum,aes(x=cp,y=eigenv_cum))+
  geom_bar(stat="identity",fill="steelblue")+
  theme_minimal()+
  ggtitle("Total inertia resumed / Principal components")+
  xlab("Principal components")+
  ylab("Total inertia resumed")+
  scale_x_continuous(breaks=cp)

# X <- raw_data[,]

#Correlation
library(psy)
dependent = "cnt"
independent = c("T9","T12","T15","Ne9", "Ne12","Ne15","Vx9","Vx12","Vx15","maxO3v","wind","rain")
fpca(data=ozone, y=dependent, x=independent, partial="No")

#dataset with principal components as predictors
data_pca = data.frame(out_pca$ind$coord,train_data[,14])

#train and test sets
set.seed(1789)
sel=sample(1:10886,round(10886*0.8))
data_estim=data_pca[sel,]
data_pred=data_pca[-sel,]
erreurs=data.frame(method=character(0),RMSE=numeric(0),MAPE=numeric(0))


#Simple Principal Component Regression
reg_simp=lm(train_data...14.~Dim.1+I(Dim.1^2),data=data_estim)
plot(reg_simp)

#ploting regression line
plot(data_pca[,1],data_pca[,13], type = "p", col = "blue", main = "Count vs Hour", xlab = "PC 1", ylab = "Bicycle Count")
abline(reg_simp)

summary(reg_simp)
pred_simp=predict(reg_simp,data_pred)
res_simp=data_pred$train_data...14-pred_simp
rmse_simp=sqrt(mean((res_simp)^2))
rmse_simp
mape_simp=mean(abs(1-pred_simp/data_pred$train_data...14))*100
mape_simp
erreurs=rbind(erreurs,data.frame(method="Linear (simple)",RMSE=rmse_simp,MAPE=mape_simp))

#feature engineering
library(rpart)
library(rattle) #these libraries will be used to get a good visual plot for the decision tree model. 
library(plot.rpart)
library(RColorBrewer)
d=rpart(cnt~hr,data=train_data)
plot(d)

#Polynomial regression
reg_poly=lm(maxO3~T12+I(T12^2),data=ozone_estim)
plot(reg_poly)
summary(reg_poly)
pred_poly=predict(reg_poly,ozone_pred)
res_poly=ozone_pred$maxO3-pred_poly
rmse_poly=sqrt(mean((res_poly)^2))
rmse_poly
mape_poly=mean(abs(1-pred_poly/ozone_pred$maxO3))*100
mape_poly
erreurs=rbind(erreurs,data.frame(method="Linear (polynomial)",RMSE=rmse_poly,MAPE=mape_poly))

#Multiple regression
reg_mult=lm(maxO3~T12+Ne9+maxO3v,data=ozone_estim)
plot(reg_mult)
summary(reg_mult)
pred_mult=predict(reg_mult,ozone_pred)
res_mult=ozone_pred$maxO3-pred_mult
rmse_mult=sqrt(mean((res_mult)^2))
rmse_mult
mape_mult=mean(abs(1-pred_mult/ozone_pred$maxO3))*100
mape_mult
erreurs=rbind(erreurs,data.frame(method="Linear (multiple)",RMSE=rmse_mult,MAPE=mape_mult))

#Ridge regression
library(MASS)
plot(lm.ridge(maxO3~T9+T12+T15+Ne9+Ne12+Ne15+maxO3v,data=ozone_estim,lambda=seq(0,20,0.01)))
select(lm.ridge(maxO3~T9+T12+T15+Ne9+Ne12+Ne15+maxO3v,data=ozone_estim,lambda=seq(0,20,0.01)))
reg_ridge=lm.ridge(maxO3~T9+T12+T15+Ne9+Ne12+Ne15+maxO3v,data=ozone_estim,lambda=7.53)
summary(reg_ridge)
pred_ridge=as.matrix(data.frame(c=1,ozone_pred[,c(2:7,11)]))%*%as.matrix(coef(reg_ridge))
res_ridge=ozone_pred$maxO3-t(pred_ridge)
rmse_ridge=sqrt(mean((res_ridge)^2))
rmse_ridge
mape_ridge=mean(abs(1-pred_ridge/ozone_pred$maxO3))*100
mape_ridge
erreurs=rbind(erreurs,data.frame(method="Ridge",RMSE=rmse_ridge,MAPE=mape_ridge))

#LASSO regression
library(lars)
reg_lasso=lars(data.matrix(ozone_estim[,c(2:7,11)]),ozone_estim[,2],type="lasso")
plot(reg_lasso)
summary(reg_lasso)
index.lasso=seq(0,1,length=100)
cv=cv.lars(data.matrix(ozone_estim[,c(3:8,12)]),ozone_estim[,2],K=4,type="lasso",index=index.lasso)
s_opt=index.lasso[which.min(cv$cv)]
pred_lasso=predict(reg_lasso,data.matrix(ozone_pred[,c(3:8,12)]),s=s_opt,mode="fraction")
res_lasso=ozone_pred$maxO3-pred_lasso$fit
rmse_lasso=sqrt(mean((res_lasso)^2))
rmse_lasso
mape_lasso=mean(abs(1-pred_lasso$fit/ozone_pred$maxO3))*100
mape_lasso
erreurs=rbind(erreurs,data.frame(method="LASSO (LARS)",RMSE=rmse_lasso,MAPE=mape_lasso))


#Table
library(knitr)
kable(erreurs,digits=2,format="markdown")
