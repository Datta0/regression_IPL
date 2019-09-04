library(MASS)
library(readxl)
require(devtools)
#library(car)
ID_Data <- read_excel("Test.R.xlsx")

IX <- as.matrix(ID_Data[1:6])

IX
Iy<-IX[,6]
Ix<-IX[,1]
Ix1 <- IX[,2]
Ix2 <- IX[,3]
Ix3 <- IX[,4]
Ix4 <- IX[,5]
mod1<-lm(Iy~Ix3+Ix4)
#plot(Ix,Ix1,Ix2,Ix3,Ix4,Iy)
summary(mod1)
res<-residuals(mod1)
rs<-rstudent(mod1)
ra<-rstandard(mod1)
pr<-residuals(mod1)/(1-lm.influence(mod1)$hat)
c<-cbind(res,rs,ra,pr)
py<-predict(mod1)
py
plot(mod1)
plot(py,res)
plot(Ix,res)
qqnorm(res)
pr <- residuals(mod1)
require(ggiraph)
require(ggiraphExtra)
require(plyr)
#ggPredict(lm,se = TRUE, interactive = TRUE)

# weighted 
#modwet<-lm(Iy~Ix, weights = xw)
#summary(modwet)
#Box_cox Transformation
require(car)
Iy
lambda<-boxcox(Iy~Ix3+Ix4, family="yjPower", plotit = TRUE)
ind<-which(lambda$y == max(lambda$y))
lambda.max<-lambda$x[ind]
Iy.tr<-bcPower(Iy, lambda = lambda.max)
modt<-lm(Iy.tr~Ix3+Ix4)
summary(modt)
plot(modt)
rest<-residuals(modt)
pyt<-predict(modt)
plot(pyt,rest)


Hald_Data<-as.data.frame(ID_Data)
colnames(Hald_Data) <- c("x1","x2","x3","x4","x5","y")
Hald_Data
model<-lm(y~x1+x2+x3+x4+x5,data = Hald_Data)
summary(model)

library(olsrr)
ols_step_all_possible(model)

ols_step_forward_p(model,prem = 0.05)

ols_step_backward_p(model,prem = 0.05)

ols_step_both_p(model, details = TRUE)


ols_step_backward_aic(model,details = TRUE)

ols_step_forward_aic(model,details = TRUE)
