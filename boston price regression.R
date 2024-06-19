library(tidyverse)
library(car) 
library(GGally)
library(caret)
library(leaps)
library(MASS)
library(glmnet)
boston <- read_csv("boston.csv")
str(boston)
boston$B
ggplot(boston)+
  geom_bar(aes(x=AGE))

boston <- mutate(boston,CHAS=factor(CHAS))
ggpairs(boston,showStrips = T)
boston |> dplyr::select(where(is.numeric)) |> ggcorr(label=T)
set.seed(123456)
x.id <- createDataPartition(boston$MEDV,p=0.8,list=F)[,1]
train <- boston  |> slice(x.id)
test <- boston |> slice(-x.id)
subset <- regsubsets(MEDV~.,train)
plot(subset) #ZN,CHAS,NOX,RM,DIS,PTRATIO,B,LSTAT
plot(subset,scale="adjr2")#ZN,CHAS,NOX,RM,DIS,PTRATIO,B,LSTAT
plot(subset,scale="Cp")#ZN,CHAS,NOX,RM,DIS,PTRATIO,B,LSTAT
fit_full <- lm(MEDV~.,train)
fit_null <- lm(MEDV~1,train)
fit1 <- lm(MEDV~CHAS+DIS+LSTAT+NOX+PTRATIO+RM,train)
fit2 <- update(fit1,.~.+ZN+B)
fit2_1 <- update(fit2,.~.-ZN)
fit3 <- update(fit2,.~.+CRIM+RAD+TAX)
fit3_1 <- update(fit3,.~.-B)
fit4 <- update(fit2,.~.+CRIM)
fit4_1 <- update(fit4,.~.-B-CRIM)
fit5 <- update(fit3,.~.-B)
AIC(fit1,fit2,fit2_1,fit3,fit3_1,fit4)
BIC(fit1,fit2,fit2_1,fit3,fit3_1,fit4)
summary(fit2_1)
par(mfrow=c(2,2))
plot(fit2_1_1)
par(mfrow=c(1,1))
crPlots(fit2_1_1)
fit2_1_1 <- update(fit2_1,.~.+I(LSTAT^2)+I(RM^2))
vif(fit2_1_1)
influencePlot(fit2_1_1)

predicted <- predict(fit2_1_1,newdata=test)
defaultSummary(data.frame(obs=test$MEDV,
                          pred=predicted))
summary(fit2_1_1)$sigma
summary(fit2_1_1)$r.squared
fc <- cbind(test |> 
              rownames_to_column(var="name"),
            pred=predicted)
fc |> 
  ggplot(aes(x=MEDV, y=pred))+
  geom_point()+
  geom_abline(linetype=2)+
  geom_text(data=slice_max(fc, abs(MEDV-pred), n=3),
            aes(label=name), nudge_y=0.5, nudge_x=-1)+
  labs(x="Observed data",y="Predicted data")+
  coord_fixed()