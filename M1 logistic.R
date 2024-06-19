library(tidyverse)
library(GGally)
library(car)
library(pROC)
library(bestglm)
library(caret)
library(smotefamily)
library(readxl)
library(glmnet)
M1 <- read_csv("M1_data.csv")
str(M1)
M1 <- M1 |> select(-status,-domain)
M1 <- M1 |> mutate(across(where(is.character),as.factor))
M1 <- M1 |> rename(trust=trust_apple,
                   in_com=interest_computers,
                   age_com=age_computer,
                   apply_count=appleproducts_count,
                   fam=familiarity_m1,
                   bettery=f_batterylife,
                   price=f_price,
                   size=f_size,
                   multi=f_multitasking,
                   noise=f_noise,
                   perfor=f_performance,
                   synergy=f_synergy,
                   loss_perfor=f_performanceloss,
                   consider=m1_consideration,
                   purchase=m1_purchase,
                   age=age_group,
                   income=income_group)
M1 |> 
  ggplot(aes(x=price, y=as.numeric(purchase)-1))+
  geom_jitter(height=0.0005, width=0.1)+
  geom_smooth(method="glm", method.args=list(family=binomial),
              se=F)
str(M1)
table(M1$user_pcmac)
ggpairs(M1,aes(color=purchase),cardinality_threshold = 22)
M1 |> select(where(is.numeric)) |> ggcorr(label=T,label_round=2)
ggplot(M1)+
  geom_bar(aes(x=user_pcmac))
table(M1$gender[M1$purchase=="Yes"])
M1 <- M1 |> filter(!user_pcmac %in% c("Hp","Other")) |> 
  mutate(user_pcmac=factor(user_pcmac,label=c("Apple","PC")))
set.seed(1234)
x.id <- createDataPartition(M1$purchase,p=0.7,list=F)[,1]
train <- M1 |> slice(x.id)
test <- M1 |> slice(-x.id)
M1 |> count(purchase) |> mutate(p=n/sum(n))
train |> count(purchase) |> mutate(p=n/sum(n))
test |> count(purchase) |> mutate(p=n/sum(n))
Xy <- train |> relocate(purchase, .after=last_col())
fit1 <- bestglm(as.data.frame(Xy), family=binomial)$BestModel
fit2 <- bestglm(as.data.frame(train), family=binomial,IC="AIC")$BestModel
x_la <- model.matrix(purchase~., train)[,-1]
cv_fit <- cv.glmnet(x_la, train$purchase, family="binomial")
cv_fit
plot(cv_fit)
coef(cv_fit)
fit_full <- glm(purchase~.,train,family=binomial)
fit_null <- glm(purchase~1,train,family=binomial)
back_AIC <- MASS::stepAIC(fit_full, direction="both",trace=F)
for_AIC <- MASS::stepAIC(fit_null,scope=list(lower=fit_null, upper=fit_full),trace=F)
back_bic <- MASS::stepAIC(fit_full, direction="both",trace=F,k=log(nrow(train)))
for_bic <- MASS::stepAIC(fit_null,scope=list(lower=fit_null, upper=fit_full),trace=F, k=log(nrow(train)))
names(back_AIC$model)[-1] |> sort()
names(for_AIC$model)[-1] |> sort()
names(back_bic$model)[-1] |> sort()
names(for_bic$model)[-1] |> sort()
fit1 <- back_AIC
summary(fit1)
fit1_1 <- update(fit1,.~.-multi)
summary(fit1_1)
fit1_2 <- update(fit1_1,.~.-trust-perfor)
summary(fit1_2)
fit1_3 <- update(fit1_1,.~.-perfor)
summary(fit1_3)
fit2 <- back_bic
summary(fit2)
residualPlots(fit1)
residualPlots(fit1_1)
residualPlots(fit1_2)
residualPlots(fit1_3)
residualPlots(fit2)
AIC(fit1,fit1_1,fit1_2,fit1_3,fit2)
BIC(fit1,fit1_1,fit1_2,fit1_3,fit2)
vif(fit1)
vif(fit1_1)
vif(fit1_2)
vif(fit1_3)
vif(fit2)
pred1 <- predict(fit1,type="response")
pred1_1 <- predict(fit1_1,type="response")
pred1_2 <- predict(fit1_2,type="response")
pred1_3 <- predict(fit1_3,type="response")
pred2 <- predict(fit2,type="response")
confusionMatrix(data=factor((fit1_1$fitted>=0.5)*1),  #yes는 1 no는 0으로 되서 수준이 안맞아 결과가 안나옴
                ref=train$purchase, positive="Yes",mode="everything")
train |> mutate(purchase_hat=
                  factor(if_else(pred1>=0.5,"Yes","No"))) |>
  with(confusionMatrix(data=purchase_hat,ref=purchase,
                       positive="Yes",mode="everything"))

table1_1 <- train |> mutate(purchase_hat=
                              factor(if_else(pred1_1>=0.5,"Yes","No"))) |>
  with(confusionMatrix(data=purchase_hat,ref=purchase,
                       positive="Yes",mode="everything"))
train |> mutate(purchase_hat=
                  factor(if_else(pred1_2>=0.5,"Yes","No"))) |>
  with(confusionMatrix(data=purchase_hat,ref=purchase,
                       positive="Yes",mode="everything"))
train |> mutate(purchase_hat=
                  factor(if_else(pred1_3>=0.5,"Yes","No"))) |>
  with(confusionMatrix(data=purchase_hat,ref=purchase,
                       positive="Yes",mode="everything"))
train |> mutate(purchase_hat=
                  factor(if_else(pred2>=0.5,"Yes","No"))) |> 
  with(confusionMatrix(data=purchase_hat,ref=purchase,
                       positive="Yes",mode="everything"))
auc(roc(train$purchase, fit1$fitted)) 
auc(roc(train$purchase, fit1_1$fitted)) 
auc(roc(train$purchase, fit1_2$fitted))
auc(roc(train$purchase, fit1_3$fitted,plot=T))
auc(roc(train$purchase, fit2$fitted,plot=T))
str(M1)

fc <- predict(fit1_1,newdata=test,type="response")
test |> 
  mutate(y_hat=
           factor(if_else(fc>=0.5,"Yes","No"))) |>
  with(confusionMatrix(data=y_hat, reference=purchase,
                       positive="Yes",mode="everything"))
roc(test$purchase,fc,plot=T,quiet=T) |> auc()
roc(test$purchase,fc,plot=T)
table1_1$byClass[c(1,2,5,7)]
table1_1$overall[1]
coef(fit1_1) |> exp() |> round(6)