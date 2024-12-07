library(tidyverse)
train <- read_csv('C:/Users/xoosl/Desktop/전공/blog/big-data-analytics-certification-kr-2024-3/train.csv')
test <- read_csv('C:/Users/xoosl/Desktop/전공/blog/big-data-analytics-certification-kr-2024-3/test.csv')

str(train)
str(test)
summary(train)
colSums(is.na(train))
colSums(is.na(test))
#Alley,QoolQc,MiscFeature,Fence,MasVnrType제거, FireplaceQu 변환?
test$SalePrice <- 0
data <- rbind(train,test)

data1 <- data |> select(-c(Alley,PoolQC,MiscFeature,Fence,MasVnrType,FireplaceQu))
data2 <- data1 |> mutate(across(where(is.character),as.factor))
psych::describe(data1)
train1 <- slice_head(data2,n=1168)
test1 <- slice_tail(data2,n=292)
str(train1)
library(tidymodels)
model <- rand_forest() |> set_engine('ranger') |> set_mode('regression')

rec1 <- recipe(SalePrice~.,data=train1) |> update_role(Id,new_role='ID') |> 
  step_impute_mode(all_nominal_predictors()) |> 
  step_impute_mean(all_numeric_predictors()) |> 
  step_scale(all_numeric_predictors())
set.seed(1234)
vfold <- vfold_cv(train1,5)
wflow <- workflow() |> add_model(model) |> add_recipe(rec1)
fit1 <- fit_resamples(wflow,resamples=vfold,control=control_resamples(
  save_pred=TRUE,save_workflow=TRUE
))
collect_metrics(fit1)
collect_predictions(fit1)
show_best(fit1,metric='rmse')
best <- select_best(fit1,metric='rmse')
final_wflow <- finalize_workflow(wflow,best)
last_fit <- fit(final_wflow,train1)
pred <- augment(last_fit,test1)
pred


train <- read_csv('C:\\Users\\xoosl\\Desktop\\전공\\blog\\2024-4-big-data-analytics-certification-kr\\train.csv')
test <- read_csv('C:\\Users\\xoosl\\Desktop\\전공\\blog\\2024-4-big-data-analytics-certification-kr\\test.csv')
str(train)
str(test)
colSums(is.na(test))
psych::describe(train)
table(train$Sex)
table(test$Sex)
summary(train)

set.seed(123)
split <- initial_split(train,prop=0.7)
train1 <- training(split)
valid <- testing(split)
rand <- rand_forest(mtry=3,min_n=tune()) |> set_engine('ranger') |> set_mode('regression')
rec <- recipe(Age~.,data=train1) |> update_role(id,new_role='ID') |> 
  step_normalize(all_numeric_predictors()) |> step_mutate(Sex=as.factor(Sex)) |> step_dummy(Sex)
wflow <- workflow() |> add_model(rand) |> add_recipe(rec)
vfold <- vfold_cv(train1,v=5)
option <- control_resamples(save_pred=TRUE,save_workflow=TRUE)
tune <- tune_grid(wflow,vfold,grid=5,control=option)
show_best(tune)
autoplot(tune)
best <- select_best(tune,metric='rmse')
final_wflow <- finalize_workflow(wflow,best)
last <- last_fit(final_wflow,split)
collect_metrics(last)
fit1 <- fit(final_wflow,train1)
pred1 <- augment(fit1,train1)
metrics(pred1,.pred,Age)
pred <- augment(fit1,valid)
metrics(pred,.pred,Age)
test_pred <- augment(fit1,test)
sample <- read_csv('C:\\Users\\xoosl\\Desktop\\전공\\blog\\2024-4-big-data-analytics-certification-kr\\sample_submission.csv')
test_bind <- cbind(test_pred,sample[,2]) |> as.data.frame()
metrics(test_bind,.pred,Age)
