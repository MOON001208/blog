library(tidyverse)
train <- read_csv('https://dataq.goorm.io/input-file/content?quizIndex=q_iIhBr_1698648603412&fileName=data%2Fcustomer_train.csv')
test <- read_csv('https://dataq.goorm.io/input-file/content?quizIndex=q_iIhBr_1698648603412&fileName=data%2Fcustomer_test.csv')
test['성별'] <- 0
data <- rbind(train,test)
data <- data |> mutate(성별=as.factor(성별))
train1 <- slice_head(data,n=3500)
test1 <- slice_tail(data,n=2482)
library(tidymodels)
set.seed(123)
split = initial_split(train1,0.7,strata=성별)
train2 = training(split)
valid = testing(split)
logit = logistic_reg() %>% set_engine("glm")
rec = recipe(성별~., data=train1) %>% step_mutate(환불금액 = if_else(is.na(환불금액),0,환불금액)) %>% 
  step_mutate(across(c(주구매상품,주구매지점),as.factor)) |>   step_novel(주구매상품,주구매지점) |>
  step_dummy(주구매상품,주구매지점) |>
  update_role(회원ID,new_role='ID')
rec_prepped <- prep(rec, training = train2)
train_baked <- bake(rec_prepped, new_data = train2)
valid_baked <- bake(rec_prepped, new_data = valid)
sum(is.na(train_baked$환불금액))  # 결과: 0이어야 함
sum(is.na(valid_baked$환불금액))  # 결과: 0이어야 함
sum(is.na(valid$환불금액))
wflow = workflow() %>% add_model(logit) %>% add_recipe(rec)
fit1 <- fit(wflow,train2)
pred <- augment(fit1,valid)
fit1 |> extract_fit_engine() |> coef() |> exp() |> round(6)
pred |> conf_mat(.pred_class, estimate=성별) |> summary(event_level='second')
augment(fit1,test1)

##fit_resamples()
vfold <- vfold_cv(train1, v=5,strata=성별)
option <- control_resamples(save_pred=TRUE,
                            save_workflow=TRUE,
                            event_level = 'second')
metric <- metric_set(accuracy, sens, roc_auc)
cvfit <- fit_resamples(wflow,vfold, control=option, metrics=metric)
cvfit |> collect_metrics()

cvfit2 <- fit_resamples(wflow1,vfold, control=option, metrics=metric)
cvfit2 |> collect_metrics()
cvfit2 |> collect_predictions()

#######################################
library(tidymodels)
library(tidyverse)
train = read_csv("data/customer_train.csv")
test = read_csv("data/customer_test.csv")

# 사용자 코딩
test$성별 = 0
data = rbind(train,test)
#str(data)
#반응변수 factor화, 결측값 처리
data = data %>% mutate(성별 = factor(성별,labels=c('0','1')),
                       across(c(주구매상품,주구매지점),as.factor)) %>% 
                         mutate(환불금액 = if_else(is.na(환불금액),0,환불금액))
#summary(data)
train1 = slice_head(data,n=3500)
test1 = slice_tail(data,n=2482)
test1 = test1 %>% select(-성별)

split = initial_split(train1, 0.7, strata=성별)
train2 = training(split)
valid = testing(split)
logit = logistic_reg() %>% set_engine('glm')
rand = rand_forest() %>% set_engine('ranger') %>% set_mode('classification')
#svm = svm_rbf(cost=tune()) %>% set_mode('classification') %>% set_engine('kernlab')
library(xgboost)
boost = boost_tree() %>% set_engine('xgboost') %>% set_mode('classification')
rec = recipe(성별~.,data=train2) %>% step_novel(주구매상품,주구매지점) %>%step_dummy(주구매상품,주구매지점) %>% update_role(회원ID,new_role='ID')
wflow1 = workflow() %>% add_model(rand) %>% add_recipe(rec)
#fold = vfold_cv(train2,3,strata=성별)
#option = control_resamples(save_pred=TRUE,
#													save_workflow=TRUE,
#												event_level='second')
#D_metric = metric_set(accuracy,sens,roc_auc)
#set.seed(123)
#grid = tune_grid(wflow1, fold, option, grid=5, metrics= D_metric)
#show_best(grid,metric='roc_auc')
#0.00161
#best_cost = select_best(grid,metric='roc_auc')
#final_wflow = finalize_workflow(wflow1, best_cost)
#fit1 = fit(final_wflow, train2)

#fit = fit_resamples(wflow1, fold, metrics=D_metric,control=option)
#collect_metrics(fit)

fit1 = fit(wflow1, train2)
tidy(fit1)
extract_fit_engine(fit1) |> coef() |> exp()
extract_fit_parsnip(fit1) 
pred = augment(fit1, valid)
pred %>% conf_mat(.pred_class,성별) %>% summary(event_level='second')
pred %>% roc_auc(.pred_1, truth=성별,event_level='second')

boost <- boost_tree() |> set_engine('xgboost') |> set_mode('classification')

