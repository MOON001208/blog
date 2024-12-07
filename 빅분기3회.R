library(tidymodels)
library(tidyverse)
data <- read_csv('country.csv')
str(data)
summary(data)
#결측치 제거 후 Guam 국가 자료에 대한 상위 70% 사분위 값 구하기
data |> na.omit() |> summarise(quantile(Guam,0.3))
#2000년도 7개 국가들 중 평균값보다 큰 값의 결핵환자 수가 나타난 국가의 수
nadata <- na.omit(data)
nadata
m <- apply(nadata[6,2:8],1,mean)
m
n <- 0
for (i in 2:length(nadata)){
  if (nadata[6,i]>m) n <- n+1 
}
print(n)
#결측치가 가장 많은 국가 출력
result <- data[,2:8] |> lapply(function(x) {sum(is.na(x))})
data |> summarise(across(where(is.numeric),~ sum(is.na(.x)))) #이렇게 하는게 맞냐?


#작업형 제2유형 iris 데이터
data(iris)
str(iris)
summary(iris)
set.seed(123)
iris_split <- initial_split(iris, prop=0.7, strata=Species)
train <- training(iris_split)
test <- testing(iris_split)
decision <- decision_tree(cost_complexity = tune(),
                          tree_depth=tune(),
                          min_n=tune()) |> set_engine('rpart') |> set_mode('classification')
rec1 <- recipe(Species~.,data=train)
wflow1 <- workflow() |> add_model(decision) |> add_recipe(rec1)
set.seed(234)
vfold <- vfold_cv(train, strata=Species)
tree_grid <- grid_regular(cost_complexity(),tree_depth(),min_n(),levels=10)
option <- control_resamples(save_workflow=TRUE,
                            save_pred=TRUE,
                            event_level='virginica')
set.seed(123)
bwt_metric <- metric_set(accuracy, sens, roc_auc)
tree_rs <- tune_grid(
  wflow1,
  resamples = vfold,
  grid=20,
  metrics=bwt_metric,
  control=option
)

tree_rs |> autoplot()
best <- select_best(tree_rs, metric='accuracy')
tree_final <- finalize_workflow(wflow1,best)
tree_fit <- last_fit(tree_final, iris_split,control=option)
tree_fit |> collect_metrics()
#tree_fit |>  pull_workflow_fit() |> rpart.plot::rpart.plot()
#collect_predictions(tree_fit) |> roc_curve(.pred)

collect_metrics(tree_fit)
collect_predictions(tree_fit) |> conf_mat(truth='Species',
                                         estimate=.pred_class)


svm <- svm_linear(cost=tune()) |> set_engine('kernlab',scale=TRUE) |> set_mode('classification')
rec2 <- recipe(Species~., data=train)
wflow2 <- workflow() |> add_model(svm) |> add_recipe(rec2)
svm_grid <- grid_regular(cost(),levels=10)
svm_rs <- tune_grid(wflow2,resamples = vfold,grid=20,metrics=bwt_metric,control=option)
autoplot(svm_rs)
best_param <- select_best(svm_rs,metric='accuracy')
svm_final <- finalize_workflow(wflow2, best_param)
svm_fit <- last_fit(svm_final, iris_split)
svm_fit |> collect_metrics()

collect_predictions(svm_fit) |> conf_mat(truth='Species',
                                         estimate=.pred_class)

collect_predictions(svm_fit) |> roc_curve(truth=Species, 
                                                          .pred_setosa,
                                                          .pred_versicolor,
                                                          .pred_virginica) |> 
  autoplot()
collect_predictions(svm_fit) |> roc_auc(truth=Species, 
                                          .pred_setosa,
                                          .pred_versicolor,
                                          .pred_virginica)


collect_predictions(svm_fit) |> select(Species,.pred_class) |> write.csv('result.csv')
