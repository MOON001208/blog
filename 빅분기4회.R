data(women)
str(women)
library(tidyverse)
#1유형형
#(1) 몸무게에 대한 1,3사분위수, (1-3 사분위수의)절댓값, 절댓값을 정수로 출력
value <- women |> summarise(one_tile = quantile(weight,0.25),
                               three_tile = quantile(weight,0.75)) |> summarise(abs(one_tile-three_tile))
floor(value) #x보다 작거나 같은 정수중 가장 큰 정숫값
trunc(value) #소숫점 버림
ceiling(value) #x보다 크거나 같은 정수중 가장 큰 정수 값
round(value) #반올림

#(2)views에 대한 긍정적인 반응수(likes)의 비율을 계산한 열을 추가
#category_id=10인 영상들 중 긍정적 반응의 비율이 0.04보다 크고 0.05보다 작은 값을 가지는 영상 개수 출력
data <- read_csv('USvideos.csv')
str(data)
head(data)
summary(data)
data <- data |> mutate(ratio = likes/views)
summary(data)
data |> filter(category_id==10) |>  summarise(between(ratio,0.04,0.05)) |> length() #이러면안됌
#length() => 데이터프레임에서 열의 수 반환
#summarise가 그룹별로 여러행을 반환해서 오류임
#reframe()은 여러 행을 반환할 때 사용합니다.
#summarise()는 그룹당 하나의 요약 결과를 생성할 때 사용합니다.
data |> filter(category_id==10, between(ratio,0.04,0.05)) |> nrow()
data |> filter(category_id==10) |> reframe(between(ratio,0.04,0.05)) |> nrow() #얘도안되넹?

#(3) netflix 콘텐츠 데이터에서 data_added가 2021.7~8월 영상들 중 등록 국가(country)가 United Kingdom인 콘텐츠 수
netflix1 <- read.csv('netflix.csv') #모든 데이터를 chr처리
netflix <- read_csv('netflix.csv') #데이터 특성을 자동으로 파악
str(netflix1)
str(netflix2)
netflix$year <- year(netflix$date_added)
netflix$month <- month(netflix$date_added)
netflix$day <- day(netflix$date_added)
str(netflix)
#방법1
sum(if_else(netflix$country=='United Kingdom' & netflix$year==2021 & (netflix$month==7 | netflix$month==8),1,0),na.rm=T)
#방법2
netflix |> filter(country=='United Kingdom' & year==2021 & (month==7 | month==8)) |> nrow()
#방법3
netflix |> filter(country=='United Kingdom' & between(date_added,as.Date("2021-07-01"),as.Date("2021-08-31")))

netflix1$date_added <- lubridate::ymd(netflix1$date_added)
netflix1 |> filter(country=='United Kingdom' & between(date_added,as.Date("2021-07-01"),as.Date("2021-08-31"))) |> nrow()
#read.csv는 data_added를 변경해주어야함 얘가 chr이여서  between이 안먹음

#2유형
insurance <- read_csv('insurance.csv')
library(psych)
library(tidymodels)
str(insurance)
summary(insurance)
describe(insurance)
head(insurance)
set.seed(1234)
split <- initial_split(insurance, 0.7,strata=sex)
train <- training(split)
test <- testing(split)
?rand_forest
rforest <- rand_forest(mtry=tune(),trees=tune(),min_n=tune()) |> set_engine('randomForest') |> set_mode('classification')
rec1 <- recipe(sex~.,data=train) |> step_dummy(smoker, region)
wflow <- workflow() |> add_model(rforest) |> add_recipe(rec1)
wflow
metric <- metric_set(accuracy, f_meas)
control <- control_resamples(save_workflow = TRUE,
                             save_pred = TRUE,
                             event_level = 'second')
vfold <- vfold_cv(train, strata=sex, 5)
set.seed(12345)
tuning <- tune_grid(wflow, resamples = vfold, grid=20,metrics = metric, control=control)
autoplot(tuning)
param <- select_best(tuning, metric='f_meas')
final <- finalize_workflow(wflow, param)
last <- last_fit(final,split,metrics=metric,control=control)
collect_metrics(last)
collect_predictions(last) |> conf_mat(truth='sex',
                                      estimate=.pred_class) |> summary(event_level='second')
collect_predictions(last) |> select(.pred_class) |> write.csv('1234.csv')
read_csv('1234.csv')

