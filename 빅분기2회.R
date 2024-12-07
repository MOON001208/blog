library(tidymodels)
library(tidyverse)
library(psych) #describe
library(Hmisc) #rcorr
data(Boston, package='MASS')
Boston
describe(Boston)
rcorr(as.matrix(Boston),type='pearson')
str(Boston)
summary(Boston)
cor(Boston$crim, Boston$chas)
GGally::ggcorr(Boston,label=T,digits=3)
#####2회 1번
#(1) 범죄율 항목에 대해 범죄율이 가장 높은 10개 지역의 범죄율 출력
data <- arrange(Boston, desc(crim))
data[1:10,] |> select(crim)
#(2) 범죄율이 가장 높은 1위 지역부터 10위 지역의 범죄율을 10번째로 높은 지역의 범죄율 값으로 모두
#대체하고 그 결과 출력
data$crim[1:10] <- data$crim[10]
#(3) 2의 데이터를 이용하여 1940년 이전 주택의 비율 항목(age)이 80% 이상인 지역에 대한 평균
#범죄율을 출력하시오
data |> filter(age>=80) |> summarise(mean(crim))

#####2회 2번 
house <- read_csv('housing.csv')
#주어진 데이터의 첫번째 행부터 89%행까지 자료(16512개)를 data1에 저장
data1 <- slice_head(house,n=16512)
#data1에서 total_bedrooms항목의 결측값 제외하여 total_bedrooms 항목의 표준편차 출력
data1 |> summarize(sd(total_bedrooms,na.rm=T)) |> as.data.frame()
#결측값 대치 후 표준편차 출력
data1 |> mutate(total_bedrooms = if_else(is.na(total_bedrooms), median(total_bedrooms,na.rm=TRUE),total_bedrooms)) |>
  summarise(sd(total_bedrooms)) |> as.data.frame()

#####2회 3번
insurance <- read_csv('insurance.csv')
str(insurance)
summary(insurance)
#(1) charge 항목에 대한 평균 표준편차
insurance |> summarise(m = mean(charges),
                       n= sd(charges)) |> as.data.frame()
#(2) 이상값
insurance |> mutate(result = charges>= mean(charges)+1.5*sd(charges)) |>
  summarise(sum(insurance$charges[result]))

#####2회 작업형 2유형
Train <- read_csv('Train.csv')
str(Train)
train <- Train |> select(-ID) |> mutate(Reached.on.Time_Y.N=as.factor(Reached.on.Time_Y.N))
str(train)
summary(train)
train_split <- initial_split(train,prop=0.7,strata=Reached.on.Time_Y.N)
train_data <- training(train_split)
test_data <- testing(train_split)

svm <- svm_rbf(mode='classification',cost=10,rbf_sigma=0.1) |> set_engine('kernlab')
rec1 <- recipe(Reached.on.Time_Y.N~.,data=train_data) |> step_scale(all_numeric()) |> 
  step_dummy(all_nominal_predictors())
wflow <- workflow() |> add_model(svm) |> add_recipe(rec1)
wflow
fit1 <- fit(wflow,train_data)
augment(fit1,test_data)
fit1
option_last <- control_last_fit(event_level='second')
fit <- last_fit(wflow, train_split,
                control=option_last)
collect_metrics(fit)
accuracy <- collect_metrics(fit)[,".estimate"] |> slice_head(n=1)
error <- 1-accuracy

collect_predictions(fit) |> conf_mat(truth=Reached.on.Time_Y.N,
                                     estimate=.pred_class) |> summary(event_level='second')
roc_curve(collect_predictions(fit), ends_with("_1"), 
          truth = Reached.on.Time_Y.N, event_level= "second") |> 
  autoplot()

new <- as.data.frame(collect_predictions(fit)[,c(1,6)])

