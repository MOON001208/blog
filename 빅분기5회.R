library(tidyverse)
data <- read_csv('garbagebag.csv',locale=locale(encoding='EUC-KR'))
head(data)
summary(data)
#(1)종량제봉투 처리방식이 소각용이면서 종량제봉투사용대상이 가정용인 2L 종량제 봉투의 평균 가격
str(data)
data |> filter(`종량제봉투처리방식`=='소각용' & `종량제봉투사용대상` == '가정용' & `2L가격` != 0) |> 
  summarise(price = mean(`2L가격`))

#(2) 정상인 사람과 과체중인 사람의 차이(명) 과체중 = BMI 25이상, BMI=W/(t*t)
Body <- read_csv('index.csv')
str(Body)
head(Body)
summary(Body)
Body |> mutate(BMI = Weight/(0.01*Height * 0.01*Height)) |> 
  summarise(pig = sum(BMI>=25),normal= sum(BMI<25)) |> summarise(human=pig-normal)

#(3) 전입학생수합계-전출학생수합계 = 순전입학생수를 구하고, 순전입학생수가 가장 많은 학교의 
#순전입학생수와 전체 학생수를 출력
student <- read_csv('student.csv',locale=locale(encoding = 'EUC-KR'))
str(student)
head(student)
summary(student)
student |> filter(!is.na(`전입학생수합계(명)` & `전출학생수합계(명)` & `전체학생수합계(명)`)) |> 
  group_by(`학교명`) |> 
  reframe(`순전입학생수`= `전입학생수합계(명)` - `전출학생수합계(명)`) |> 
  arrange(desc(`순전입학생수`))

student1 <- student |> filter(!is.na(`전입학생수합계(명)` & `전출학생수합계(명)` & `전체학생수합계(명)`)) |> 
    group_by(`학교명`) |> 
    mutate(`순전입학생수`= `전입학생수합계(명)` - `전출학생수합계(명)`) |> 
  arrange(desc(`순전입학생수`))
student1 |> select(`순전입학생수`, `전체학생수합계(명)`)

#작업형2유형
carprice <- read_csv('carprice.csv')
str(carprice)
head(carprice)
summary(carprice)
psych::describe(carprice)
table(carprice$model)
table(carprice$transmission)
table(carprice$fuelType)
library(tidymodels)
set.seed(1234)
split <- initial_split(carprice, 0.75)
train <- training(split)
test <- testing(split)

reg <- linear_reg() |> set_engine('lm')
rec1 <- recipe(price~year + mileage + tax + mpg + engineSize,data=train)
reg_wflow <- workflow() |> add_model(reg) |> add_recipe(rec1)
reg_fit <- fit(reg_wflow,train)
tidy(reg_fit)
pred <- augment(reg_fit, test)
metrics(pred,truth=price,estimate=.pred)

decision <- decision_tree() |> set_mode('regression') |> set_engine('rpart')
tree_wflow <- workflow() |> add_model(decision) |> add_recipe(rec1)
tree_fit <- last_fit(tree_wflow,split)
collect_metrics(tree_fit)

rand <- rand_forest() |> set_mode('regression') |> set_engine('ranger')
rand_wflow <- workflow() |> add_model(rand) |> add_recipe(rec1)
rand_fit <- last_fit(rand_wflow,split)
collect_metrics(rand_fit)
collect_predictions(rand_fit) |> select(.pred,price) |> rename(predict=.pred,
                                                               actual=price) |> 
  write.csv('빅분기5회.csv',row.names=FALSE)
read_csv('빅분기5회.csv')
