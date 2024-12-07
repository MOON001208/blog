library(tidyverse)
data(mtcars)
#Q-01 연비가 높은 순서대로 데이터 정렬 후, 연비가 높은 상위 10개 차량의 대한 데이터를 이용.
# 카뷰레터의 수(crab)가 2개인 경우 마력의 평균값과 카뷰레터의 수가 1개인 경우 마력의 평균값의 차이
str(mtcars)
Q1 <- mtcars |> arrange(desc(mpg)) |> slice_head(n=10)
Q1 |> group_by(carb) |> summarise(mean_hp=mean(hp)) |> summarise(result=diff(mean_hp))
#Q-02 자동차 변속기가 수동(am=1)인차량들 중에서 4기통(cyl=4)인 데이터들의 
#mpg 평균값과 hp의 표준편차의 합계

mtcars |> filter(am==1 & cyl==4) |> summarise(mean_mpg=mean(mpg),
                                              sd_hp=sd(hp)) |> summarise(sum(mean_mpg,sd_hp))
#Q-03 am=0에 대한 연비의 이상값을 나타내는 차량의수와 이상값의 평균 출력
#이상값의 평균[이상값 = 연비의평균 +IQR 이상이거나 [연비의평균-IQR이하인값]] IQR 3분위-1분위
outlier <- mtcars |> filter(am==0) |> summarise(upper=mean(mpg)+IQR(mpg),
                                     lower=mean(mpg)-IQR(mpg))
  
mtcars |> filter(am==0 & (mpg<=outlier$lower | mpg>=outlier$upper)) |> summarise(mean(mpg))
mtcars |> filter(am==0 & (mpg<=outlier$lower | mpg>=outlier$upper)) |> nrow()

#Q-04 am=0 자동차 중에서 가장 무거운 상위 10개 데이터 평균 연비를 m1 저장
#am=1인 자동차 중에서 가장 가벼운 상위 10개 데이터 평균 연비 m2로저장
#m2-m1?
m1 <- mtcars |> filter(am==0) |> arrange(desc(wt)) |> slice_head(n=10) |> summarise(mean(mpg))
m2 <- mtcars |> filter(am==1) |> arrange(wt) |> slice_head(n=10) |> summarise(mean(mpg))
print(m2-m1)

#Q-05 (전진)기어의 수가 4개(gear=4)이고 수동 변속기(am=1)인 차량들의 연비와 마력값을 이용하여
#이상값을 가지는 차량들의 평균 연비와 마력을 출력
#이상값 -> r1은 (연비의 평균) + 1.1 * (연비의 표준편차)
# r2는 (마력의 평균) - 1.1 * (마력의 표준편차)
#연비가 r1 이상이거나 또는 마력이 r2이하인 경우 이상값으로 정의
Q5 <- mtcars |> filter(gear==4 & am==1)
outlier <- Q5 |> summarise(r1 = mean(mpg) + 1.1 *sd(mpg),
                        r2 = mean(hp) - 1.1*sd(hp))
Q5 |> filter(mpg>=outlier$r1 | hp<=outlier$r2) |> summarise(mean(mpg),
                                                            mean(hp))

data(iris)
str(iris)
#Q-01 Petal.Lengt의 평균+표준편차, Petal.Width의 평균+표준편차
#setosa Petal.Length값에 대한 min-max scale, z-score변환 평균값
iris |> summarise(length = mean(Petal.Length)+sd(Petal.Length),
                  width = mean(Petal.Width) + sd(Petal.Width))
minmax <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
zscore <- function(x){
  (x-mean(x))/(sd(x))
}

iris |> filter(Species=='setosa') |> summarise(mean(minmax(Petal.Length)),
                                               mean(zscore(Petal.Length)))

#Q-02 Species== versicolor, virginica 이용
#versicolor은 Sepal.Length의 중앙값 이상인 붓꽃 비율
#virginica는 petalLength가 중앙값 이상인 붓꽃비율
#이 두ㅐㄱ 값을 빼라

iris2 <- iris |> filter(Species %in% c('versicolor','virginica'))
summary(iris2)
median_sepal = 6.3
median_petal = 4.9
#versicolor인 붓꽃에 대해 sepal.length값이 중앙값 이상인 붓꽃비율
q1 <- iris2 |> filter(Species=='versicolor' &
                  Sepal.Length>=6.3) |> summarise(n()/sum(iris2$Species=='versicolor'))
q2 <- iris2 |> filter(Species=='virginica' &
                        Petal.Length>=4.9) |> summarise(n()/sum(iris2$Species=='virginica'))
q2-q1

#Q-03 setosa품종 데이터이용, 새로운 항목으로써 꽃받침 길이 (Sepal.Length)가 꽃받침 길이의
#중앙값보다 크면 1, 이하이면 0을 열로 추가, 새로운 항목이 추가된 데이터 세트에 대하여꽃받침길이의
#중앙값보다 큰 붓꽃에 대한 '꽃받침 길이의 평균'출력
iris |> filter(Species=='setosa') |> mutate(value = if_else(Sepal.Length > median(Sepal.Length),1,0)) |> 
  filter(value==1) |> summarise(mean(Sepal.Length))

#Q-04 Petal.Length가 긴 순서대로 정렬 상위 50개 데이터이ㅛㅇ
#Petal.Length의 길이가 긴 순서대로 상위 10개의 Petal.Length길이를 평균값으로 대체
#평균값 대체전 평균 - 평균값 대체 후 평균 출력
Q4 <- iris |> arrange(desc(Petal.Length)) |> slice_head(n=50)
before <- mean(Q4$Petal.Length)
Q4$Petal.Length[1:10] <- mean(Q4$Petal.Length)
after <- mean(Q4$Petal.Length)
before-after
iris |> arrange(desc(Petal.Length)) |> slice_head(n=50) |> 
  mutate(Petal.Length = if_else(row_number()<=10,mean(Petal.Length),Petal.Length)) |> 
  summarise(mean(Petal.Length))
iris |> arrange(desc(Petal.Length)) |> slice_head(n=50) |> 
  summarise(mean(Petal.Length))

#Q-05 setosa, virginica인 100개 데이터
#Sepal.width 길이가 긴 순서대로 정렬 상위 10개 Sepal.width값을 Sepal.width의 중앙값대치
#sepal.width의 이상값을 'sepal.width의 평균값 - IQR(sepal.width)이하값으로 정의'
#IQR Q3 - Q1, 이상값에 포함되지 않은 Sepal.Width의 평균출력
Q5 <- iris |> filter(Species %in% c('setosa', 'virginica'))
Q5 |> arrange(desc(Sepal.Width)) |> mutate(Sepal.Width = 
                                             if_else(row_number()<=10,median(Sepal.Width),Sepal.Width)) |> 
  filter(Sepal.Width > mean(Sepal.Width)-IQR(Sepal.Width)) |> summarise(mean(Sepal.Width))


data("airquality")
str(airquality)
summary(airquality)
psych::describe(airquality)
head(airquality)
#Q-01 8월 26일의 오존량과 태양 복사량(Solar.R)을 출력
#8월데이터에 대해 onzone 값이 8월 26일 이상인 날의 수와 Solar.R값이 8월 26일 하루동안의 태양 복사량 이상인 날의 수의 합계와 
#해당조건을 만족하는 (오존량의 평균, 태양복사량의평균)출력(일수와 평균계산시 결측값 제외)
airquality |> filter(Month == 8 & Day == 26) |> select(Ozone,Solar.R)
airquality |> filter(Month==8) |>  filter(Ozone >= 73) |> summarise(n(),
                                                                    mean(Ozone,na.rm=T))
airquality |> filter(Month==8) |>  filter(Solar.R >= 215) |> summarise(n(),
                                                                       mean(Solar.R,na.rm=T))
print(10+11)

#Q-02 Ozone 항목 결측값 제외한 데이터에 대해서 Solar.R항목의 결측값을 중앙값으로 대체한 후
#중앙값 대체 전 Solar.R 표준편차 - 중앙값 대체 후 Solar.r의 표준편차의 값
airquality |> filter(!is.na(Ozone)) |> nrow()
airquality |> na.omit(airquality$Ozone) |> nrow() #na.omit은 모든 열에 대해서 적용되므로 적절치 않음

sd1 <- airquality |> filter(!is.na(Ozone)) |> 
  mutate(Solar.R = if_else(is.na(Solar.R),median(Solar.R,na.rm=T),Solar.R)) |> summarise(sd(Solar.R))
sd2 <- airquality |> filter(!is.na(Ozone)) |> 
  summarise(sd(Solar.R,na.rm=T))
sd2-sd1

#Q-03 Solar.R을 내림차순 정렬 후 전체 자료들 중 80% 자료(122)개를 저장,
#Ozeon항목의 결측값을 Ozone 항목의 평균값으로 대체한 후 Ozone항목에대하여
#(평균값 대체 전 중앙값) - (평균값 대체 후 중앙값)출력
m1 <- airquality |> arrange(desc(Solar.R)) |> slice_head(prop=0.8) |> 
  mutate(Ozone = if_else(is.na(Ozone),mean(Ozone,na.rm=T),Ozone)) |> summarise(median(Ozone))
m2 <- airquality |> arrange(desc(Solar.R)) |> slice_head(prop=0.8) |> 
  summarise(median(Ozone,na.rm=T))
m2-m1  

#Q-04 결측값이 모두 제거된 데이터를 이용하여 Ozone 항목에 대해 quantile() 함수로 사분위수를 구한다.
#Ozone 항목에 대한 상위 25% 이상의 값과 하위 25% 이하의 값을 모두 0으로 대체하고, 대체된 데이터세트를 이용하여
#ozeon 항목에 대한 평균 + 표준편차의 값을 출력
Q4 <- na.omit(airquality)
quantile(Q4$Ozone)
Q4 |> mutate(Ozone = if_else(Ozone>=quantile(Q4$Ozone,probs=0.75) | Ozone <= quantile(Q4$Ozone,probs = 0.25),0,Ozone)) |> 
  summarise(mean(Ozone) + sd(Ozone))

#Q-05 하루동안의 태양열 복사량(Solar.R)의 결측값을 Solar.R의 중앙값(결측값을 제외하여 구한 중앙값)으로 대체
# 대체된 데이터 세트를 이용하여 ((평균)-1.1*IQR, (평균)+1.1*IQR))범위에 존재하는 Solar.R의 평균을 구하시오
airquality |> mutate(Solar.R = if_else(is.na(Solar.R), median(Solar.R,na.rm=T),Solar.R)) |> filter(
  between(Solar.R, mean(Solar.R)-1.1*IQR(Solar.R), mean(Solar.R)+1.1*IQR(Solar.R))) |> summarise(mean(Solar.R))

data("diamonds")
str(diamonds)
summary(diamonds)
psych::describe(diamonds)

#Q-01 가격기준으로 상위 200개 데이터 에서 cut=Premium인 다이아몬드들의 평균가격
diamonds |> arrange(desc(price)) |> slice_head(n=200) |> filter(cut=='Premium') |> 
  summarise(mean(price)) |> as.data.frame()

#Q-02 다이아몬드 깊이와 너비의 비율이 각각 60% 이상인 데이터를 이용
#데이터들 중 가격이 높은 순서대로 상위 100개의 데이터들 중에서 다이아몬드 가격의 최대-최소값
diamonds |> filter(depth>=60,
                   table>=60) |> arrange(desc(price)) |> slice_head(n=100) |> summarise(max(price)-min(price))

#Q-03 cut=Ideal인 데이터에서ㅓ, 다이아몬드의 x,y,z와 가격 price사이의 상관계수
#결과 파일의 마지막 열(항목)에 상관계수의 최댓값을 구하여 그 결과를 cor.csv로 저장
Q3 <- diamonds |> filter(cut=='Ideal')
c1 <- cor(Q3$x,Q3$price)
c2 <- cor(Q3$y,Q3$price)
c3 <- cor(Q3$z,Q3$price)
results <- data.frame(c1,c2,c3)
results$max <- max(c1,c2,c3)

#Q-04 cut=verygood데이터를 이용, 가격에 대한 이상값의 평균을 출력
#이상값은 median에서 IQR의 1.5배를 초과하는 값
diamonds |> filter(cut=='Very Good') |> filter(price>median(price)+1.5*IQR(price)) |> 
  summarise(mean(price)) |> as.data.frame()

#Q-05 다이아몬드의 caret 1이상이고 cut=Premium인 데이터를 이용
#가격을 높은 순서로 정렬한 뒤 가격 상위 100개의 다이아몬드 자료를 저장(data)
#100개의 행으로 구성된 data에서 다이아몬드의 색상(color)이 (F,G,H)에 대한 비율을 각각 구하고
#최대 비율값을 새로운 열로 추가하여 color.csv 파일로 저장
data <- diamonds |> filter(carat>=1, cut=='Premium') |> arrange(desc(price)) |> slice_head(n=100)
descr::freq(data$color,plot=FALSE)
Fp <- data |> filter(color=='F') |> summarise(F=n()/nrow(data))
Gp <- data |> filter(color=='G') |> summarise(G=n()/nrow(data))
Hp <- data |> filter(color=='H') |> summarise(H=n()/nrow(data))
color <- data.frame(Fp,Gp,Hp)
color$`최대비율값` <- max(color)
color

#p.119
#Q1. Boston
data(Boston,package='MASS')data(Boston)
str(Boston)
#(1)
Boston |> arrange(desc(medv)) |> slice_head(n=20) |> select(medv)

#(2)
Boston |> arrange(desc(medv)) |> mutate(medv = if_else(medv==max(medv), median(Boston$medv),medv)) |> select(medv)
#(3)
Boston |> arrange(desc(medv)) |> mutate(medv = if_else(medv==50, median(Boston$medv),medv)) |> 
  filter(age>=80) |> summarise(mean(medv))

#Q2. presidents
data('presidents')
str(presidents)
data <- matrix(presidents, ncol=4, byrow=TRUE)
data1 <- as.data.frame(data)
colnames(data1) <- c('1분기','2분기','3분기','4분기')
is.na(data1) |> apply(MARGIN = 2,sum)
summary(data1)

#Q3. state.x77
head(state.x77)
str(state.x77)
state <- as.data.frame(state.x77,row.names = FALSE)
psych::describe(state)
scaling <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
state |> mutate(Income = scaling(Income)) |> filter(Income>=0.5) |> nrow()
zscore <- function(x){
  return((x-mean(x,na.rm=T))/sd(x,na.rm=T))
}
state |> mutate(Income = zscore(Income)) |> filter(Income>0) |> nrow()

#Q4 precip
data(precip)
precip
Q4 <- as.data.frame(precip,row.names = names(precip))
Q4 <- rownames_to_column(Q4,var='US city')
RANGE <- IQR(Q4$precip)
#summarise(Q4, IQR(precip))$`IQR(precip)`
Q4 |> filter(precip <= quantile(Q4$precip,0.25)-1.5*RANGE |
               precip>=quantile(Q4$precip,0.75)+1.5*RANGE)

#Q5 housing
house <- read_csv('housing.csv')
str(house)
summary(house)
#(1)
data1 <- house |> mutate(total_bedrooms = if_else(is.na(total_bedrooms),median(total_bedrooms,na.rm=T),total_bedrooms))
summary(data1)
#(2)
data1 |> summarise(m=mean(total_bedrooms),
                   n=sd(total_bedrooms))
#(3)
IQR <- data1 |> summarise(m=mean(total_bedrooms),
                   n=sd(total_bedrooms)) |> summarise(Low = m-n*1.5,
                                                      Upper=m+n*1.5)
data1 |> filter(total_bedrooms<= IQR$Low|
                  total_bedrooms>=IQR$Upper) |> summarise(mean(total_bedrooms)) |> as.data.frame()

#Q-06
train <- read_csv('train_commerce.csv')
str(train)
#(1)
d1 <- train |>  arrange(desc(Customer_care_calls)) |> slice_head(n=500)
d2 <- train |> arrange(desc(Cost_of_the_Product)) |> slice_head(n=500)
d3 <- train |> arrange(desc(Weight_in_gms)) |> slice_head(n=500)

#(2)
r1 <- d1 |> filter(Reached.on.Time_Y.N==1) |> summarise(Care_calls=n()/nrow(d1))
r2 <- d2 |> filter(Reached.on.Time_Y.N==1) |> summarise(Cost_Product=n()/nrow(d2))
r3 <- d3 |> filter(Reached.on.Time_Y.N==1) |> summarise(Weight=n()/nrow(d3))
Ratio <- data.frame(r1,r2,r3)
Ratio$max <- max(Ratio)
Ratio
cor1<- cor(d1$Customer_care_calls,d1$Reached.on.Time_Y.N)
cor2 <- cor(d2$Cost_of_the_Product,d2$Reached.on.Time_Y.N)
cor3 <- cor(d3$Weight_in_gms,d3$Reached.on.Time_Y.N)
corr <- data.frame(cor1,cor2,cor3)
corr$max <- max(corr)
colnames(corr) <- c('Care_calls','Cost_Product','Weight','max')
corr
result <- rbind(Ratio,corr)
rownames(result) <- c('Ratio_onTime','Correlation')
result

#Q7
insurance <- read_csv('insurance.csv')
insurance |> summarise(m=mean(bmi),
                       n=sd(bmi))
insurance |> filter(bmi<=mean(bmi)-1.5*sd(bmi) |
                      bmi>=mean(bmi)+1.5*sd(bmi)) |> 
  summarise(mean(bmi)) |> as.data.frame()

#Q8
country <- read_csv('country.csv')
summary(country)
na_country <- na.omit(country)
na_country |> summarise(quantile(China,prob=0.4))
#Q9
mean <- filter(country,year=='2004') |> select(-year) |>   apply(MARGIN=1,mean)
result <- ifelse(na_country[9,2:8]>mean,1,0)
sum(result)

