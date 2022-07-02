## 모평균의 신뢰구간
# weights 데이터의 모분산이 100이라고 가정하고 CI구하기
weights <- read.csv('weights.csv', header=T, stringsAsFactors = F)
weights
weights <- as.vector(weights$values)

z.mean <- function(x, sigma, alpha=0.05){
  cat('표본평균은', mean(x),'\n')
  zcrit <- qnorm(1 - alpha/2)  # z분포의 (1-alpha)/2 분위수 생성
  stderror <- sigma/sqrt(length(x))  # 표준오차
  margin <- stderror * zcrit    # 오차한계
  LL <- mean(x) - margin             # CI 상한
  UL <- mean(x) + margin             # CI 하한
  cat('신뢰수준', 1-alpha,'%하에서','\n')
  cat('신뢰구간은', LL,',',UL,'이다','\n')
}

z.mean(weights, 10)

# 오차한계 줄이려면? 필요한 표본크기 계산  
sampsize.est <- function(E, sigma, alpha=.05){
  # E는 오차한계(margin of error)
  n <- ((qnorm(alpha/2)*sigma)/E)^2
  estsize <- ceiling(n)  # n보다 크거나 같은 정수
  cat('오차한계',E,'에 필요한 표본 크기는',estsize,'이다')
}

sampsize.est(5, sd(weights))

# 모분산을 모를 때 t분포로 CI구하기
t.mean <- function(x, alpha=.05){
  cat('표본평균은', mean(x),'\n')
  df <- length(x) - 1
  tcrit <- qt(1-alpha/2, df) # 자유도 df인 t분포의 1-알파/2 분위수 생성
  stderror <- sd(x) / sqrt(length(x))  # 표준오차
  margin <- stderror * tcrit           # 오차한계
  LL <- mean(x) - margin               # 신뢰하한
  UL <- mean(x) + margin               # 신뢰상한
  cat('신뢰수준', 1-alpha,'%하에서','\n')
  cat('신뢰구간은', LL,',',UL,'이다','\n')
}

t.mean(weights)
t.test(weights)

## 모비율의 신뢰구간
confi.prop <- function(phat, n, alpha=0.05){
  zcrit <- qnorm(1-alpha/2)
  margin <- zcrit * sqrt((phat*(1 - phat))/n)
  upper <- phat + margin
  lower <- phat - margin
  cat('표본비율은',phat,'\n')
  cat('표본오차는',margin,'%포인트','\n')
  cat('신뢰수준',1-alpha,'%하에서','\n')
  cat("신뢰구간은 (", lower, ',', upper, ") 이다.", '\n')
}

confi.prop(0.609, 17000)

## 모분산의 신뢰구간 --> 카이제곱분포
# 카이제곱분포 이해하기
x <- seq(0,50)
y1 <- dchisq(x,9)         # 자유도 9인 카이스퀘어의 각 확률
y2 <- dchisq(x,19)        # 자유도 19인 카이스퀘어의 각 확률

plot(y1, type='l', main='Chi-Square Distribution',
     xlab='Chi-Square', ylab='dchisq')
points(x, y2, type='l', col='red')
y3 <- dchisq(x,14)         # 자유도 14인 카이스퀘어의 각 확률
y4 <- dchisq(x,49)        # 자유도 49인 카이스퀘어의 각 확률
points(x, y3, type='l', col='blue')
points(x, y4, type='l', col='magenta')

x <- seq(0,100)
y1 <- dchisq(x,9) ; y2 <- dchisq(x,19)  
y3 <- dchisq(x,14); y4 <- dchisq(x,49) 
plot(y1, type='l', main='Chi-Square Distribution',
     xlab='Chi-Square')

# 카이제곱분포의 확률밀도함수는 비대칭
qchisq(0.025, 39)
qchisq(0.975, 39)

# 카이제곱분포로 모분산의 CI 구하는 함수 작성
confi.var <- function(x, n, alpha=0.05){
  chisqL <- qchisq(alpha/2, n-1) 
  chisqR <- qchisq(1 - alpha/2, n-1)
  sampvar <- var(x)
  lower <- (n-1)*sampvar/ chisqR
  upper <- (n-1)*sampvar/ chisqL
  cat('표본분산은',sampvar,'\n')
  cat('신뢰수준', 1-alpha, '%하에서','\n')
  cat('모분산에 대한 신뢰구간은','\n')
  cat('(', lower, ',', upper, ')이다', '\n','\n')
  cat('신뢰수준', 1-alpha, '%하에서','\n')
  cat('표본표준편차에 대한 신뢰구간은','\n')
  cat('(', sqrt(lower), ',', sqrt(upper), ')이다', '\n')

}

confi.var(weights, length(weights))


## 평균 간 차이에 대한 신뢰구간
attach(dataset)
t.test(Age ~ Sex)

install.packages('stats')
library(stats)

require(stats)
attach(dataset)
factor(Sex)
model <- lm(Age ~ Sex)
confint(model)

## t 검정
# t분포
x <- seq(-3.5, 3.5, .1)
y1 <- dnorm(x)
y2 <- dt(x,4)
y3 <- dt(x,9)
y4 <- dt(x,19)
plot(x, y1, type='l', main='Comparing z and t distributions')
points(x, y2, type='l', col='red')
points(x, y3, type='l', col='blue')
points(x, y4, type='l', col='purple')

# 단일표본 t-검정 
set.seed(6)
sp1 <- rnorm(50, 500, 100)
summary(sp1)

t.test(x=sp1, mu=500)
t.test(x=sp1, mu=500, alternative='two.sided')  # 디폴트
t.test(x=sp1, mu=500, alternative='less')       # 좌측검정
t.test(x=sp1, mu=500, alternative='greater')       # 우측검정

# 단일표본 예제 미국의 강 평균 길이가 500마일 이상인가
data(rivers)
summary(rivers)

t.test(rivers, mu=500, alternative = 'greater')

# 대응표본 t-검정 --> 한 집단 내의 변화
load('prepost.rda')
prepost
attach(prepost)
Differences <- Posttest - Pretest
Differences

t.test(Differences, mu=0)       # 단일표본으로 해보기  복습

t.test(x=Pretest, y=Posttest, paired=T)
data(sleep)
str(sleep)
attach(sleep)
t.test(extra ~ group, paired=T, conf.level=0.99)


# 독립표본 t-검정
load('stackeddata.rda')
str(stackeddata)

attach(stackeddata)
var.test(PostTest ~ Class)    # 등분산검정 --> Ho:등분산이다 기각

# 이분산일시 default인  Welch t검정 적용
t.test(PostTest ~ Class)
data(iris)
iris
library(dplyr)
sp2 <- iris %>%
  filter(Species %in% c('setosa','virginica'))
attach(sp2)
var.test(Sepal.Length ~ Species)      # H0기각 --> 이분산
t.test(Sepal.Length ~ Species)
