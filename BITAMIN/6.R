## 0508 예습과제 정해성
numsold <- c(0,1,2,3,4,5)
prob <- c(0.6, 0.15, 0.1, 0.08, 0.05, 0.02)
saturday_sales <- data.frame(numsold, prob)

mu <- sum(numsold*prob)
variance <- sum((numsold-mu)^2 * prob)

# 이항분포 예시 - The number of heads in five tosses of a fair coin
x <- c(0,1,2,3,4,5)
cbind(dbinom(x, size=5, prob=0.5))

# 임신을 했을 때 몇 쌍의 부부가 아들 또는 딸을 가질 수 있을까?
# 가구 수: 10, 아들일 확률 P(male)=0.76, P(female)=0.24
# male을 성공이라고 생각
dbinom(8, size=10, prob=0.76)  # P(X=8)각 즉, 8가구가 아들일 확률
dbinom(10, size=10, prob=0.76) # P(X=10)

xvec <- 0:6
dbinom(xvec, 10, 0.76)
sum(dbinom(xvec, 10, 0.76))
pbinom(6, 10, 0.76)             # p+분포 == cdf

# r+분포 == random sampling
# 예) 가족 수 10 중에서 p(male)=0.76인 데이터 100개 생성
rbinom(100, 10, 0.76)

# 이항분포의 확률변수 X: 독립인 베르누이 시행 시 성공의 횟수
# 포아송분포의 확률변수 X: 단위 당(시간,공간) 성공 횟수
# 포아송의 조건: 1)독립성 2) 비례성 3) 비군집성

# 예) 30분마다 지나가는 자동차의 수가 평균 25대일 때,
#     20대보다 적은 자동차가 지나갈 확률은?
#     주어진 단위시간: 30분 // X: 30분마다 지나가는 자동차 수
#     E(X) = lambda = 25 // 구할 것: P(X<20)
xvec <- seq(0,20,1)
sum(dpois(xvec, 25))   # 람다=25인 포아송 분포에서 X가 0~20까지의 확률의 합
dpois(25, 25)          # P(X=25)

## 이항분포와 포아송분포의 정규분포와의 관계
# 이항분포의 시행 횟수 n이 커질수록 정규분포 근사
# 포아송분포의 평균(E(X)=lambda)이 커질수록 정규분포 근사
x <- seq(0,10)
prob <- dbinom(x, 10, 0.5)   # 10번 시행, p=0.5인 이항분
plot(x, prob, type='o', main='Binomial Approximates Normal')

x <- seq(0,15)
prob <- dpois(x, 6)         # lambda=6인 포아송의 0<=x<=15 확률
plot(x, prob, type='o', main='Poisson Approximates Normal')

# ex)집에 초고속 인터넷이 있냐고 물어봤을 때 응답자가 “Yes”라고 답할 확률?
#전수조사 결과, 인터넷이 설치가 되어있을 확률은 0.6378 
# 1500가구를 무작위로 뽑았을 때 적어도 1000가구가 “Yes”라고 응답할 확률은?

1 - pbinom(999, 1500, 0.6378)   # 1 - (P(X<=999))

## 정규확률 계산
# 표본 평균의 분포는 표본 수가 커지면 정규분포 근사 -- CLT

# 평균이 20이고 표준편차가 각각 3,6인 두 정규분포의 비교
xaxis <- seq(0,40,0.5)
y1 <- dnorm(xaxis, 20, 6)
y2 <- dnorm(xaxis, 20, 3)
plot(xaxis, y2, type='l', main='Comparing two Nirmal Dist.')
points(xaxis, y1, type='l', color='red')

# pnorm()으로 확률 찾기 --> p+분포 ==> cdf
pnorm(1.96)   # Z=1.96까지의 누적확률

pnorm(1.96) - pnorm(-1.96)    # P(-1.96 < Z < 1.96)

1 - pnorm(1.96)               # P(Z>1.96)

# scale() --> 스케일링 함수
# X ~ N(500, 100) 일 때, P(X<=720)의 구하기
# 표준화 후 표준정규분포를 이용함.
scale(720, 500, 100)    # {데이터(720)-평균(500)} / 표준편차(100)
pnorm(2.2)


# qnorm()으로 확률에 대응하는 Z값 찾기 --> 임계값 찾을 때
# q+분포(누적확률) --> 확률변수값 반환
qnorm(0.975)           # cdf=0.975인 Z값 반환 -> P(Z<1.96)=0.975

# 유의수준 alpha = 0.05 일때, 표준정규분포의 임계값 찾기
qnorm(1 - 0.05)

# rnorm()활용해 n=50, mu=100, sigma=15인 샘플 만들기
samples <- rnorm(50, 100, 15)
hist(samples)

# rnorm()활용해 n=1000, mu=100, sigma=15인 샘플 만들기
samples2 <- rnorm(1000, 100, 15)
hist(samples2)

## 표본분포 --> 모집단이 정규분포를 따를 경우 정규분포
#           --> n이 클 경우 표본평균은 정규분포에 근사 (by CLT)
# bar(X) ~ N(mu, sigma / sqrt(n))

# weights 는 20~74세 남성 40명의 몸무게
# 같은 연령대 남성 평균 몸무게 = 191
# H0: u1 >= u2       vs   H1: u1 < u2 
weights <- read.csv('weights.csv', stringsAsFactors = F)
weights

ztest <- function(xbar, mu, stdev, n){
  z = (mean(xbar) - mu) / (stdev / sqrt(n))
  return(z)
}

z <- ztest(mean(weights$values), 191, sd(weights$values), 40)
pnorm(z)        # 좌측검정 & pvalue = 4.663168e-06
