data(cars)
m <- lm(dist~speed, cars)
summary(m)
par(mfrow=c(2,2))
plot(m)

## 이분산성 해결
# 1. 변수변환
plot(pressure, main='Pressure')
par(mfrow=c(1,2))

out1 <- lm(temperature ~ pressure, data=pressure);summary(out1)
plot(pressure, main='scale'); abline(out1, col='red')

y <- pressure[-1,]$pressure; logtemp <- log(y)
x <- pressure[-1,]$temperature; logpress <- log(x)

out2 <- lm(logtemp~logpress); summary(out2)
plot(logpress, logtemp, main='log scale'); abline(out2,col='red')

# 2. 가중최소제곱법
x <- 1:5
y <- c(1.1,2.5,3.4,3.8,7)
sd <- c(0.3,0.2,0.2,0.1,0.5)
w <- 1/sd^2
model0 <- lm(y~x)
summary(model0)
model <- lm(y~x, weights = w)
summary(model)
par(mfrow=c(1,1))
plot(y~x, cex=1.3, main='data and model')
lines(x, predict(model), col='red', lwd=2)
lines(x, predict(model0), col='blue', lwd=2)
legend('topleft', c('weighted','Unweighted'), lty=c(1,1),
       lwd=c(2,2), col=c('red','blue'), bty='n')

## 자기상관 해결 -- DW Test
library(car)
Hartnagel <- read.csv('Hartnagel.csv')
fit_h <- lm(fconvict ~ tfr + partic, data=Hartnagel)
dwt(fit_h)

## 다중 공선성
x1 <- rnorm(30)
x2 <- rnorm(30)
x3 <- x1+x2
y <- 0.1*x1 + 0.2*x2 + 0.3*x3 + rnorm(30)
data1 <- data.frame(x1=x1, x2=x2, x3=x3,y=y)
model1 <- lm(y ~ x1+x2+x3, data=data1)
summary(model1)

data2 <- rbind(data1, c(1,2,4,7))      # design mat == full rank
model2 <- lm(y ~ x1+x2+x3,data=data2)
summary(model2)

## 다중공선성 의심
lm3 <- lm(ACHV ~ FAM+PEER+SCHOOL, data=P236)
summary(lm3)

## 다중공선성 탐색법
# 1. 독립 변수들 간 상관계수
pairs(P236)

# 2. VIF
install.packages('regbook')
library(regbook)
data(hald)
hald.lm <- lm(y ~. data=hald)
summary(vif(hald.lm))

# 3. PCA
library(car)
lm3 <- lm(ACHV ~ FAM+PEER+SCHOOL, data=P236)
vif(lm3); mean(vif(lm3))

# 고유값 고유벡터 찾기
install.packages('perturb')
library(perturb)
eigen(cor(P236[,-1]))
colldiag(lm3, center=T, scale=T)

# 다중 공선성 제거
pca <- prcomp(P236[,-1], center=T, scale=T)
pca
lm4 <- lm(P236$ACHV ~ pca$x[, 1:3])
summary(lm4)
lm5 <- lm(P236$ACHV ~ pca$x[,1])
anova(lm5, lm4)
summary(lm5)

## 변수선택법
install.packages('faraway')
install.packages('mixlm')
library(leaps)
library(mixlm)
library(MASS)
library(faraway)

lm <- lm(Y ~ X1+X2+X3+X4+X5+X6, data= P060)

forward(lm, alpha = 0.5, full=T)
forward(lm, alpha = 0.95, full=T)

backward(lm, alpha = 0.1, full=T)
backward(lm, alpha = 0.001, full=T)

stepWise(lm, alpha.enter = 0.15, alpha.remove = 0.15, full=T)
stepWise(lm, alpha.enter = 0.15, alpha.remove = 0.05, full=T)

## AIC를 이용한 변수 선택
base <- lm(Y~1, data=P060)
full <- lm(Y~., data=P060)

step(base, scope=list(upper=full), direction = 'forward')
step(full, direction = 'backward')
step(base, scope=list(upper=full), direction='both')
