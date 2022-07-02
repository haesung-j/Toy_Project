# 적합도 검정
attach(mtcars)
str(mtcars)
glm.vs <- glm(vs~mpg+am,family=binomial)
summary(glm.vs)
1-pchisq(43.860-20.646,2)

# 위험도 평가
prob <- glm.vs$fitted.values
prob <- ifelse(prob>=0.5, 1, 0)
as.integer(prob)

mtcars$risk <- prob
head(mtcars)
xtabs(~vs+risk, data=mtcars)
chisq.test(xtabs(~vs+risk,data=mtcars))

# AIC
library(rsq)
data(hcrabs)
head(hcrabs)
table(hcrabs$color)
y <- hcrabs$num.satellites; y[hcrabs$num.satellites>0]<-1
c <- as.factor(as.numeric(hcrabs$color)-1)
s <- as.factor(hcrabs$spine)
c <- relevel(c, ref='3'); s <- relevel(s,ref='3')
width <- hcrabs$width
weight <- hcrabs$weight
crab.main <- glm(y~c+s+weight+width, family=binomial)
summary(crab.main)

crab.sub <- glm(y ~c, family = binomial)
summary(crab.sub)

# ROC curve
data(hcrabs)
colnames(hcrabs)[4] <- 'target'  # target변수 = num.satellites
hcrabs$target[hcrabs $ target>0] <- 1 # 이항변수로 변환
train_index <- sample(nrow(hcrabs))[1:(nrow(hcrabs)*.7)]
train <- hcrabs[train_index,]
test <- hcrabs[-train_index,]

crab.main <- glm(data= train, target~., family = binomial)
table(test$target, predict(crab.main,newdata=test)>0)

install.packages('ROCR')
library(ROCR)
prob <- predict(crab.main, newdata=test, type='response')
pred <- prediction(prob, test$target)
roc <- performance(pred, measure='tpr', x.measure = 'fpr')
plot(roc, col='red', main='ROC of Test Data')
str(roc)


## 적합도 검정
#1. 기대빈도가 동일한 경우
obs <- c(12,8,10,16,7,7)
chisq.test(obs)
exp <- rep(1/6,6)
chisq.test(obs, p=exp)

#2. 기대빈도가 동일하지 않은 경우
cars <- MASS::Cars93
car_type <- table(cars$Type)
car_type
car_type_prob <- c(0.2,0.1,0.2,0.2,0.2,0.1)
chisq.test(car_type, p=car_type_prob)


# 3. 모집단 분포의 모수가 알려져 있지 않은 경우
classnum <- 0:9
obsnum <- c(6,20,36,50,52,40,25,12,4,5)
obssum <- sum(obsnum)
lambda <- sum(classnum*obsnum) / sum(obsnum)
lambda
hyp_prob <- round(dpois(classnum,lambda),3)
hyp_prob
sum(hyp_prob)
hyp_prob[10]<- 1- sum(hyp_prob[1:9])
sum(hyp_prob)

chisq.gof.test <- chisq.test(obsnum, p=hyp_prob)
chisq.gof.test

chisq.gof.test$observed
chisq.gof.test$expected

obsnum1 <- obsnum[1:9]
obsnum1[9] <- sum(obsnum[9:10])
obsnum1
hyp_prob1 <- hyp_prob[1:9]
hyp_prob1[9] <- sum(hyp_prob[9:10])
hyp_prob1

chisq.test(obsnum1, p = hyp_prob1)

# 각 class별 관측도수와 기대도수 시각화
chisq.gof.test1 <- chisq.test(obsnum1, p=hyp_prob1)
plot(0:8, chisq.gof.test1$observed, main='Poisson Dist:Observed vs Expected', type='b', pch=0, col='blue', xlab = 'Number of Events', ylab='Frequency',ylim =c(0,55))   # 관측도수 시각화 
par(new=T)
plot(0:8, chisq.gof.test1$expected, type='b',pch=1, col='red',
     xlab='',ylab='',ylim=c(0,55))


## 독립성 검정
install.packages('moonBook')
acs <- moonBook::acs
head(acs)

# 분할표 만들기
table(acs$sex)
acs_t <- table(acs$sex, acs$obesity)
acs_t

acs_xt <- xtabs(~sex+obesity, data= acs)
acs_xt
install.packages('descr')
library(descr)
acs_ct <- CrossTable(acs$sex, acs$obesity)
acs_ct

acs_ct_np <- CrossTable(acs$sex, acs$obesity, prop.r=F, prop.c=F)
acs_ct_np

table(acs$smoking, acs$Dx)
xtabs(~smoking+Dx, data=acs)
CrossTable(acs$smoking,acs$Dx, prop.r=F, prop.c=F,prop.t = F)

chisq.test(table(acs$smoking, acs$Dx))
summary(xtabs(~smoking+Dx, data=acs))
CrossTable(acs$smoking,acs$Dx, prop.r=F, prop.c=F,prop.t = F, chisq=T)

# 야트보정
acs_t <- table(acs$sex, acs$obesity)
chisq.test(acs_t)
acs_ct_chi <- CrossTable(acs$sex,acs$obesity, chisq=T)
acs_ct_chi

# Fisher's exact Test
head(mtcars)
cars <- table(mtcars$carb, mtcars$cyl, dnn = c('carb','cyl'))
cars
chisq.test(cars)
chi_cars <- chisq.test(cars)
chi_cars$expected
fisher.test(cars)


# 연습
factory_default <- matrix(c(62,40,37,61,35,72,37,76,28,71,57,24), nrow=3, byrow=T)
rownames(factory_default) <- c('l','ll','lll')
colnames(factory_default) <- c('A','B','C','D')
factory_default
addmargins(factory_default)
factory_default_prop <- prop.table(factory_default,margin=2)
factory_default_prop
barplot(factory_default_prop, legend=T)
chi_factory <- chisq.test(factory_default)
chi_factory

# 연습
sex_credit <- matrix(c(26,23,101,28,72,70),nrow=2, byrow=T)
rownames(sex_credit) <- c('F','M')
colnames(sex_credit) <- c('bad','excellent','fair')
sex_credit
p <- prop.table(sex_credit)
p
barplot(p, beside=T, legend=T, ylim=c(0,0.35))
chi_sex_credit <- chisq.test(sex_credit)
chi_sex_credit

female <- c(18,102)
male <- c(10,110)
migraine <- cbind(female,male)
migraine
chisq.test(migraine, correct =F)

count <- c(18,10)
total <- c(120,120)
prop.test(count,total, correct = F)

p1 <- 0.15
p2 <- 0.08333
q1 <- 1 - p1
q2 <- 1 - p2
var1 <- p1 * q1/120
var2 <- p2 * q2/120
se <- sqrt(var1 + var2)
z <- (p1-p2)/se
z
z^2
