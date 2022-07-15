#####################################################################
## econo.class : 4 levels showing the economic class
## (1 if 1st class, 2 if 2nd class, 3 if 3rd class, 4th if crew), 
## age : 0 if child, 1 if adult
## sex : 0 if female, 1 if male
## survived: 0 if not survived, 1 if survived
## Goal: women and children first? or other?
#####################################################################
attach(titanic.data)
library(ggplot2)
library(dplyr)

ftable(survived ~ sex+age+econo.class,data=titanic.data)
########### 1. EDA #################

## 전체 생존자와 사망자 빈도
table(survived)
ggplot(data=titanic.data, aes(x=survived)) + geom_bar()


## 각 범주별 생존률 그래프
df_eco <- titanic.data %>%
  group_by(econo.class) %>%
  summarise(mean_survived = mean(survived))
df_age <- titanic.data %>%
  group_by(age) %>%
  summarise(mean_survived = mean(survived))
df_sex <- titanic.data %>%
  group_by(sex) %>%
  summarise(mean_survived = mean(survived))

# econo.class
table(econo.class, survived)
# 막대그래프
ggplot(data=titanic.data, aes(x=econo.class)) + geom_bar()
# 생존률
round(prop.table(table(econo.class, survived),1),2)
ggplot(data=df_eco, aes(x=econo.class, y=mean_survived)) +
  geom_col()

# age
table(age, survived)
# 막대그래프
ggplot(data=titanic.data, aes(x=age)) + geom_bar()
# 생존률
round(prop.table(table(age, survived),1),2)
ggplot(data=df_age, aes(x=age, y=mean_survived)) +
  geom_col()

# sex
table(sex, survived)
# 막대그래프
ggplot(data=titanic.data, aes(x=sex)) + geom_bar()
# 생존률
round(prop.table(table(sex, survived),1),2)
ggplot(data=df_sex, aes(x=sex, y=mean_survived)) +
  geom_col()

# 특이점: econo.class = 4에는 age=0인 사람이 없다. 즉, Crew에는 어린 아이가 타지 않았다.
table(econo.class, age)

##################################
# 교차표와 그래프를 통해 각 변수가 생존 여부에 영향을 미치는 것처럼 보인다.
# 먼저 생존여부와 econo.class, age, sex 각 변수들에 대해 독립성 검정을 시행
# 먼저 각 변수를 명목형으로 바라보고 피어슨 카이제곱통계량과 우도비 검정 시행
##################################
## 1.1 Pearson's Chi-sq test
# econo.class and survived
table(econo.class, survived)
chisq.test(table(econo.class, survived))

# 멘텔-헨젤 통계량
econo <- table(econo.class, survived)
econo.prop <- econo / sum(econo)
p.i.plus <- apply(econo.prop,1,sum)
p.plus.j <- apply(econo.prop,2,sum)

u.bar <- 1*p.i.plus[1] + 2*p.i.plus[2] + 3*p.i.plus[3]+4*p.i.plus[4]
v.bar <- 1*p.plus.j[1] + 2*p.plus.j[2]

u.score <- c(1,2,3,4)
v.score <- c(1,2)

u.score.mat <- matrix(c(1,1,2,2,3,3,4,4), byrow=T, nrow=4)
v.score.mat <- matrix(c(1,1,1,1,2,2,2,2), byrow=F, nrow=4)

num <- sum((u.score.mat-u.bar)*(v.score.mat-v.bar)*econo.prop)
den <- sqrt(sum((u.score-u.bar)^2*p.i.plus) * sum((v.score-v.bar)^2*p.plus.j))
r <- num/den
M.sq = (sum(econo)-1)*r^2
M.sq         # ~chi^2(1)


# age and survived
table(age, survived)
chisq.test(table(age, survived))

# sex and survived
table(sex, survived)
chisq.test(table(sex, survived))


### 3원 분할표에 대한 로그 선형 모형
titanic.loglin = titanic.data[,c(2,3,4)]
titanic.table = table(titanic.loglin)

# (age:sex:survived) - saturated model
model1 = loglin(titanic.table, margin=list(c(1,2,3)), fit=T, param=T)
model1


# (age:sex, age:survived, sex:survived) - homogeneous association
model2 = loglin(titanic.table, margin=list(c(1,2), c(1,3), c(2,3)), fit=T, param=T)
model2

## Conditional independence
# (age:sex, age:survived)
model31 = loglin(titanic.table, margin=list(c(1,2), c(1,3)), fit=T, param=T)
model31

# (age:sex, sex:survived)
model32 = loglin(titanic.table, margin=list(c(1,2),c(2,3)), fit=T, param=T)
model32

# (age:survived, sex:survived)
model33 = loglin(titanic.table, margin=list(c(1,3),c(2,3)), fit=T, param=T)
model33

## Joint independence
# (age, sex:survived)
model41 = loglin(titanic.table, margin=list(1,c(2,3)), fit=T, param=T)
model41

# (sex, age:survived)
model42 = loglin(titanic.table, margin=list(2,c(1,3)), fit=T, param=T)
model42

# (survived, age:sex)
model43 = loglin(titanic.table, margin=list(3,c(1,2)), fit=T, param=T)
model43

## Mutual independence
model5 = loglin(titanic.table, margin=list(1,2,3), fit=T, param=T)
model5


## 4원 분할표에 대한 로그 선형 모형
titanic.table2 <- table(titanic.data)

model1 <- loglin(titanic.table2, margin=list(c(1,2,3,4)),fit=T, param=T)
model1

model2 <- loglin(titanic.table2, margin=list(c(1,c(2,3,4))),fit=T, param=T)
model2

model3 <- loglin(titanic.table2, margin=list(c(2,c(1,3,4))),fit=T, param=T)
model3

model4 <- loglin(titanic.table2, margin=list(c(3,c(1,2,4))),fit=T, param=T)
model4

model5 <- loglin(titanic.table2, margin=list(c(4,c(1,2,3))),fit=T, param=T)
model5

model6 <- loglin(titanic.table2, margin=list(c(1,2),c(1,3),c(1,4),c(2,3),c(2,4),c(3,4)),fit=T, param=T)
model6

# (ES, AS, GS): 선택
model7 <- loglin(titanic.table2, margin=list(c(1,4),c(2,4),c(3,4)),fit=T, param=T)
model7

## 로지스틱 회귀 모형

df <- titanic.data %>%
  group_by(econo.class, age, sex) %>%
  summarise(surv.rate = mean(survived), n = n())
df$survived <- (df$surv.rate * df$n)
df$age <- factor(df$age)
df$sex <- factor(df$sex)
df$econo.class <- factor(df$econo.class)

m1 <- glm(survived/n ~ age+sex+econo.class+age:sex+sex:econo.class, weights=n, family=binomial, data=df) # (A*G+G*E)

m2a <- glm(survived/n ~ age+sex+econo.class+sex:econo.class, weights=n, family=binomial, data=df) # (A+G*E)

m2b <- glm(survived/n ~ age+sex+econo.class+age:sex, weights=n, family=binomial, data=df) # (E+A*G)

m3 <- glm(survived/n ~ age+sex+econo.class, weights=n, family=binomial, data=df) # (A+G+E)

summary(m1)
summary(m2a)
summary(m2b)
summary(m3)


1-pchisq(m2a$deviance - m1$deviance, df=m2a$df.residual-m1$df.residual)
1-pchisq(m2b$deviance - m1$deviance, df=m2$df.residual-m1$df.residual)
1-pchisq(m3$deviance - m2a$deviance, df=m3$df.residual-m2a$df.residual)
1-pchisq(m3$deviance - m2b$deviance, df=m3$df.residual-m2b$df.residual)

# 선택한 모형
summary(m2a)  
