# 1. Mann-Whitney 검정
score <- c(8,3,4,6,1,7,9,10,12)
group <- c('A','A','A','A','B','B','B','B','B')
mannwhitney <- data.frame(score,group)
mannwhitney
attach(mannwhitney)
order <- rank(score)
order
mannwhitney <- cbind(mannwhitney, order)
mannwhitney
tapply(order,group,sum)
n1 <- 20
n2 <- 20
N <- n1*n2
Utable <- matrix(c(1:N), ncol = n1, byrow=T)
for (m in 1:20){
  for(n in 1:20){
    Utable[m,n] <- qwilcox(0.05, m, n)
  }
}
head(Utable)

# 윌콕슨 순위합 검정
wilcox.test(order~group)

id <- c('A','B','C','D','E','F','G')
treatment <- c(15,17,10,14,13,10,8)
control <- c(10,15,11,13,7,5,5)
signedRanks <- data.frame(id,treatment,control)
signedRanks
wilcox.test(treatment,control, paired=TRUE)

diffs <- treatment - control
diffs
order <- rank(diffs)
order
signs <- c(1,1,-1,1,1,1,1)
signs
signed <- order * signs
signed

signedRanks <- cbind(signedRanks, diffs, order, signs, signed)
signedRanks
tapply(signed, signs, sum)


# K-W Test 1
# 수면제 종류
sleeping_pills <- as.factor(c('A','A','B','B','B','C','C'))
# 수면제 약효
effect <- c(2,4,1,3,6,5,7)
effect_by_pills <- data.frame(sleeping_pills = sleeping_pills, effect=effect)
effect_by_pills

# 검정 코드 : Kruskal.test(data, 종속변수, 처리요인)
kruskal.test(data=effect_by_pills, effect, sleeping_pills)

# K-W Test 2
# 인장강도
strength <- c(56,60,57,64,48,61,49,53,52,50,44,46)
# 기계종류
machine <- as.factor(c('A','A','A','A',
                       'B','B','B','B',
                       'C','C','C','C'))
strength_by_machines <- data.frame(machine,strength)
strength_by_machines

kruskal.test(data=strength_by_machines, strength, machine)


# Friedman Test 1
# 수면제 약효
effect <- c(3,2,3,3,2,3,1,1,1,1,2,2)
# 수명제종류
sleeping_pills <- as.factor(c('A','A','A','A',
                              'B','B','B','B',
                              'C','C','C','C'))
# 그룹
groups <- as.factor(c(1,2,3,4,1,2,3,4,1,2,3,4))
effect_by_pillsXgroups <- data.frame(sleeping_pills,
                                     groups,
                                     effect)

# friedman test 코드 : friedman.test(data,종속,요인,블록)
effect_by_pillsXgroups
friedman.test(data=effect_by_pillsXgroups,
              effect, sleeping_pills, groups)

# Friedman Test 2
# 합금 강도
alloy_strength <- c(90,95,100,
                    100, 94, 100,
                    104, 110, 113,
                    110, 105, 115)
# 주조 시간
time <- as.factor(c('A1','A1','A1',
                    'A2','A2','A2',
                    'A3','A3','A3',
                    'A4','A4','A4'))
# 혼합 비율
combine_rate <- as.factor(c('B1','B2','B3',
                            'B1','B2','B3',
                            'B1','B2','B3',
                            'B1','B2','B3'))
str_by_timeXrate <- data.frame(time,
                               combine_rate,
                               alloy_strength)
str_by_timeXrate

# case1: 혼합비율을 요인, 주조시간을 블록변수로
friedman.test(data=str_by_timeXrate,
              alloy_strength, combine_rate, time)
# case2: 혼합비율을 블록, 주조시간을 요인변수로
friedman.test(data=str_by_timeXrate,
              alloy_strength, time, combine_rate)



# 스피어만 상관계수
x <- c(6,9,2,8,5)
y <- c(8,10,4,7,3)
cor.test(x,y,method='spearman')

# 동일 순위가 있는 경우
load('spearman.rda')
attach(spearman)
x <- rank(Locations)
y <- rank(Salse)
spearman <- cbind(spearman, x, y)
spearman

cor(Locations, Sales) 
plot(Locations, Sales, abline(lm(Sales~Locations)))

cor(x,y)

# 결측치가 있는 경우 cor()은 결측치 반환
iris.na.test <- iris[,1:4]
iris.na.test[1,1] <- NA
iris.na.test[3,2] <- NA
iris.na.test[4,3] <- NA
head(iris.na.test)
cor(iris.na.test)

# 결측치 처리방법
#1) 결측치 존재하는 데이터 행벡터 삭제
cor(iris.na.test, use='complete.obs')
#2) 결측치가 존재하는 위치에서의 연산만 건너뛰는 방법
cor(iris.na.test, use='pairwise.complete.obs')


# Kendall Tau 계수
cor.test(x,y,method='kendall')


## 연습문제
x <- c(141.8,140.2,131.8,132.5,135.7,141.2,143.9,140.2,140.8,131.7,130.8,135.6,143.6,133.2)
y <- c(89.7,74.4,83.5,77.8,85.8,86.5,89.4,89.3,88.0,82.2,84.6,84.4,86.3,85.9)
#1
cor.test(x,y,method='spearman')
#2
cor.test(x,y,method='spearman', alternative = 'greater')
