library(ggplot2)
library(pscl)

## Example1. Y: 두 학교 학생들의 결석일수
## Example2. Y: 공원에서 잡은 물고기 수

###########################
## Description of the data
## school: school of the two, coded 1 or 2
## gender: coded 0:male, 1:female
## math: the test score for mat
## lang: the test score for language arts
## daysabs: the number of days of absence.
###########################
data <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
data <- within(data, {
  camper <- factor(camper)
})
data <- data[,c('count', 'persons','child','camper')]
summary(data)
## histogram of count
ggplot(data, aes(daysabs)) + 
  geom_histogram(binwidth = 1, 
                 fill='black', alpha=0.7,
                 colour='black')


###############################
## ZIP regression
## We need a package to run the zip model.
## use pscl package.
## persons_i가 항상 0인 집단(물고기를 잡지 않은 집단)에 포함되는가를 분석한 후, 0에 속하지 않는 집단에 대해 분석하기 위해 persons를 zero-inflation model의 독립변수로 포함
###############################
m1 <- zeroinfl(daysabs ~ math + lang+ gender | math + lang+ gender, data=data)
summary(m1)

m2 <- zeroinfl(daysabs ~lang+ gender |lang, data=data)
summary(m2)
exp(1.9046)
# Since we have 5 predictor variables in the full model, the degrees of freedom for the chi-squared test is 5
mnull <- update(m1, .~1)
pchisq(2*(logLik(m1) - logLik(mnull)), df=5, lower.tail = F)
pchisq(2*(logLik(m1) - logLik(m2)), df=2, lower.tail = F)


newdata1 <- expand.grid(0:3, factor(0:1), 1:4)
newdata1
colnames(newdata1) <- c("child", "camper", "persons")
newdata1 <- subset(newdata1, subset=(child<=persons))
newdata1$phat <- predict(m1, newdata1)

ggplot(newdata1, aes(x = child, y = phat, colour = factor(persons))) +
  geom_point() +
  geom_line() +
  facet_wrap(~camper) +
  labs(x = "Number of Children", y = "Predicted Fish Caught")