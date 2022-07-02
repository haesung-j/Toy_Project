snor <- matrix(c(24,1355,35,603,21,192,30,224), ncol=2, byrow=T)
colnames(snor) <- c('Yes','No')
rownames(snor) <- c('Never','Occasional','Never every night','Every night')
scores <- c(0,2,4,5)
snor.logit <- glm(snor~scores, family=binomial(link='logit'))
summary(snor.logit)

install.packages('rsq')
library(rsq)
data(hcrabs)
head(hcrabs)
table(hcrabs$color)
Y <- hcrabs$num.satellites
Y[hcrabs$num.satellites>0]<-1
x <- hcrabs$width
hcrabs$color <- as.integer(hcrabs$color)
c <- as.factor(hcrabs$color)
c <- relevel(c, ref='4')
glm(Y~x+c, family = binomial)


crab.glm <- glm(Y~x+c, family = binomial)
plot(Y~x, xlim=c(20,34))
curve(plogis(crab.glm$coefficients[1]+crab.glm$coefficients[2]*x),add=T)
curve(plogis(sum(crab.glm$coefficients[c(1,3)])+crab.glm$coefficients[2]*x),col='yellow',add=T)
curve(plogis(sum(crab.glm$coefficients[c(1,4)])+crab.glm$coefficients[2]*x),col='green',add=T)
curve(plogis(sum(crab.glm$coefficients[c(1,5)])+crab.glm$coefficients[2]*x),col='blue',add=T)


alli <- read.table('http://users.stat.ufl.edu/~aa/glm/data/Alligators3.dat',header=T)
alli <- dplyr::rename(alli,'choice'='food')
str(alli)
install.packages('VGAM')
library(VGAM)
all.fit1 <- vglm(choice~length, family=multinomial,data=alli)

