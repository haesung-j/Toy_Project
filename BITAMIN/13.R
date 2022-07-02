  # Setting 
rm(list=ls())
library(tidyverse)
library(caret)
library(e1071)
library(gtools)
rawdata = read.csv('winequality-red.csv')
wine = rawdata %>% mutate(target=(ifelse(quality>=6,TRUE,FALSE))) %>% select(-quality)
?confusionMatrix
# Model assessment
## Oneway Holdout
mod = glm(data=wine,formula = target~.,family='binomial')

(confusion = confusionMatrix(data = as.factor(predict(object=mod,newdata = wine,type='response') > 0.5),reference = as.factor(wine $ target))) # cut.off 0.5 

names(confusion)
 
confusion $ overall[1] # accuracy

## Simple Twoway Holdout
shuffle = wine[sample(nrow(wine)),];rownames(shuffle) = NULL

head(shuffle)
Train = shuffle[1:(nrow(shuffle)*0.7),]

Test = shuffle[-(1:(nrow(shuffle)*0.7)),]

mod = glm(data=Train,formula = target~.,family='binomial')

(confusion = confusionMatrix(data = as.factor(predict(object=mod,newdata=Test,type='response') > 0.5),reference = as.factor(Test $ target))) # cut.off 0.5 

names(confusion)

confusion $ overall[1] # accuracy

## Randomization Twoway Holdout
acc = NULL
for(i in 1:100){
  
  a <- createDataPartition(y=wine $ target, p=0.7)      # target변수의 비율고려해서 파티션해줌
  
  
  shuffle = wine[sample(nrow(wine)),];rownames(shuffle) = NULL
  
  Train = shuffle[1:(nrow(shuffle)*0.7),] # 70%
  
  Test = shuffle[-(1:(nrow(shuffle)*0.7)),] # 30%
  
  mod = glm(data=Train,formula = target~.,family='binomial') # Train
  
  (confusion = confusionMatrix(data = as.factor(predict(object=mod,newdata=Test,type='response') > 0.5),reference = as.factor(Test $ target))) # cut.off 0.5 
  
  names(confusion)
  
  acc = c(acc,confusion $ overall[1]) # accuracy
}

acc # 100개의 acc

mean(acc) # Randomization에 의하여 도출된 최종 accuracy

## Simple threeway Holdout
set.seed(3)

shuffle = wine[sample(nrow(wine)),];rownames(shuffle) = NULL

nrow(shuffle) # 1599 = 800(Train) + 480(validation) + 319(test)

Train = shuffle[1:800,] # 50%

validation = shuffle[801:1280,]  # 30%

test = shuffle[1281:nrow(shuffle),] # 20%


mod = glm(data= Train, formula = target~.,family='binomial')

(confusion1 = confusionMatrix(data = as.factor(predict(object=mod,newdata=validation,type='response') > 0.4),reference = as.factor(validation $ target))) # cut.off 0.4

(confusion2 = confusionMatrix(data = as.factor(predict(object=mod,newdata=validation,type='response') > 0.5),reference = as.factor(validation $ target))) # cut.off 0.5 

(confusion3 = confusionMatrix(data = as.factor(predict(object=mod,newdata=validation,type='response') > 0.7),reference = as.factor(validation $ target))) # cut.off 0.7 

confusion1 $ overall[1] # 0.725
confusion2 $ overall[1] # 0.735   , 가장 크다 , cut off는 0.5로 선택한다.
confusion3 $ overall[1] # 0.706

mod2 = glm(data= rbind(Train,validation), formula = target~.,family='binomial')

(confusion4 = confusionMatrix(data = as.factor(predict(object=mod2,newdata=test,type='response') > 0.5),reference = as.factor(test $ target)))

confusion4 $ overall[1]

## Randomization threeway Holdout
a = NULL
for (i in 1:100){
  
  shuffle = wine[sample(nrow(wine)),];rownames(shuffle) = NULL
  
  nrow(shuffle) # 1599 = 800(Train) + 480(validation) + 319(test)
  
  Train = shuffle[1:800,] # 50%
  
  validation = shuffle[801:1280,]  # 30%
  
  test = shuffle[1281:nrow(shuffle),] # 20%
  
  
  mod = glm(data= Train, formula = target~.,family='binomial')
  
  acc = numeric(3)
  (confusion1 = confusionMatrix(data = as.factor(predict(object=mod,newdata=validation,type='response') > 0.4),reference = as.factor(validation $ target))) # cut.off 0.4
  
  (confusion2 = confusionMatrix(data = as.factor(predict(object=mod,newdata=validation,type='response') > 0.5),reference = as.factor(validation $ target))) # cut.off 0.5 
  
  (confusion3 = confusionMatrix(data = as.factor(predict(object=mod,newdata=validation,type='response') > 0.7),reference = as.factor(validation $ target))) # cut.off 0.7 
  
  acc[1]=confusion1 $ overall[1] 
  acc[2]=confusion2 $ overall[1] 
  acc[3]=confusion3 $ overall[1]
  
  optimal = which.max(acc)
  cut.off = c(0.4,0.5,0.7)
  mod2 = glm(data= rbind(Train,validation), formula = target~.,family='binomial')
  
  (confusion4 = confusionMatrix(data = as.factor(predict(object=mod2,newdata=test,type='response') > cut.off[optimal]
  ),reference = as.factor(test $ target)))
  
  a = c(a,confusion4 $ overall[1])
}

a # 100개의 acc

mean(a) # Randomization에 의하여 도출된 최종 accuracy

## k-fold cross validation
set.seed(215)

n.fold = 5 

shuffle = wine[sample(nrow(wine)),];rownames(shuffle) = NULL

ind = split(1:nrow(shuffle),1:n.fold) # 5 folds cross validation 

fold_acc = NULL

for ( i in 1:n.fold){
  
  mod = glm(data= shuffle[-ind[[i]],], formula = target~.,family='binomial')
  
  confusion = confusionMatrix(data = as.factor(predict(object=mod,newdata=shuffle[ind[[i]],],type='response') > 0.5),reference = as.factor(shuffle[ind[[i]],] $ target))
  
  fold_acc = c(fold_acc, confusion $ overall[1])
}

fold_acc

(k_Fold_acc = mean(fold_acc))

