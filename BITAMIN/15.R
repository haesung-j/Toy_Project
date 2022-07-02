#install.packages('tree')
library(tree)
# install.packages('randomForest')
library(randomForest)
library(MASS)


# 데이터 불러오기
Boston <- Boston
str(Boston) # 13개 feature와 1개의 target(medv)
summary(Boston)

set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
boston.test <- Boston[-train,'medv']

# 1. Regression Tree
tree.boston <- tree(medv~., Boston, subset=train)
summary(tree.boston)

# 설명변수 lstat, rm, dis 3개만 사용하였고 터미널 노드는 8개
plot(tree.boston)
text(tree.boston, pretty=0)
# pretty=0은 각 카테고리뿐 아니라 질적변수 카테고리 이름도 포함해주는 옵션(여기서는 질적변수 사용 없었음)
yhat <- predict(tree.boston, newdata=Boston[-train,])
boston.test <- Boston[-train,'medv']
plot(yhat, boston.test)
abline(0,1, col='red', lwd=2)
mean((yhat-boston.test)^2)


####cf) pruning을 통한 성능 개선
# cv.boston <- cv.tree(tree.boston)
# cv.boston
# plot(cv.boston$size, cv.boston$dev, type='b')
# prune.boston <- prune.tree(tree.boston, best = 7)
# plot(prune.boston)
# text(prune.boston, pretty=0)
# summary(prune.boston)
# yhat <- predict(prune.boston, newdata=Boston[-train,])
# boston.test <- Boston[-train,'medv']
# plot(yhat, boston.test)
# bline(0,1, col='red', lwd=2)
# mean((yhat-boston.test)^2)
####


# 배깅은 랜덤포레스트에서 각 분류별 선택 변수의 개수가 총 변수인 특수한 경우(p개의 변수 --> 각 단계별 p개 변수 고려)
# 즉, randomForest()함수로 배깅도 수행할 수 있음!
set.seed(1)
bag.boston <- randomForest(medv~., data=Boston, subset=train,
                           mtry=13, importance=T)
bag.boston
# 여기서 mtry=13은 각 분할마다 고려 변수 개수 13개 
# 즉, 배깅을 수행한다.
# test set에 적용해, test MSE계산
yhat.bag <- predict(bag.boston, newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)
plot(yhat.bag, boston.test)      
abline(0,1, col='red', lwd=2)
# bagged regression tree의 test MSE는 13.50808

# ntree 인자로 랜덤 포레스트, 배깅에 사용할 나무의 개수를 조절할 수 있음
set.seed(1)
bag.boston2 <- randomForest(medv~., subset=train,
                           data=Boston, mtry=13,
                           ntree=25)
bag.boston2     # 25개의 의사결정 나무 default는 500
yhat.bag <- predict(bag.boston2, newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)
# 13.67774
# --> 디폴트값인 500개를 사용하나, 25개의 나무를 사용하나 이 예제에서는 큰 차이 없음! Bagging과 Random Forest는 모두 사용되는 나무의 수가 증가한다고 데이터에 overfit되지 않으며 error는 안정화되는 경향을 가진다. 너무 많은 수의 나무를 사용하면 비효율적이므로 적당한 수의 나무를 사용하는 것이 일반적이다.

# library(e1071)
# tune.randomForest(x = Boston[train,], y=Boston[train,'medv'])


#### 랜덤포레스트의 사용 변수 개수
# randomForest()의 default는 regression의 경우 3/p개 사용
# classification의 경우 sqrt(p)개 사용

set.seed(1)
rf.boston <- randomForest(medv~., data=Boston, subset=train,
                          mtry=4, importance=T)
rf.boston
yhat.rf <- predict(rf.boston, newdata=Boston[-train,])
mean((yhat.rf - boston.test)^2)
# 랜덤 포레스트가 배깅보다 더 나은 성능을 보이고 있다.
# importance()함수를 사용하면 변수의 중요도 확인 가능
importance(rf.boston)
# The former is based upon the mean decrease of accuracy in predictions on the out of bag samples when a given variable is excluded from the model. The latter is a measure of the total decrease in node impurity that results from splits over that variable, averaged over all trees

# Reg. tree는 노드의 불순도(impurity)를 training Error로 측정, classification tree는 편차(deviance)로 측정
# 변수 중요도를 그래프로 확인하기
varImpPlot(rf.boston)



set.seed(1)
rf.boston2 <- randomForest(medv~., data=Boston, subset=train,
                          mtry=5, importance=T)
yhat.rf2 <- predict(rf.boston2, newdata=Boston[-train,])
mean((yhat.rf2 - boston.test)^2)
plot(rf.boston2)
# 랜덤 포레스트가 배깅보다 더 나은 성능을 보이고 있다.
# importance()함수를 사용하면 변수의 중요도 확인 가능
importance(rf.boston2)

varImpPlot(rf.boston2)



boston.train <- Boston[train,]
set.seed(123)

est_mtry <- tuneRF(Boston[train,-14], Boston[train,'medv'],
                   stepFactor = 1.5)
est_mtry

tuned.rf <- randomForest(medv~., data=Boston, subset=train, mtry=8)
yhat <- predict(tuned.rf, newdata=Boston[-train,])
mean((yhat - boston.test)^2)





######### 1. 실습 car data
car <- read.csv('car.csv')
head(car)
str(car)
summary(car)

# 2. Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(car), 0.7*nrow(car), replace = FALSE)
car_train <- car[train,]
car_valid <- car[-train,]
summary(car_train)
summary(car_valid)

# 3.1 모형적합하기 우선 mtry는 default(sqrt(p))로 사용
model1 <- randomForest(Condition~., data=car_train, importance=T)
model1
# 나무의 개수는 500개, 각 분할 당 고려된 변수는 2개
# Error rate는 3.47%

# 3.2 tuneRF로 찾아보기
est_mtry <- tuneRF(car_train[,-7], car_train$Condition, stepFactor=2)
est_mtry
model2 <- randomForest(Condition~., data=car_train, mtry=6, importance=T)
model2
# error rate가 2.56%로 줄어듦

# 4. Validation set 예측
predvalid1 <- predict(model1, car_valid, type='class')
table(predvalid1, car_valid$Condition)
mean(predvalid1 == car_valid$Condition)    # Accuracy: 0.9538

predvalid2 <- predict(model2, car_valid, type='class')
table(predvalid2, car_valid$Condition)
mean(predvalid2 == car_valid$Condition)    # Accuracy:0.9884

# 5. 주요변수 확인
importance(model2)        
varImpPlot(model2)     # Safety, NumPersons, BuyingPrice 순(Mean DecreaseAccuracy기준)
