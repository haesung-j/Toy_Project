#### 1. 데이터 탐색 ####
# 데이터 불러오기 
wbcd <- read.csv('wisc_bc_data.csv',stringsAsFactors = F)
# 데이터 구조확인 
str(wbcd)
# id 변수 제외
wbcd <- wbcd[-1]
# diagnosis 변수의 빈도수 확인
table(wbcd$diagnosis)
# diagnosis factor화
wbcd$diagnosis <- factor(wbcd$diagnosis, levels=c('B','M'),
                         labels=c('Benign','Malignant'))
summary(wbcd$diagnosis)
str(wbcd$diagnosis)

# diagnosis 변수 비율 확인
round(prop.table(table(wbcd$diagnosis))*100, digit=1)

# feature 변수 radius_mean, area_mean, smoothness_mean 살펴보기
library(dplyr)
wbcd %>% select(radius_mean, area_mean, smoothness_mean)%>%
  summary()                  # 단위 차이가 많이 남 --> scaling?

# 수치데이터 정규화 -- normalize() 함수 만들기
normalize <- function(x) {
  a <- ( x - min(x) ) / ( max(x) - min(x) )
  return(a)
}
normalize(c(1,2,3,4,5))   # o.k
normalize(c(10,20,30,40,50))  # o.k

# lapply() 함수를 이용해 정규화 적용
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)

# train, test set 만들기
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

#### 2. 모델 훈련 - knn in class package ####
library(class)

# knn함수 --> knn(train, test, class of train ,k)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl= wbcd_train_labels, k = 21) # sqrt(469) ~= 21

#### 3. 모델 성는 평가 by 교차표 - CrossTable in gmodels package ####
library(gmodels)

CrossTable(wbcd_test_labels, wbcd_test_pred, chisq = F)
contrasts(wbcd$diagnosis)     # Malignant: TRUE


#### 4. 모델 성능 개선 ####
# z- 변환
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z[,1:3])          # 확인

wbcd_train <- wbcd_z[1:469,]
wbcd_test <- wbcd_z[-(1:469),]
wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[-(1:469),1]
wbcd_test_pred <- knn(train=wbcd_train, test=wbcd_test,
                      cl=wbcd_train_labels, k=21)
CrossTable(wbcd_test_labels, wbcd_test_pred, chisq = F)

# 다른 k 값
wbcd_test_pred <- knn(train=wbcd_train, test=wbcd_test,
                      cl=wbcd_train_labels, k=1)
CrossTable(wbcd_test_labels, wbcd_test_pred, chisq = F)

wbcd_test_pred <- knn(train=wbcd_train, test=wbcd_test,
                      cl=wbcd_train_labels, k=5)
CrossTable(wbcd_test_labels, wbcd_test_pred, chisq = F)

wbcd_test_pred <- knn(train=wbcd_train, test=wbcd_test,
                      cl=wbcd_train_labels, k=11)
CrossTable(wbcd_test_labels, wbcd_test_pred, chisq = F)


#### iris data로 knn 해보기 ####
str(iris)

normalize <- function(x){
  a <- ( x - min(x) ) / ( max(x) - min(x) )
  return(a)
}

iris_nom <- as.data.frame(lapply(iris[1:4], normalize))
set.seed(0)

ind <- sample(2, nrow(iris), replace=T, prob=c(0.7,0.3))

iris.train <- iris[ind==1, 1:4]
iris.test <- iris[ind==2, 1:4]

iris_train_labels <- iris[ind==1, 5]
iris_test_labels <- iris[ind==2, 5]

# knn 함수 짜기 (Euclidean dist.)
knn.t <- function(train, test, cl, k) {
  test.result <- numeric(length(test))
  
  euclidean.dist <- function(x,y){
    sqrt(sum((x-y)^2))
  }
  
  for (i in 1:nrow(test)){
    train$dist <- sapply(1:nrow(train), function(ntrain){
      euclidean.dist(train[ntrain,1:4], test[i,1:4])
    })
    
    nearest.k <- order(train$dist)[1:k]
    
    nearest.k.cat <- cl[nearest.k]
    
    common.cat <- names(sort(table(nearest.k.cat),decreasing = T)[1])
    
    test.result[i] <- common.cat
  }
  train$dist = NULL
  test.result
}

result <- knn.t(train=iris.train, test=iris.test,
                cl=iris_train_labels, k=3)
result

CrossTable(iris_test_labels, result, prop.chisq = F)
