## 경사하강법
# 1. 샘플 데이터 생성
library(ggplot2)
x <- runif(300,-10,10)
Noise <- rnorm(300, 0 , 3)
y <- x + Noise

df <- data.frame(x,y)
ggplot(df, aes(x,y))+
  geom_point(col='royalblue')+
  theme_bw()

# 2. Learning Rate 설정
alpha <- 0.01

# 3. 초기 가중치 행렬 생성
Weights <- matrix(c(0,0), nrow=2)
Weights

# 4. 회귀식 계산을 위한 행렬 생성
X <- matrix(x)
X <- cbind(1,x)
head(X)

# 5. Error Loss function 만들기
Error <- function(x, y, weight){
  sum((y-x %*% weight)^2) / 2
}

# 6. 알고리즘 학습
error_surface <- c()
weight_value <- list()

for (i in 1:300){
  error <- (X %*% Weights - y)
  Delta_function <- t(X) %*% error / length(y)
  Weights <- Weights - alpha * Delta_function
  error_surface[i] <- Error(X, y, Weights)
  weight_value[[i]] <- Weights
}

# 7. 시각화
p <- ggplot(df, aes(x,y)) +
  geom_point(col='royalblue', alpha=0.4) +
  theme_bw()

for (i in 1:300){
  p <- p +
    geom_abline(slope=weight_value[[i]][2],
                intercept = weight_value[[i]][1],
                col='red', alpha=0.04)
}
p

df$num <- 1:300
df$Error_value <- error_surface

ggplot(df) +
  geom_line(aes(num, Error_value), group = 1) +
  geom_point(aes(num, Error_value)) +
  theme_bw() +
  ggtitle('Error Function') + xlab('Num of iterations')

# 8.1 최소제곱법 이용한 선형회귀식과 비교
REG <- lm(y ~x)
summary(REG)

weight_value[[300]]

GR_Model <- -0.1781085 + 1.0105671 * x             
actual <- y
rss <- sum((GR_Model - actual)^2)
tss <- sum((actual - mean(actual))^2)
rsq <- 1 - rss/tss
rsq

## ANN으로 예측
# read data
concrete <- read.csv('Concrete_Data.csv')
str(concrete)

# change col names
col <- c('cement','slag','ash','water','superplastic','coarseagg','fineagg','age','strength')
colnames(concrete) <- col
str(concrete)

# normalize data
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
concrete_norm <- as.data.frame(sapply(concrete,normalize))
summary(concrete_norm)

set.seed(619)
concrete_index <- sample(nrow(concrete),nrow(concrete)*0.75)
concrete_train <- concrete_norm[concrete_index,]
concrete_test <- concrete_norm[-concrete_index,]
str(concrete_train)
str(concrete_test)

# prepare package
install.packages('neuralnet')
library(neuralnet)

concrete_ml <- neuralnet(strength ~., data=concrete_train,  hidden=1)
plot(concrete_ml)

model_result1 <- compute(concrete_ml, concrete_test[1:8])
str(model_result1)
predicted_strength1 <- model_result1$net.result
cor(predicted_strength1, concrete_test$strength)

#
concrete_ml2 <- neuralnet(strength ~., data=concrete_train,  hidden=2)
model_result2 <- compute(concrete_ml2, concrete_test[1:8])
predicted_strength2 <- model_result2$net.result
cor(predicted_strength2, concrete_test$strength)

## change the parameter
perf <- matrix(ncol=1, nrow=5)
for (i in 1:5){
  concrete_m <- neuralnet(strength ~., data=concrete_train,  hidden=i)
  model_result <- compute(concrete_m, concrete_test[1:8])
  predicted_strength <- model_result$net.result
  corr <- cor(predicted_strength, concrete_test$strength)
  perf[i,1] <- corr
}
perf


## Weblogin data
train <- read.csv('weblogin_train.csv', stringsAsFactors = F)
test <- read.csv('weblogin_test.csv', stringsAsFactors = F)
str(train)

library(tidyverse)
library(neuralnet)
library(gmodels)
library(Metrics)

# 정규화
train$label <- 'train'
test$label <- 'test'
pool <- rbind(train,test)

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

str(pool)
pool_norm <- cbind(as.data.frame(lapply(pool[,c(1,2,3,4,5,7,8)], normalize)), login = pool$login, label = pool$label)
train_set <- pool_norm %>% filter(label=='train') %>% select(-label)
test_set <- pool_norm %>% filter(label=='test') %>% select(-label)

set.seed(619)
layers <- c(); error <- c()
for (i in 1:5){
  model <- neuralnet(login ~., data=train_set, hidden=i,
                     act.fct = 'logistic', linear.output=F)
  layers <- append(layers, i)
  error <- append(error, model$result.matrix[1])
}
eval_table <- data.frame(layer=layers, error=error)
qplot(as.factor(layers), error, data=eval_table, geom='col', fill=as.factor(layers))

# 예측
model <- neuralnet(login~., data=train_set, hidden=5, act.fct = 'logistic', linear.output = F)
result <- neuralnet :: compute(model, test_set[,1:7])$net.result

# MSE
table <- CrossTable(ifelse(result>0.5,1,0), test$login)$prop.tbl
acc <- table[1,1] + table[2,2]; acc
