library(e1071)
str(iris)
iris.subset <- subset(iris, select=c('Sepal.Length', 'Sepal.Width','Species'), Species %in% c('setosa', 'virginica'))
iris.subset

plot(iris.subset$Sepal.Length, iris.subset$Sepal.Width, col=iris.subset$Species, pch=19)

svm.model <- svm(Species~., data=iris.subset, kernel='linear',cost=1, scale=F)
svm.model
points(svm.model$SV, col='blue',cex=3)

w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
abline(a =-b/w[1,2], b=-w[1,1]/w[1,2], col='red', lty=5)

svm.model <- svm(Species~., data=iris.subset, kernel='linear',cost=1000, scale=F)
svm.model
plot(iris.subset$Sepal.Length, iris.subset$Sepal.Width, col=iris.subset$Species, pch=19)
points(svm.model$SV, col='blue', cex=3)
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
abline(a =-b/w[1,2], b=-w[1,1]/w[1,2], col='red', lty=5)


# 유방암 데이터 예제
wbcd <- read.csv('wisc_bc_data.csv', stringsAsFactors = F)
str(wbcd)
wbcd$diagnosis <- factor(wbcd$diagnosis, level=c('B','M'),
                         labels=c('Benign','Malignant'))
wbcd <- wbcd[-1]
index <- 1:nrow(wbcd)
test_index <- sample(index, trunc(length(index)/3))
test_data <-wbcd[test_index,]
train_data <- wbcd[-test_index,]
svm.model <- svm(diagnosis~., data=train_data, cost=100, kernel='linear')
svm.model

svm_pred_train <- predict(svm.model, train_data)
table(train_data$diagnosis, svm_pred_train)
svm_pred_test <- predict(svm.model, test_data)
table(test_data$diagnosis, svm_pred_test)

linear.tune <- tune.svm(diagnosis ~., data=train_data, kernel='linear', cost=c(0.001,0.01,0.1,1,5,10,100))
summary(linear.tune)
best.linear <- linear.tune$best.model
tune.test <- predict(best.linear, newdata=train_data)
table(tune.test, train_data$diagnosis)


# 
set.seed(0)
iris.part <- iris[,c(1,2,5)]
attach(iris.part)
head(iris.part)

plot(Sepal.Width, Sepal.Length, col=Species)
legend(x=3.9, y=7.5, legend=c('setosa','versicolor','verginica'),
       fill=c('black','red','green'))

x <- subset(iris.part, select=Species)
y <- Species

model <- svm(Species ~., data=iris.part, kernel='radial')
summary(model)
y_pred <- predict(model,x)

library(caret)
confusionMatrix(data=y_pred, reference = y, positive = '1')

tune_svm <- tune(svm, train.x=x, train.y=y, kernel='radial',ranges=list(cost=10^(-2:2), gamma=c(0.25,0.5,1,2)))

final.svm <- svm(Species~., data=iris.part, kernel='radial',
                 cost=1, gamma=1)
plot(final.svm, iris.part)
legend(x=3.3,y=8,legend=c('setosa','versicolor','verginica'),
       fill=c('black','green','red'))

m <- svm(Species~., data=iris.part, kernel='radial',cost=10,gamma=0.5)
plot(m,iris.part)
legend(x=3.3,y=8,legend=c('setosa','versicolor','verginica'),
       fill=c('black','green','red'))

m <- svm(Species~., data=iris.part, kernel='radial',cost=500,gamma=0.5)
plot(m,iris.part)
legend(x=3.3,y=8,legend=c('setosa','versicolor','verginica'),
       fill=c('black','green','red'))

m <- svm(Species~., data=iris.part, kernel='radial',cost=10,gamma=100)
plot(m,iris.part)
legend(x=3.3,y=8,legend=c('setosa','versicolor','verginica'),
       fill=c('black','green','red'))

#
letters <- read.csv('letter-recognition.csv')
str(letters)
index <- sample(1:nrow(letters),nrow(letters)*0.7)
letters_train <- letters[index,]
letters_test <- letters[-index,]
library(kernlab)
letter_classifier <- ksvm(letter~., data=letters_train,
                          kernel='vanilladot')
letter_classifier
letter_predictions <- predict(letter_classifier,letters_test)
confusionMatrix(letter_predictions, letters_test$letter)

set.seed(1997)
tune_rbf <- tune.svm(letter~., data=letters_train,
                     gamma=10^(-2:2), cost=10^(-1:1),
                     tunecontrol = tune.control(cross=5))
trun_rbf
tune_rbf$best.model
letter_classifier_rbf <- ksvm(letter~., data=letters_train,
                              kernel='rbfdot',cost=10, gamma=0.1)
summary(letter_classifier_rbf)
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)
confusionMatrix(letter_predictions_rbf, letters_test$letter)

set.seed(0)
car <- mtcars
rownames(car) <- c()
index <- sample(1:nrow(car), nrow(car)*0.7)
train <- car[index,]
test <- car[-index,]
tune <- tune.svm(mpg~., data=car, gamma=2^(-1:1),cost=2^(2:4))

m <- tune$best.model                 
m
yhat_test <- predict(m, test)
plot(test$mpg, yhat_test, main='SVR')
abline(0,1)
mse <- mean((yhat_test - test$mpg)^2)
mse

set.seed(814)
test <- runif(100) * 10
test[sample(1:100,5)] <- sample(10:20,5)
head(test,10)

plot(test, type='l', col='blue')

svm_model <- ksvm(test, nu=0.01, type='one-svc',
                  kernel='vanilladot')
svm_model

get_index <- predict(svm_model)
head(get_index)
out_index <- which(get_index[,1]==T)
out_index
test[out_index]
plot(test, col='blue', type='l')
points(out_index, test[out_index], pch=18, col='red')
