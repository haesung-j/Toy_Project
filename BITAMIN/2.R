# x에서 20이상의 원소 개수 파악
x <- sample(10:30, 30, replace=T)
sum(x>=20)

# x에서 짝수만 선별하여 even_x에 저장
x <- c(80,88,90,93,95,94,99,78,101)
x.even <- x[x%%2==0]

# 1부터 100까지 3과 5의 공배수들의 개수와 합
x <- 1:100
sum(x%%3==0 & x%%5==0)      # 공배수들의 개수
sum(x[x%%3==0 & x%%5==0])   # 공배수들의 ㅎ합

## 조건문
# 예제1
for (i in 1:10){
  print(i)
}

# 예제2
a <- 0
for (i in 1:10){
  a <- a+i
}
a

# 예제3
a<-0
for (i in 1:10){
  a <- a+i
  print(a)
}

# 예제4
fac.x = 1
for (i in 1:5){
  fac.x <- fac.x * i
  cat(i,'!=',fac.x,'\n',sep='')
}

# 예제5
for (i in 2:9){
  for(j in 1:9){
    print(paste(i,'*',j,'=',i*j))
  }
}

## while 문
# 예제1
i <- 1
while(i<=10){
  print(i)
  i<- i+1
}

# 예제2
a <- 0
i <- 1
while(i<=10){
  a <- a + i
  i <- i+1
}
a

a <- 0
i <- 1
while(i<=10){
  a <- a + i
  print(paste('1부터',i,'까지의 합은',a,'이다'))
  i <- i + 1
}

# 예제4
fac.x <- 1
i <- 1
while(i<=5){
  fac.x <- fac.x * i
  cat(i,'!=',fac.x,'\n',sep = '')
  i <- i + 1
}

# if문
# 예제1
x <- runif(1) - 0.5
if(x<0){print(abs(x))}

# 예제2
if(x<0){print(abs(x))
}else{print(x)}

# 예제3
if(x<0){
  print(x); print('x is negarive')
} else {print(x); print('x is positive')}

# 예제4
if(x>=0.5&&x<=0.5){print(x)
}else{print('wrong number')}

# 예제1
if(x>0){
  print(2*x) 
} else if (x == 0){
  print(x) 
} else{print(x)}

# 예제2 : x^2 + 4x +3 = 0 해 구하기
a=1; b=4; c=3
D <- b^2 - 4*a*c
if(D>0){
  answer = c((-b+sqrt(D))/(2*a),(-b-sqrt(D))/(2*a))
  print(answer)
} else if (D==0){
  answer = (-b)/2*a
  print(answer)
} else {
  print("No answer")
}

## ifelse문
a <- runif(10, min=-1, max=1)
z <- ifelse(a>=0,1,-1)
z

# 예제1
x <- 1:5
for (val in x){
  if(val == 3){
    break
  }
  print(val)
}

# 예제2
x <- 1:5
for (val in x){
  if(val ==3){
    next
  }
  print(val)
}

# 예제3
for (i in 2:9){
  for(j in 1:9){
    cat(i,'*',j,'=',i*j,'\t')
    if(j == 5){break}
  }
  cat('\n')
}

#### 함수
a <- c(1,3,5)
noact <- function(x){
  loc <- 3
  a[1] <- 3
  return(loc)
}
noact(a)
loc


b <- 2:4
noact2 <- function(x){
  b[1] <- 3
  glb <- c(1,2)
  return(b)
}
noact2(10)
b
glb

noact3 <- function(x,type=1){
  if(type==1) c[1] <- 3
  if(type==2) c[1] <<-3
  return(c)
}
noact3(10,1);c
noact3(10,2);c

meaan <- function(x){
  a <- sum(x)
  b <- length(x)
  mean_x <- a/b
  return(mean_x)
}
x <- 1:100
meaan(x); mean(x)

ssd <- function(x){
  a <- mean(x)
  b <- length(x)
  sd_x <- sqrt(sum((x-a)^2)/(b-1))
  print(sd_x)
}
ssd(x); sd(x)


googoo81 <- function(x){
  cat('\n')
  for (i in 1:9) {
    cat(x,'*',i,'=',x*i,'\n')
  }
  cat('\n')
}
googoo81(5)


fibona <- function(x){
  if(x==1) return(1)
  if(x==2) return(rep(1,2))
  a <- rep(1,x)
  for (i in 3:x)
    a[i] <- a[i-1] + a[i-2]
  return(a)
}
fibona(1); fibona(2);fibona(5)
