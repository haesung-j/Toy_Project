# Data frame
names <- c("Nicolas", "Thierry", "Bernard", "Jerome") 
age <- c(27, 25, 29, 26) 
height <- c(180, 170, 185, 177) 
married <- c(TRUE, FALSE, TRUE, TRUE) 
friends.data <- data.frame(names,age,height,married) 
friends.data

friends.data2 <- data.frame(names = c("Nicolas", "Thierry", "Bernard", "Jerome"),
                            age = c(27, 25, 29, 26), 
                            height = c(180, 170, 185, 177), 
                            married = c(TRUE, FALSE, TRUE, TRUE), stringsAsFactors = F)
friends.data2

# indexing
friends.data$names   # factor
friends.data2$names  # stringAsFactor=F 사용 시 chacrater로 인식

# data_frame : tibble
library(tibble)
friends.data3 <- data_frame(names = c("Nicolas", "Thierry", "Bernard", "Jerome"),
                            age = c(27, 25, 29, 26), 
                            height = c(180, 170, 185, 177), 
                            married = c(TRUE, FALSE, TRUE, TRUE))
friends.data3
install.packages('nycflights13')
nycflights13::flights
iris

# xlsx
library(readxl)
getwd()
read_excel('excel_exam.xlsx') 
read_excel('excel_exam_novar.xlsx', col_names = F)
read_excel('excel_exam_sheet.xlsx', sheet = 3 )

# csv
read.csv('csv_exam.csv', stringsAsFactors = F) 
read.csv2('csv_exam_eu.csv', stringsAsFactors = F)
read.table('csv_exam.csv', header = T , sep = ',') 
read.table('csv_exam.csv', header = T , sep = ',', stringsAsFactors = F) 
read.table('csv_exam_space.txt',header= T, sep = '/') 
read.table('csv_exam_NA.csv',sep = ',', header = T) 
read.table('csv_exam_NA.csv',sep = ',', header = T, na = '.') 
read.csv('csv_exam_NA2.csv',na=c('','?')) 

# fwf
read.fwf('fwf.txt',widths = c(2,1,4))
read.fwf('fwf.txt',widths = c(2,1,4), col.names = c('age','gender','income'))

# 외부데이터
read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/ abalone.data',header = F, sep = ',', col.names = c('gender','Length','Diam',' Height','Whole','Shucked','Viscera','Shell','Rings')) 

# Tab 구분 데이터
read.table('delim.txt',sep = '\t',header = T, stringsAsFactors = F) 
read.delim('delim.txt',stringsAsFactors = F) 

# 데이터 프레임 저장하기: csv, txt, rda
df_midterm <- data.frame(english = c(90,80,60,70), math = c(50,60,100,20), class = c(1,1,2,2)) 
write.csv(df_midterm, file = 'df_midterm.csv') 
write.table(df_midterm, file = 'df_midterm1.txt') 

write.table(df_midterm,"df_midterm2.txt",quote=FALSE) 

write.table(df_midterm,"df_midterm3.txt",quote=FALSE,row.names=FALSE) 

write.table(df_midterm,"df_midterm4.txt",quote=FALSE,row.names=FALSE,sep=",") 

save(df_midterm, file = 'df_midterm.rda') 
rm(df_midterm)
df_midterm 
load('df_midterm.rda')
df_midterm 


#### 데이터 탐색
exam <- read.csv('csv_exam.csv') 
head(exam)
head(exam,10)
tail(exam)
tail(exam,3)
View(exam)
dim(exam)
nrow(exam)
ncol(exam)
names(exam)
str(exam)
summary(exam)

exam$math
attach(exam)
math
a <- math       # same as a<-exam$math
mean(a) ; median(a) ; sd(a) ; var(a) ; length(a) ; boxplot(a) ; hist(a) 

library(ggplot2)
df_mpg <- as.data.frame(ggplot2 :: mpg) 
?ggplot2::mpg
head(df_mpg)
tail(df_mpg)
dim(df_mpg)
names(df_mpg)
str(df_mpg)
summary(df_mpg)

head(airquality)
tail(airquality)
dim(airquality)
names(airquality)
str(airquality)
summary(airquality)

# 변수명 수정
df_raw <- data.frame(var1=c(1,2,1), var2 = c(2,3,2))
library(dplyr)
df_new <- df_raw
df_new <- rename(df_new, v1 = var1, v2 = var2)
df_new

# 데이터프레임에 새 변수 추가
exam$total <- exam$math + exam$english + exam$science 

exam$score_mean <- exam$total/3 
head(exam)
mean(exam$score_mean)

exam$grade <- ifelse(exam$score_mean>=mean(exam$score_mean),'pass','fail')
head(exam) 
table(exam$grade)      # 빈도요약
exam$grade <- ifelse(exam$score_mean>= 70,'A',ifelse(exam$score_mean>=60,'B','C'))

attach(exam)
with(exam, cut(score_mean, breaks=c(min(score_mean), 60, 70, max(score_mean)), labels = c('C','B','A'), include.lowest = T, right = F))

# 숫자로 데이터 추출하기
exam[1,]
exam[2,]
exam[c(2,3),]
exam[,1]
exam[2,3]
exam[,2]
exam[,c(2,3,4)]

# 조건 충족 행 추출
exam[exam$class == 1,]
exam[exam$math >= 80,]
exam[exam$class ==1 & exam$math>= 80,]
exam[exam$english <90 | exam$science < 50,]
subset(exam, select = c(id,math,english), subset = math >= 80) 

# sort, order, which
sort(exam$math)  # 오름차순 print
sort(exam$math, decreasing = T) # 내림차순

order(exam$math) # 색인 print
exam$math[order(exam$math)]
exam$math[order(-exam$math)]

which(exam$math >= 80) # 데이터 위치 주소
exam[which(exam$math>=80),]

exam[order(exam$math),]  # math 기준 오름차순 정렬
exam[order(-exam$math),]

     