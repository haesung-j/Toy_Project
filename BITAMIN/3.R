library(ggplot2)
#### 1. 배경설정
mpg <- as.data.frame(ggplot2::mpg)
ggplot(data=mpg, aes(x=displ,y=hwy))

ggplot(data=mpg, aes(x=drv))

#### 2. 그래프 추가

## 변수1개 - 이산형
# barplot
ggplot(data=mpg, aes(x=drv))+
  geom_bar()

## 변수1개 - 연속형
# (1) histogram
ggplot(data=mpg, aes(x=hwy))+
  geom_histogram()            # 디폴트: 구간 범위: 전체/30

ggplot(data=mpg, aes(x=hwy))+
  geom_histogram(binwidth = 4)# 구간 범위: 전체/4

ggplot(data=mpg, aes(x=hwy))+
  geom_histogram(bins = 10)   # 구간 개수: 10개

# (2) dotplot
ggplot(data=mpg, aes(x=hwy))+
  geom_dotplot()

# (3) freqpoly - 도수를 선으로 이은 것
ggplot(data=mpg, aes(x=hwy))+
  geom_freqpoly()

## 변수 2개 - 연속+연속

# geom_point - 산점도
ggplot(data=mpg, aes(x=displ, y=hwy))+
  geom_point()

# geom_line - 주로 시계열 데이터
ggplot(data=economics, aes(x=date, y=unemploy))+
  geom_line()

ggplot(data=economics_long, aes(date,value01,colour=variable))+
  geom_line()

# geom_area - 데이터를 영역으로 표현한 그래프
ggplot(data=economics, aes(date,unemploy))+
  geom_area()

# geom_smooth - 전반적 추세를 곡선으로
ggplot(data=economics, aes(date,unemploy))+
  geom_point()+
  geom_smooth()

ggplot(data=economics, aes(date,unemploy))+
  geom_line()+
  geom_smooth()

## 변수 2개 - 연속 + 이산

# geom_col() - 이산형에 대해 대응하는 연속형값을 막대로
library(dplyr)
df_mpg <- mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy))
df_mpg

ggplot(data=df_mpg, aes(drv,mean_hwy))+
  geom_col()

ggplot(data=df_mpg, aes(reorder(drv, -mean_hwy),mean_hwy))+
  geom_col()

# geom_boxplot()
ggplot(mpg, aes(drv,hwy))+
  geom_boxplot()

# geom_violin() - 형식연속 분포를 밀도와 함께 손쉽게
ggplot(mpg, aes(drv,hwy))+
  geom_violin()

## 변수 2개 - 이산 + 이산

# geom_count() - 겹치는 항목에 대한 개수 출력
ggplot(mpg, aes(drv,class))+
  geom_count()

#### 3. 설정추가
# (1)
ggplot(mpg, aes(x=class))+
  geom_bar()

ggplot(mpg, aes(x=class))+
  geom_bar(fill='red', colour='blue')

# (2)
ggplot(mpg, aes(x=displ, y=hwy))+
  geom_point()

ggplot(mpg, aes(x=displ, y=hwy))+
  geom_point()+
  xlim(3,6) + ylim(10,30)

# (3)
ggplot(mpg, aes(x=fl))+
  geom_bar()

ggplot(mpg, aes(x=fl))+
  geom_bar()+
  coord_cartesian(ylim=c(0,40))

ggplot(mpg, aes(x=fl))+
  geom_bar()+
  coord_trans(y='sqrt')

# (4) fl에 따라 col로 나누는 옵션
ggplot(mpg, aes(x=cty,y=hwy))+
  geom_point()

ggplot(mpg, aes(x=cty,y=hwy))+
  geom_point()+
  facet_grid(cols=vars(fl))

# (5) year에 따라 row로 나누는 옵션
ggplot(mpg, aes(cty,hwy))+
  geom_point()+
  facet_grid(rows=vars(year))

# (6) col은 fl기준, row는 year기준
ggplot(mpg, aes(cty,hwy))+
  geom_point()+
  facet_grid(col=vars(fl), row=vars(year))

# (7) Theme_function
r <- ggplot(mpg, aes(x=fl))+geom_bar()
r + theme_bw()
r + theme_gray()
r + theme_dark()
r + theme_light()
r + theme_classic()
r + theme_linedraw()
r + theme_minimal()
r + theme_void()
