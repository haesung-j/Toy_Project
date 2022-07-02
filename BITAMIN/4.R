#### 0320 예습과제 정해성 ####

#### 미국 state별 강력 범죄율 단계 구분도 만들기 ####

## 패키지 준비
install.packages('ggiraphExtra')
library(ggiraphExtra)

## 데이터 확인
str(USArrests)
head(USArrests)               # 행이름이 지역명

## 데이터 준비
library(tibble)
crime <- rownames_to_column(USArrests,var='state') # 행이름을 열로
crime$state <- tolower(crime$state)   # 소문자 변환
crime$state
head(crime)
str(crime)

install.packages('maps')
library(ggplot2)

states_map <- map_data('state')     # 위도,경도 데이터 가져오기
str(states_map)

## 단계 구분도 만들기
install.packages('mapproj')
ggChoropleth(data=crime,            # 지도에 표현할 데이터
             aes(fill=Murder,       # 색깔로 표현할 변수
                 map_id=state),     # 지역 기준 변수
             map = states_map)      # 지도 데이터

## 인터랙티브 단계 구분도 만들기 - 마우스 움직임에 반응
ggChoropleth(data=crime,            # 지도에 표현할 데이터
             aes(fill=Murder,       # 색깔로 표현할 변수
                 map_id=state),     # 지역 기준 변수
             map = states_map,      # 지도 데이터
             interactive=T)         # 인터랙티브

#### 대한민국 시도별 인구 단계 구분도 만들기 ####

## 패키지 준비
# stringi 패키지 - 문자열/텍스트 처리
install.packages('stringi')

# devtools 패키지 - github로부터 패키지 다운 가능
install.packages('devtools')
devtools :: install_github('cardiomoon/kormaps2014')
library(kormaps2014)
library(ggiraphExtra)
library(dplyr)
library(ggplot2)
library(mapproj)

## 데이터 준비
head(korpop1)                       # 한글 깨짐
str(changeCode(korpop1))            # CP949로 변환 그냥 영문으로
# 변수명 변경
korpop1 <- rename(korpop1, pop='총인구_명',
                  name='행정구역별_읍면동')
str(changeCode(korpop1))
str(changeCode(kormap1))

# 단계 구분도 만들기
ggChoropleth(data=korpop1,           # 지도에 표현할 데이터 
             aes(fill=pop,           # 색깔로 표현할 변수
                 map_id=code,        # 지역 기준 변수 
                 tooltip=name),      # 지도위 표시할 지역명
             map = kormap1,          # 지도 데이터
             interactive=T)          # 인터랙티브


#### 대한민국 시도별 결핵 환자 수 단계 구분도 만들기 ####
## 패키지 준비
# 앞과 패키지 동일

# 데이터 준비
str(changeCode(tbc))
str(changeCode(kormap1))
View(changeCode(tbc))
ggChoropleth(data=tbc,              # 지도에 표현할 데이터 
            aes(fill=NewPts,        # 색깔로 표현할 변수
                map_id=code,        # 지역 기준 변수 
                tooltip=name),      # 지도위 표시할 지역명
            map = kormap1,          # 지도 데이터
            interactive=T)          # 인터랙티브


#### plotly 패키지로 인터랙티브 그래프 만들기 ####

## 패키지 설치
install.packages('plotly')
library(plotly)
library(ggplot2)

## 데이터 준비 - mpg data
str(mpg)

## ggplot2로 그래프 만들기
p<- ggplot(mpg, aes(displ,hwy,col=drv))+
  geom_point()

## 인터랙티브 그래프 만들기
ggplotly(p)


## 인터랙티브 막대 그래프 만들기
p <- ggplot(diamonds, aes(cut,fill=clarity))+
  geom_bar(position='dodge')

ggplotly(p)


#### dygraphs 패키지로 인터랙티브 시계열 그래프 만들기 ####
install.packages('dygraphs')       # 데이터가 xts타입이어야함
library(dygraphs)

## 데이터 준비
economics <- ggplot2 :: economics
head(economics)
class(economics$unemploy)       # int 타입 -> xts로 변환해야함

## xts타입으로 변경
# xts(data, order.by=)
library(xts)   
eco <- xts(economics$unemploy, order.by=economics$date)
head(eco)

## 인터랙티브 시계열 그래프 만들기
dygraph(eco, main='bitamin data')


## 인터랙티브 시계열 그래프 아래에 날짜 선택기능 추가하기
dygraph(eco) %>% dyRangeSelector()

# 여러 옵션
dygraph(eco) %>% 
  dyRangeSelector(dateWindow = c('1970-01-01','1980-01-01'),   # 날짜 지정
                  height=100,                               # 높이 지정
                  fillColor = 'red',                          # 채우기 색 지정
                  strokeColor = 'black')                    # 테두리 색 지정


## 인터랙티브 시계열 그래프에 여러 값 표현하기
# unemploy와 psavert을 함께 표현하기
class(economics$psavert)
eco_a <- xts(economics$psavert, order.by = economics$date)
class(economics$unemploy/1000)
eco_b <- xts(economics$unemploy/1000, order.by=economics$date)

head(eco_a)
head(eco_b)

# xts 합치기
eco2 <- cbind(eco_a, eco_b)
colnames(eco2) <- c('pasvert','unemploy')
head(eco2)

# 그래프 만들기
dygraph(eco2) %>% dyRangeSelector()

#### dygraph 패키지의 다른 함수

# dySeries()
dygraph(eco2) %>%
  dySeries('pasvert',label='pppp',color='red',fillGraph=T) %>% dySeries('unemploy',label='kkkk',color='blue',fillGraph = T)

