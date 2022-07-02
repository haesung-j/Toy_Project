install.packages('foreign')
library(foreign)
library(readxl)
library(ggplot2)
library(dplyr)

## 데이터 준비하기
raw_welfare <- read.spss(file='Koweps_hpc10_2015_beta1.sav',
                         to.data.frame = T)
welfare <- raw_welfare
welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7)
head(welfare)


#### 성별에 따른 월급 비교
# 1. 변수 검토하기  - sex변수
class(welfare$sex)              # numeric
table(welfare$sex)

# 2. 전처리 - sex변수
boxplot(welfare$sex)
welfare$sex <- ifelse(welfare$sex==1, 'male', 'female')
table(welfare$sex)
qplot(welfare$sex)              # 빈도그림


# 1. 변수 검토하기  - income 변수
summary(welfare$income)         # NA - 12030
qplot(welfare$income)           # 최대값까지라 가독성문제
qplot(welfare$incom)+xlim(0,1000)

# 2. 전처리 - income 변수
summary(welfare$income)
welfare$income <- ifelse(welfare$income %in% c(0,9999), NA, welfare$income)

table(is.na(welfare$income))    # NA - 12044


# 성별 월급 평균표 만들기
sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(mean_income = mean(income))

sex_income

# 성별 월급 평균 그래프 만들기
ggplot(sex_income, aes(sex,mean_income))+geom_col()


#### 연령대에 따른 월급 비교       
# 파생변수 만들기 - 연령대
summary(welfare$birth)
table(is.na(welfare$birth))      # NA - 0
welfare$birth <- ifelse(welfare$birth==9999,NA,welfare$birth)
table(is.na(welfare$birth))

welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)

welfare <- welfare %>%
  mutate(ageg = ifelse(age<30, 'young',
                       ifelse(age <= 59, 'middle','old')))
table(welfare$ageg)

# 연령대별 월급 평균표 만들기
ageg_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarise(mean_income = mean(income))

ageg_income

# 연령대별 월급 평균 그래프 만들기
ggplot(ageg_income, aes(ageg,mean_income))+geom_col()

ggplot(ageg_income, aes(ageg,mean_income))+geom_col()+
  scale_x_discrete(limits=c('young','middle','old'))


## 직업별 월급 차이
# 변수 검토 - job
class(welfare$code_job)       # numeric  - 변환필요
table(welfare$code_job)

# 전처리
list_job <- read_excel('Koweps_Codebook.xlsx', col_names=T,sheet=2)
head(list_job)
dim(list_job)

# 변수 결합하기
welfare <- left_join(welfare,list_job,id='code_job')
welfare %>%
  filter(!is.na(code_job)) %>%
  select(code_job,job) %>%
  head(10)

# 1. 직업 월급 평균표 만들기
job_incomes <- welfare %>%
  filter(!is.na(job) & !is.na(income)) %>%
  group_by(job) %>%
  summarise(mean_income = mean(income))
head(job_incomes)

# 2.상위 10개 직업의 월급에 따라 내림차순 정렬
top10 <- job_incomes %>%
  arrange(desc(mean_income)) %>%
  head(10)
top10

# 3. 그래프 만들기
ggplot(top10, aes(reorder(job, mean_income), mean_income))+
         geom_col()+coord_flip()

# 4. 하위 10개 직업의 월급에 따라 내림차순 정렬
bottom10 <- job_incomes %>%
  arrange(mean_income) %>%
  head(10)

ggplot(bottom10, aes(reorder(job,-mean_income),mean_income))+
  geom_col()+
  coord_flip()+
  ylim(0,850)

## 지역별 연령대 비율
# 1. 변수검토 - region
class(welfare$code_region)
table(welfare$code_region)

# 2. 전처리
list_region <- data.frame(code_region=c(1:7),
                          region=c('서울',
                                   '수도권(인천/경기)',
                                   '부산/경남/울산',
                                   '대구/경북',
                                   '대전/충남',
                                   '강원/충북',
                                   '광주/전남/전북/제주도'))
list_region

welfare <- left_join(welfare,list_region, id='code_region')
welfare %>%
  select(code_region, region) %>%
  head()

# 1. 지역별 연령대 비율표 만들기
region_ageg <- welfare %>%
  group_by(region,ageg) %>%
  summarise(n=n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100,2))
head(region_ageg)

region_ageg <- welfare %>%
  count(region, ageg) %>%
  group_by(region) %>%
  mutate(pct=round(n/sum(n)*100,2) )

# 2. 그래프 만들기
ggplot(region_ageg, aes(region, pct, fill=ageg))+
  geom_col()+
  coord_flip()

# 3. 노년층 비율 순으로 막대 정렬하기
list_order_old <- region_ageg %>%
  filter(ageg=='old') %>%
  arrange(pct)
list_order_old

order <- list_order_old$region    # 지역명 추출
order 

ggplot(region_ageg, aes(region,pct,fill=ageg))+
  geom_col()+
  coord_flip()+
  scale_x_discrete(limits=order)

# 4. 연령대 순으로 막대 색깔 나열하기
class(region_ageg$ageg)
levels(region_ageg$ageg)

region_ageg$ageg <- factor(region_ageg$ageg,
                           level=c('old','middle','young'))
class(region_ageg$ageg)
levels(region_ageg$ageg)

ggplot(region_ageg, aes(region,pct,fill=ageg))+
  geom_col()+
  coord_flip()+
  scale_x_discrete(limits=order)

#### 실습하기 ####

# 데이터 불러오기
raw_lab20 <- read.spss('klips20p.sav', to.data.frame = T)

## 데이터 전처리
# 1. 변수명 변경 
labdata <- rename(raw_lab20,
                  religion = p209031,
                  drink = p206161)

# 2. 종교유무 변수 생성
table(labdata$religion == -1)     # 결측값 확인 - 없음

labdata$is_rel <- ifelse(labdata$religion==1,'종교없음','종교있음')
table(labdata$is_rel)

# 3. 음주여부 변수값 변경
table(labdata$drink == -1)         # 결측값 확인 - 없음
labdata$drink <- ifelse(labdata$drink==1, '마신다',
                        ifelse(labdata$drink==2,'현재 안마신다','안마신다'))
table(labdata$drink)

# 4. 종교유무 와 음주여부에 관련된 데이터 생성
rel_drink <- labdata %>%
  group_by(is_rel,drink) %>%
  summarise(n=n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100,1))
rel_drink

# 5. 시각화
library(ggplot2)
library(plotly)
p <- ggplot(rel_drink, aes(is_rel,pct,fill=drink))+geom_col()
ggplotly(p)

## 흡연여부와 성별은 월급과 관계가 있을까
labdata <- rename(labdata,
                  sex=p200101,
                  smoke=p206158,
                  income=p201642)
table(labdata$sex==-1)
table(labdata$income==-1)       # 결측값 1개 존재
labdata$income <- ifelse(labdata$income==-1,NA,labdata$income)

boxplot(labdata$income)$stats
labdata$income <- ifelse(labdata$income<8|labdata$income>550,NA,labdata$income)
boxplot(labdata$income)$stats
labdata$income <- ifelse(labdata$income>525,NA,labdata$income)
boxplot(labdata$income)$stats

labdata$sex <- ifelse(labdata$sex==1,'남성','여성')
labdata$smoke <- ifelse(labdata$smoke==1,'피운다',
                        ifelse(labdata$smoke==2,'현재 피운다', '안피운다'))
table(labdata$sex)
table(labdata$smoke)

smoke_income <- labdata %>%
  filter(!is.na(income))%>%
  group_by(smoke,sex)%>%
  summarise(mean_income = mean(income))
smoke_income

# 시각화
q <- ggplot(smoke_income, aes(smoke,mean_income,fill=sex))+
  geom_col(position='dodge')
ggplotly(q)
