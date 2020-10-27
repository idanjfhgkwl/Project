# KDX 한국데이터거래소 소비트렌드 코리아 2020



## 개요

### 1. 과제
+ KDX의 다양한 데이터와 외부 데이터를 활용해 한국의 소비 트렌드를 분석해 인사이트를 도출하고 이를 보기 좋게 시각화 해주세요.
- 예시 : "코로나 전후, 온라인과 오프라인의 소비는 어떻게 다를까?" "특정 사회 이슈와 소비의 관계는?" "주가와 특정 품목이 연관성이 있을까?" "주중에 사람들이 사는 품목과 주말에 유독 가격이 뛴 부동산은?"

### 2. 설명
+ KDX가 제공하는 다양한 소비 데이터(온라인 쇼핑, 오프라인 신용카드, 부동산 등)를 융,복합해 분석해주세요.
+ 분석 결과는 참신한 아이디어와 창의력을 이용해 시각화 해주세요.
+ 공공과 민간을 가리지않고 많은 분야에 활용될 수 있는 결과가 나온다면 더욱 환영입니다.

### 3. 데이터셋 구성 
+ 123

### 4. 상금 (총 600만원)
+ 대상 : 300만원
+ 최우수상 : 150만원
+ 우수상 : 100만원
+ 장려상 : 25만원 (2팀)

### 5. 규칙 
+ 제공 데이터 : MBN 뉴스데이터, 삼성카드 구매 데이터, 엠코퍼레이션 온라인 구매 데이터, 신한카드 오프라인 구매 데이터, 지인플러스 전국 아파트 시세 및 거래량 데이터
+ 이용가능한 데이터 :  KDX 무료데이터, 외부 또는 공공데이터, 크롤링 데이터
+ 팀은 개인 또는 최대 4명까지 구성 가능
+ 참가신청 시 발급받은 계정으로 내부 분석환경에 접속한 후 내/외부 데이터를 통해 분석 수행
+ 내부 분석환경에서 코드 저장 후 제출 페이지에서 제출
+ 결과 제출시, 기획안 ppt는 pdf로 변환하여 제출
+ 대시보드는 5개 이상의 차트로 구성, 인사이트에 대한 설명이 반드시 포함되야 함

### 6. 참가
+ 작업툴 : RStudio
+ 인원 : 4명
+ 주요업무 : 
  + 기간 : 20.10.12 ~ 20.10.25



## 프로그램 소스코드 설명
### 1. 패키지 설치 및 라이브러리 불러오기

```r
install.packages("readxl")
install.packages("tidyverse")
install.packages("lubridate")
```

```r
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
```

* 제출 서버 환경에서 tidyverse 설치는 되지만 불러오기가 안 돼서 각각 불러온다.

### 2. 신한카드 데이터 불러오기

```r
shinhan <- read_xlsx("kdx2020/data/Shinhancard.xlsx")
```

<img src="https://raw.githubusercontent.com/idanjfhgkwl/Project/main/kdx2020/img/K-001.png">

### 3. 데이터 전처리

#### 3-1) 불필요한 열 제거 (6~8열)

```r
shinhan <- shinhan %>%
  select(-c(6:8))
```

#### 3-2) 일별 데이터 형식(chr)을 날짜 형식(date)으로 바꾸기

```r
shinhan$일별 <- ymd(shinhan$일별)
```

#### 3-3) 업종 코드 지우기

```r
shinhan <- shinhan %>%
  separate(업종, c(" ", "업종"), sep = "_")
shinhan <- shinhan %>%
  select(-4)
```

#### 3-4) 19년 2~4월과 20년 2~4월 데이터를 비교하기 위한 준비

```r
shinhan_covid <- shinhan %>%
  mutate(
    코로나 = case_when(
      일별 >= "2019-02-01" & 일별 < "2019-05-01"  ~ "2019",
      일별 >= "2020-02-01" & 일별 < "2020-05-01"  ~ "2020",
      TRUE ~ "기타"))
```

<img src="https://raw.githubusercontent.com/idanjfhgkwl/Project/main/kdx2020/img/K-20201027-140940.png">

### 4. 모델링

#### 4-1)

```r
shinhan %>%
  rename(date = "일별") %>%  
  group_by(date, 업종) %>% 
  summarise(mean = mean(`카드이용건수(천건)`)) %>%  
  filter(data <- 업종 =="한식"| 업종 =="일식/중식/양식"| 
           업종 =="제과/커피/패스트푸드"| 업종 =="기타요식") %>%
  ggplot(aes(x=date)) + 
  geom_smooth(aes(y=mean, colour = 업종), se= F) +
  geom_line(aes(y=mean, colour = 업종)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(vjust=0.6, size = 12, face = "bold"), 
        axis.text.y = element_text(size =12, face = 'bold'), 
        axis.title=element_text(size=14, face='bold'),
        title = element_text(size=15, face='bold'),
        legend.position = "top", 
        legend.text = element_text(size= 18))+
  labs(title="신한카드", 
       subtitle="외식업종 카드 사용현황", 
       caption="Source: shinhancard", 
       y="이용건수", x= "구매기간") 
```

<img src="https://raw.githubusercontent.com/idanjfhgkwl/Project/main/kdx2020/img/Rplot.png">

*	외식업의 현황을 확인하기 위해 시계열 그래프를 생성함
*	업종 내의 항목을 표현하기 위해 groupby 설정
*	시계열 그래프의 가독성을 높이기 위해 중앙선을 추가

#### 4-2)

```r
shinhan %>%
  group_by(업종, 일별) %>%
  count(`카드이용건수(천건)`) %>%
  filter(data <- 업종 =="한식"| 업종 =="일식/중식/양식"| 
           업종 =="제과/커피/패스트푸드"| 업종 =="기타요식") %>%
  ggplot(aes(x = 업종, y= `카드이용건수(천건)`)) + 
  geom_bar(stat = "identity", position= 'dodge', width=.8, fill= "#FF6666") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size =12, face = 'bold'), 
        axis.title=element_text(size=14, face='bold'),
        title = element_text(size=15, face='bold'),
        legend.position = "top") +
  labs(title="신한카드", 
       subtitle="외식업종 카드 사용현황", 
       caption="source: shinhancard",
       x= " ")
```

<img src="https://raw.githubusercontent.com/idanjfhgkwl/Project/main/kdx2020/img/Rplot01.png">

* 업종별 카드사용 현황을 세부적으로 확인 하기위해 바형 그래프로 설정함

#### 4-3)

```r
shinhan %>%
  rename(date = "일별") %>%  
  group_by(date, 업종, 연령대별) %>%  
  filter(연령대별 %in% c("20대", "30대", "40대", "50대")) %>%
  summarise(mean = mean(`카드이용건수(천건)`)) %>%
  filter(data <- 업종 =="한식"| 업종 =="일식/중식/양식"| 
           업종 =="제과/커피/패스트푸드"| 업종 =="기타요식") %>%
  ggplot(aes(x=date)) + 
  geom_smooth(aes(y=mean, colour = 연령대별),se=F, size=1.5) + 
  labs(title="신한카드", 
       subtitle="외식업종 카드 사용현황", 
       caption="Source: shinhancard", 
       y="구매횟수", x= "구매기간") + 
  theme_minimal() +
  theme(axis.text.x = element_text(vjust=0.6, size = 12, face = "bold"), 
        axis.text.y = element_text(size =12, face = 'bold'), 
        axis.title=element_text(size=14, face='bold'),
        title = element_text(size=15, face='bold'),
        legend.position = "top")
```

<img src="https://raw.githubusercontent.com/idanjfhgkwl/Project/main/kdx2020/img/Rplot02.png">

*	외식업의 현황을 좀더 자세하게 확인하기 위하여 연령대별로 기간그래프를 생성함?
*	평소에는 20대의 매출이 많았으나 코로나 대책인 거리두기의 일환으로 20대의 매출이 크게 감소한 것으로 생각됨

#### 4-4)

```r
shinhan %>%
  rename(date = "일별") %>%  
  group_by(date, 업종, 연령대별) %>%  
  filter(연령대별 %in% c("20대", "30대", "40대", "50대")) %>%
  summarise(mean = mean(`카드이용건수(천건)`)) %>%
  filter(data <- 업종 =="한식"| 업종 =="일식/중식/양식"| 
           업종 =="제과/커피/패스트푸드"| 업종 =="기타요식") %>%
  ggplot(aes(x=date)) + 
  geom_smooth(aes(y=mean, colour = 업종),se=F, size=1.5) + 
  facet_grid(연령대별 ~ .) + 
  labs(title="신한카드", 
       subtitle="외식업종 카드 사용현황", 
       caption="Source: shinhancard", 
       y="구매횟수", x= "구매기간") + 
  theme_minimal() +
  theme(axis.text.x = element_text(vjust=0.6, size = 12, face = "bold"), 
        axis.text.y = element_text(size =12, face = 'bold'), 
        axis.title=element_text(size=14, face='bold'),
        title = element_text(size=15, face='bold'),
        legend.position = "top")
```

<img src="https://raw.githubusercontent.com/idanjfhgkwl/Project/main/kdx2020/img/Rplot03.png">

* 연령대별 지출항목을 좀더 명확히 확인하기 위해 연령대별로 업종을 나눔
*	20, 30대의 매출이 가장 높은 업종은 기타요식(유흥음식/주점, 나이트클럽)으로 확인되고 40,50대의 경우 한식의 비중이 가장 높은 것을 확인됨

#### 4-5)

```r
shinhan_covid %>%
  group_by(일별, 업종, 연령대별, 코로나) %>% 
  summarise(업종, `카드이용건수(천건)`) %>% 
  filter(연령대별 %in% c("20대", "30대", "40대", "50대")) %>%
  filter(data <- 업종 =="한식"| 업종 =="일식/중식/양식"| 
           업종 =="제과/커피/패스트푸드"| 업종 =="기타요식") %>% 
  ggplot(aes(x = 업종, y= `카드이용건수(천건)`, fill= 연령대별)) + 
  geom_bar(stat = "identity", position = 'dodge', width=.6) +
  facet_grid(. ~코로나) +
  labs(title="신한카드", 
       subtitle="외식업종 전년대비 카드 사용현황", 
       caption="source: shinhancard",
       x= " ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=0.6, size = 12, face = "bold"), 
        axis.text.y = element_text(size =12, face = 'bold'), 
        axis.title=element_text(size=14, face='bold'),
        title = element_text(size=15, face='bold'),
        legend.position = "top")
```

<img src="https://raw.githubusercontent.com/idanjfhgkwl/Project/main/kdx2020/img/Rplot04.png">

* 시계열그래프, 기간그래프를 확인한 결과 코로나 시기에 맞물려 감소하다 다시 증가하는 추세를 보여, 증감이 모두 코로나로 인해 발생한 것인지 확인하기 위해 전년동월 데이터를 활용하기로 함
* 전년도, 금년도 데이터를 비교한 결과 전년대비 업종별 매출이 모두 감소한 것을 확인하여 코로나로 인해 매출이 감소한 것으로 추정함 
* 전년도 데이터를 비교하기 위해 mutate를 사용하여 코로나 함수를 설정함

## 제출
