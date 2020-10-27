# 패키지 불러오기

install.packages(c("tidyverse", "readxl"))
library(tidyverse) # 데이터 가공 및 시각화
library(readxl) # 엑셀파일 불러오기 패키지 

# 엠코퍼레이션 코로나 전후로 데이터 나누기

files <- list.files(path = "data/Mcorporation/상품 카테고리 데이터_KDX 시각화 경진대회 Only/", pattern = "*.xlsx", full.names = TRUE) # 다중 엑셀파일 불러오기

glimpse(files)

mcorp <- sapply(files[2:65], read_excel, simplify=FALSE) %>% 
  bind_rows(.id = "id") # KDX_CONTEST_파일정의서.xlsx : 파일 제외

glimpse(mcorp)
rm(files)

# 구매날짜 date형으로 바꾸기

library(lubridate)
mcorp$구매날짜 <- ymd(mcorp$구매날짜)

mcorp %>%
  arrange(구매날짜) %>%
  filter(구매날짜 > "2020-02-17")

# 코로나 이전과 이후의 온라인 쇼핑내의 식료품결제의 증가추이
# 코로나로 인한 식표품의 증감추이

library(ggplot2) 

glimpse(mcorp)

# 샘플링

temp_products <- sample_n(products, 1000)
g <- ggplot(temp_products, aes(x = 구매수, y = 구매금액))
g + geom_count(col="tomato3", show.legend=F) +
  labs(subtitle="products: count vs revenue ", 
       y="revenue", 
       x="count", 
       title="Counts Plot")

#

ggplot(mcorp, aes(x=구매날짜)) + 
  geom_line(aes(y=구매수)) + 
  labs(title="Time Series Chart", 
       subtitle="Returns Percentage from 'Economics' Dataset", 
       caption="Source: Economics", 
       y="Returns %")

mcorp %>% 
  rename(date = "구매날짜") %>% 
  group_by(date) %>% 
  summarise(mean = mean(구매수)) %>% 
  ggplot(aes(x=date)) + 
  geom_line(aes(y=mean)) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Time Series Chart", 
       subtitle="Avg. Sales from 'Shinhan Card' Dataset", 
       caption="Source: Shinhan Card", 
       y="Avg. Sales (1000)") + 
  theme_bw()

# 없음 데이터 빼기

mcorp_nn <- mcorp %>%
  filter(고객성별 != "없음" & OS유형 != "없음")

# 시계열 그래프 (코로나 전후)
         
mcorp_nn %>% 
  rename(date = "구매날짜", gender = "고객성별") %>%    
  group_by(date, 카테고리명) %>% 
  summarise(mean = mean(구매수)) %>% 
  filter(date <= "2020-02-17" & 카테고리명 == "가공식품") %>%
  ggplot(aes(x=date)) + 
  geom_line(aes(y=mean, colour = 카테고리명)) + 
  geom_smooth(aes(y=mean, colour = 카테고리명)) +
  coord_cartesian(ylim = c(0, 3200)) +
  labs(
    title="가공식품(온라인 쇼핑)은 코로나 전에는 꾸준히 증가 추세이다.",
    caption="출처: 엠코퍼레이션",
    y="평균 구매수"
  )

mcorp_nn %>% 
  rename(date = "구매날짜", gender = "고객성별") %>%    
  group_by(date, 카테고리명) %>% 
  summarise(mean = mean(구매수)) %>% 
  filter(카테고리명 == "가공식품") %>%
  ggplot(aes(x=date)) + 
  geom_line(aes(y=mean, colour = 카테고리명)) + 
  geom_smooth(aes(y=mean, colour = 카테고리명)) +
  coord_cartesian(ylim = c(0, 3200)) +
  labs(
    title="가공식품(온라인 쇼핑)의 구매수는 꾸준히 증가하고 있다?",
    caption="출처: 엠코퍼레이션",
    y="평균 구매수"
  )

mcorp_nn %>% 
  rename(date = "구매날짜", gender = "고객성별") %>%    
  group_by(date, 카테고리명) %>% 
  summarise(mean = mean(구매수)) %>% 
  filter(카테고리명 == "가공식품") %>%
  ggplot(aes(x=date)) + 
  geom_line(aes(y=mean, colour = 카테고리명)) + 
  geom_smooth(aes(y=mean, colour = 카테고리명)) +
  coord_cartesian(ylim = c(0, 3200)) +
  labs(
    title="구매수 변화 희미하다",
    caption="출처: 엠코퍼레이션",
    y="평균 구매수"
  ) +
  geom_vline(xintercept = as.Date("2020-02-17",  format = "%Y-%m-%d"), linetype = 'dotted', color = 'blue')

# 샘플링 예제
# 단 산점도를 시각화할 때 유의점이 있다. 데이터가 크기 때문에 시각화 구현에 시간이 소요된다.
# 이 때 필요한 방법론이 샘플링이다.

mcorp_sp <- sample_n(mcorp, 10000)

ggplot(mcorp_sp, aes(x = 구매수, y = 구매금액)) + 
  geom_count(col="tomato3", show.legend=F) +
  scale_x_log10() + 
  scale_y_log10() +
  labs(subtitle="products: count vs revenue ", 
       x="구매수", 
       y="구매금액", 
       title="Counts Plot")

#

mcorp_sample <- sample_n(mcorp_nn, 10000)

ggplot(data = mcorp_sample) +
  scale_x_log10() + 
  scale_y_log10() + 
  geom_point(mapping = aes(x = 구매수, y = 구매금액))

head(mcorp_nn)

mcorp_select <- mcorp_nn[mcorp_nn$카테고리명 %in% c("가공식품", "농축수산물"), ]
head(mcorp_select)

theme_set(theme_bw())  # pre-set the bw theme.

mcorp_select %>%
  ggplot(aes(구매금액, 구매수)) +
  geom_jitter(aes(col = 카테고리명, size = 고객나이)) +
  geom_smooth(aes(col = 카테고리명), method = "lm", se = F) +
  labs(subtitle="mpg: Displacement vs City Mileage",
       title="Bubble chart")
