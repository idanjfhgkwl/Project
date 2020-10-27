shinhancard

shinhancard2 <- shinhancard

colnames(shinhancard2) <- c('date', 'sex', 'age', 'business_type', 'case_number')

shinhancard2

ggplot (data = shinhancard) +
  geom_point(mapping = aes(x = 연령대별, y = `카드이용건수(천건)`))

ggplot(shinhancard2, aes(age, case_number)) +
  geom_point(mapping = aes(x = age, y = case_number)) +
  geom_smooth(se = FALSE) +
  labs(
    title = "성별, 연령대"
    )

ggplot(shinhancard2, aes(business_type, case_number)) +
  geom_col()

diamonds

ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

ggplot(data = shinhancard2) +
  geom_count(mapping = aes(x = age, y = sex))

products2 <- products

colnames(products2) <- c('id', 'date', 'category', 'sex', 'age', 'os', 'amount', 'purchase')

products2[2:8]

ggplot (data = products2[2:8]) +
  geom_point(mapping = aes(x = sex, y = age))

shinhancard2
s



mbnnews <- read_xlsx("data/mk_news_201901_202006.xlsx")
head(shinhancard)
shinhancard <- shinhancard %>% 
  select(-c(6:8))
head(shinhancard)






shinhancard - 카드이용건수
products - 구매수

# 일별 카드이용건수 by 카테고리

shinhancard %>% 
  rename(date = "일별") %>%  
  filter(업종 == "M001_한식") %>%
  group_by(date, 업종) %>% 
  summarise(mean = mean(`카드이용건수(천건)`)) %>%
  ggplot(aes(x=date)) + 
  geom_line(aes(y=mean, colour = 업종)) + 
  labs(title="Time Series Chart", 
       subtitle="Avg. Sales from 'Shinhan Card' Dataset", 
       caption="Source: Shinhan Card", 
       y="Avg. Sales (1000)")

# 업종별 구매건수(평균)

cat_rev <- shinhan_afco %>%
  group_by(업종) %>% 
  summarise(mean = mean(`카드이용건수(천건)`)) %>% 
  arrange(desc(mean)) 

shinhancard
cat_rev

cat_rev <- cat_rev[order(cat_rev$mean, decreasing = TRUE), ]  # sort
cat_rev$업종 <- factor(cat_rev$업종, levels = cat_rev$업종)  # to retain the order in plot.
head(cat_rev)
glimpse(cat_rev)
cat_rev
arrange(cat_rev, desc(업종))

head(shinhan_afco)

shinhan_afco %>%
  group_by(업종) %>% 
  summarise(mean = mean(카드이용건수.천건.)) %>% 
  arrange(업종) 

cat_rev <- shinhan_afco %>%
  group_by(업종) %>% 
  summarise(mean = mean(카드이용건수.천건.)) %>% 
  arrange(desc(mean)) 

cat_rev <- cat_rev[order(cat_rev$mean, decreasing = TRUE), ]  # sort
cat_rev$업종 <- factor(cat_rev$업종, levels = cat_rev$업종)  # to retain the order in plot.
head(cat_rev)

ggplot(cat_rev %>% head(20), aes(x = 업종, y = mean)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Ordered Bar Chart", 
       subtitle="업종 Vs Avg. 구매금액", 
       caption="source: products")

shinhan_afco %>% 
  rename(date = "년월") %>%  
  filter(업종 == "한식") %>%
  group_by(date, 업종) %>% 
  summarise(mean = mean(카드이용건수.천건.)) %>%
  ggplot(aes(x=date)) + 
  geom_line(aes(y=mean, colour = 업종)) + 
  labs(title="Time Series Chart", 
       subtitle="Avg. Sales from 'Shinhan Card' Dataset", 
       caption="Source: Shinhan Card", 
       y="Avg. Sales (1000)")

glimpse(shinhan_afco)

rm(samsung_card)

# 

install.packages("nycflights13")
library(nycflights13)
library(ggplot2)
nf <- count(flights, year, month, day)
library(lubridate)
nf <- mutate(nf, date = make_date(year, month, day))
library(ggplot2)
p <- ggplot(nf, aes(date, n)) + geom_line()
p
p + facet_wrap(~ wday(date, TRUE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
monthweek <- function(d, w) ceiling((d - w) / 7) + 1
nf <- mutate(nf, wd = wday(date, label = TRUE))
nf <- mutate(nf, wd = factor(wd, levels = rev(levels(wd))))
nf <- mutate(nf, mw = monthweek(day, wday(date)))
ggplot(nf, aes(x = as.character(mw), y = wd, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white") +
  facet_wrap(~ month(date, TRUE)) +
  ylab("") + xlab("Week of Month") +
  theme(panel.grid.major = element_blank())
nf2 <- mutate(nf, wd = wday(date, label = TRUE))
nf2 <- mutate(nf2, wd = factor(wd))
nf2 <- mutate(nf2, mw = factor(monthweek(day, wday(date))))
nf2 <- mutate(nf2, mw = factor(mw, rev(levels(mw))))
ggplot(nf2, aes(x = wd, y = mw, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white") +
  facet_wrap(~ month(date, TRUE)) +
  ylab("") + xlab("Week of Month") +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
head(nf2)

head(shinhan_temp)
monthweek <- function(d, w) ceiling((d - w) / 7) + 1
shinhan_temp <- mutate(shinhan_covid, wd = wday(일별, label = TRUE))
shinhan_temp <- mutate(shinhan_temp, wd = factor(wd))
shinhan_temp <- mutate(shinhan_temp, mw = factor(monthweek(day, wday(일별))))
shinhan_temp <- mutate(shinhan_temp, mw = factor(mw, rev(levels(mw))))
nf <- count(flights, year, month, day)
glimpse(shinhan_temp)
shinhan_temp %>%
  group_by(일별, 업종번호, `카드이용건수(천건)`, 코로나, wd, mw) %>%
  filter(업종번호 == "M001") %>%
  ggplot(aes(x = wd, y = mw, fill = `카드이용건수(천건)`)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white") +
  facet_wrap(~ month(일별, TRUE)) +
  ylab("") + xlab("Week of Month") +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 1))

shinhan_temp %>%
  select(일별, 업종번호, `카드이용건수(천건)`, 코로나, wd, mw)
  group_by(일별, 업종번호, `카드이용건수(천건)`, 코로나, wd, mw) %>%
  filter(업종번호 == "M001" & (코로나 == 2019 | 코로나 == 2020))


rm(monthweek)

rm(shinhan_temp)

#

install.packages("ggcharts")
library(dplyr)
library(ggplot2)
library(ggcharts)
install.packages("tidytext")
library(tidytext)
data("biomedicalrevenue")

biomedicalrevenue %>%
  filter(year %in% c(2012, 2015, 2018)) %>%
  bar_chart(x = company, y = revenue, facet = year, top_n = 10)

shinhan_covid_pv %>%
  filter(코로나 %in% c(2019, 2020)) %>%
  bar_chart(x = 업종번호, y = `카드이용건수(천건)`, facet = 코로나, top_n = 10)

head(biomedicalrevenue)
head(shinhan_covid)
head(shinhan_covid_pv)
glimpse(shinhan_covid_pv)

shinhan_covid_pv <- dcast(shinhan_covid, 코로나 ~ 업종번호, value.var="카드이용건수(천건)", mean)
rm(biomedicalrevenue)

#

data("revenue_wide")
library(ggcharts)
line_chart(data = shinhan_covid, x = year(일별), y = `카드이용건수(천건)`)

pyramid_chart(data = popch, x = age, y = pop, group = sex)
library(dplyr, warn.conflicts = FALSE)

pyramid_chart(
  data,
  x,
  y,
  group,
  bar_colors = c("#1F77B4", "#FF7F0E"),
  sort = "no",
  xlab = NULL,
  title = NULL
)

#

head(mcorp_nn[2:8], n = 10)

mcorp %>%
  group_by(카테고리명) %>%
  summarise(mean(구매수)) %>%
  
shinhancard %>%
  group_by(성별) %>%
  summarise(mean(`카드이용건수(천건)`)) %>%

glimpse(mcorp)

sort(category_list)
category_list






# 삼성카드 데이터

samsung <- read_xlsx("data/Samsungcard.xlsx")
samsung$소비일자 <- ymd(samsung$소비일자)
head(samsung)



# 신한카드 데이터

shinhan <- read_xlsx("data/Shinhancard.xlsx")
shinhan <- shinhan %>% 
  select(-c(6:8))
shinhan$일별 <- ymd(shinhan$일별)
head(shinhan)



# 엠코퍼레이션 데이터

files <- list.files(path = "data/Mcorporation/상품 카테고리 데이터_KDX 시각화 경진대회 Only/", pattern = "*.xlsx", full.names = TRUE)
mcorp <- sapply(files[2:65], read_excel, simplify=FALSE) %>% 
  bind_rows(.id = "id")
mcorp$구매날짜 <- ymd(mcorp$구매날짜)
mcorp <- mcorp[2:8]
head(mcorp)
rm(files)



# 신한카드(오프라인) 코로나 데이터 열 추가 (2019년 2-4월, 2020년)

shinhan_covid <- shinhan %>% mutate(
  코로나 = case_when(
    일별 >= "2019-02-01" & 일별 < "2019-05-01"  ~ "2019",
    일별 >= "2020-02-01" & 일별 < "2020-05-01"  ~ "2020",
    TRUE ~ "기타"))

shinhan_covid %>%
  group_by(업종, 코로나) %>%
  summarise(평균 = mean(`카드이용건수(천건)`)) %>%
  filter((코로나 == 2019 | 코로나 == 2020) & 업종 == "M001_한식") %>%
  ggplot(aes(x = 업종, y = 평균, fill = 코로나)) +
  geom_col(aes(x = 업종, y = 평균, fill = 코로나), position='dodge')+
  geom_bar(stat="identity", position = "dodge", width=.5) + 
  coord_cartesian(ylim = c(200, 350)) +
  labs(title = "기타요식(오프라인) 카드이용건수가 줄었다.", 
       subtitle = "비교: 2019년 2~4월 vs 2020년 2~4월", 
       caption = "출처: 신한카드")



# 따라하기: [R 데이터 분석,시각화] 한국복지패널 데이터 분석 실습
# https://blog.itpaper.co.kr/rdata-%ED%95%9C%EA%B5%AD%EB%B3%B5%EC%A7%80%ED%8C%A8%EB%84%90/

# 조사대상들에 대한 성별 분포 조사하기

성별추출 <- shinhan %>% select(성별)
head(성별추출)

성별df <- rename(성별추출, 성별코드 = 성별)
head(성별df)

table(성별df$성별코드)

성별df <- 성별df %>% mutate(성별 = ifelse(성별코드 == "M", "남자", "여자"))
head(성별df)

ggplot(data = 성별df) +
  geom_bar(aes(x = 성별, fill = 성별)) +
  # 배경을 흰색으로 설정
  theme_bw() +
  # 그래프 타이틀 설정
  ggtitle("성별 분포") +
  # x축 제목 설정
  xlab("성별") +
  # y축 제목 설정
  ylab("분포수(명)") +
  # y축 간격
  coord_cartesian(ylim = c(97770, 97820)) +
  # 각 텍스트의 색상, 크기, 각도, 글꼴 설정
  theme(plot.title=element_text(color="#0066ff", size=25, face="bold", hjust=0.5),
        axis.title.x=element_text(color="#999999", size=18, face="bold"),
        axis.title.y=element_text(color="#999999", size=18, face="bold"),
        axis.text.x=element_text(color="#000000", size=16, angle=0),
        axis.text.y=element_text(color="#000000", size=16, angle=0)) +
  # 범주 설정
  theme(legend.title = element_blank(),
        legend.text = element_text(face="bold", size=15, color="#330066"),
        legend.key = element_rect(color="red", fill="white"),
        legend.key.size = unit(1,"cm"),
        legend.box.background = element_rect(fill="skyblue"),
        legend.box.margin = margin(6, 6, 6, 6))



# 성별에 따른 평균 카드이용건수 차이 분석

성별카드이용건수추출 <- shinhan %>% select(성별, `카드이용건수(천건)`)
head(성별카드이용건수추출)

성별카드이용건수df <- rename(성별카드이용건수추출, 성별코드 = 성별, 카드이용건수 = `카드이용건수(천건)`)
head(성별카드이용건수df)

성별카드이용건수df <- 성별카드이용건수df %>% mutate(성별 = ifelse(성별코드 == "M", "남자", "여자"))
head(성별카드이용건수df)

colSums(is.na(성별카드이용건수df))
분석결과df <- 성별카드이용건수df %>%
  group_by(성별) %>%
  summarise(평균카드이용건수 = mean(카드이용건수, na.rm = TRUE))
분석결과df

ggplot(data=분석결과df) +
  geom_col(aes(x=성별,y=평균카드이용건수,fill=성별)) +
  # 배경을 흰색으로 설정
  theme_bw() +
  # 그래프 타이틀 설정
  ggtitle("성별 평균 카드이용건수") +
  # x축 제목 설정
  xlab("성별") +
  # y축 제목 설정
  ylab("평균 카드이용건수(천건)") +
  # y축 간격
  scale_y_continuous(breaks=seq(0, 110, 10))



# 나이에 따른 평균 월급의 변화

분석결과df <- shinhan %>%
  group_by(연령대별) %>%
  summarise(평균 = mean(`카드이용건수(천건)`))
head(분석결과df)

options(repr.plot.width=15, repr.plot.height=10, warn=-1)

ggplot(data = 분석결과df) +
  geom_line(aes(x=연령대별, y=평균, group=1), color="#ff6600") +
  theme_bw()






shinhan %>%
  filter(업종 == "M001_한식" | 업종 == "M002일식/중식/양식")

shinhan2 <- shinhan %>%
  filter(업종 == "M001_한식" | 업종 == "M002일식/중식/양식")

shinhan2 %>%
  group_by(연령대별) %>%
  summarise(mean(`카드이용건수(천건)`))

shinhan %>%
  group_by(연령대별) %>%
  summarise(mean(`카드이용건수(천건)`))






#