# 신한카드 데이터

head(shinhancard)
glimpse(shinhancard)

# R에서 깔끔하게 날짜·시간 데이터 처리하기(feat. lubridate)

shinhancard$일별 <- ymd(shinhancard$일별)
glimpse(shinhancard)

#

cat_rev <- shinhancard %>%
  group_by(업종) %>% 
  summarise(mean = mean(`카드이용건수(천건)`)) %>% 
  arrange(desc(mean))

head(cat_rev)

cat_rev <- cat_rev[order(cat_rev$mean, decreasing = TRUE), ]  # sort
cat_rev$업종 <- factor(cat_rev$업종, levels = cat_rev$업종)  # to retain the order in plot.
head(cat_rev)

category_list <- cat_rev$업종
category_list

#

ggplot(cat_rev %>% head(20), aes(x = 업종, y = mean)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Ordered Bar Chart", 
       subtitle="카테고리명 Vs Avg. 구매금액", 
       caption="source: products")

# 

shinhancard %>% 
  rename(date = "일별", gender = "성별", category = "업종") %>%    
  group_by(date, category) %>% 
  summarise(mean = mean(`카드이용건수(천건)`)) %>% 
  filter(date > "2020-02-17" & category == "M010_음/식료품") %>%
  ggplot(aes(x=date)) + 
  geom_line(aes(y=mean, colour = category)) + 
  geom_smooth(aes(y=mean, colour = category)) +
  #coord_cartesian(ylim = c(100, 400)) +
  labs(
    title="오프라인 한식 이용건수 (코로나 후)",
    caption="출처: 신한카드",
    y="평균 카드이용건수"
  )

temp_shinhan <- shinhancard %>%
  transmute(
    bcday <- day <= "2020-02-17",
    acday <- day >= "2020-02-18"
  )

newdf <- shinhancard[shinhancard$일별 >= "2019-02-01" & shinhancard$일별 < "2019-05-01",]
rm(newdf)

# 

temp <- shinhancard %>%
  summarise(beco = )

cat_rev <- shinhancard %>%
  group_by(일별, 업종, 성별) %>% 
  summarise(mean = mean(`카드이용건수(천건)`)) %>% 
  filter(업종 == "M010_음/식료품")
  arrange(desc(mean)) 

glimpse(cat_rev)

cat_rev <- cat_rev[order(cat_rev$mean, decreasing = TRUE), ]  # sort

ggplot(cat_rev, aes(x = 성별, y = mean, fill = 업종)) +
  geom_bar(stat="identity", position = "dodge", width=.5)

cat_rev %>% 
  ggplot(aes(x = 업종, y = mean, fill = 성별)) +
  geom_bar(stat="identity", position = "dodge", width=.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Ordered Bar Chart", 
       subtitle="카테고리명 Vs Avg. 구매금액 by 고객성별", 
       caption="source: products")

#

cat_rev <- shinhancard %>%
  group_by(업종, 일별) %>% 
  summarise(mean = mean(`카드이용건수(천건)`)) %>% 
  arrange(desc(mean)) 

substr(shinhancard$일별, 1, 7)
shinhan_month <- cbind(shinhancard, month = substr(shinhancard$일별, 1, 7))
head(shinhan_month)
glimpse(shinhan_month)

dcast(shinhan_month, month ~ ., value.var = "카드이용건수(천건)", mean)

#

shinhan_covid <- dcast(shinhan_month, month ~ 업종, value.var = "카드이용건수(천건)", mean)
head(shinhan_covid)

# [R 데이터 분석,시각화] 한국복지패널 데이터 분석 실습 무작정 따라하기
# https://blog.itpaper.co.kr/rdata-%ED%95%9C%EA%B5%AD%EB%B3%B5%EC%A7%80%ED%8C%A8%EB%84%90/

# 연령층 구분

colSums(is.na(y))
shinhan_covid <- shinhan_month %>% mutate(
  코로나 = case_when(
    일별 >= "2019-02-01" & 일별 < "2019-05-01"  ~ "2019",
    일별 >= "2020-02-01" & 일별 < "2020-05-01"  ~ "2020",
    TRUE ~ "기타"))

# 지역과 연령층에 대한 그룹분석

#shinhan_covid_group = shinhan_covid %>%
#  group_by(month, 업종, 코로나) %>%
#  #summarise(평균 = mean(`카드이용건수(천건)`)) %>%
#  filter((코로나 == 2019 | 코로나 == 2020))

# 분석결과를 피벗테이블로 구성

shinhan_covid_group_pv <- dcast(shinhan_covid_group, 코로나 ~ 업종, value.var="카드이용건수(천건)", mean)
rm(shinhan_covid_group_pv)

# 각 지역별 연령층 분포 비교

shinhan_covid %>%
  group_by(월별, 업종번호, 업종명, 코로나) %>%
  summarise(평균 = mean(`카드이용건수(천건)`)) %>%
  #filter(업종 == "M001_한식" | 업종 == "M002_일식/중식/양식" | 업종 == "M003_제과/커피/패스트푸드" | 업종 == "M004_기타요식") %>%
  filter((코로나 == 2019 | 코로나 == 2020) & 업종번호 == "M004") %>%
  ggplot(aes(x = 업종명, y = 평균, fill = 코로나)) +
  geom_col(aes(x = 업종명, y = 평균, fill = 코로나), position='dodge')+
  geom_bar(stat="identity", position = "dodge", width=.5) + 
  #geom_text(aes(label = 평균),
  #          position = position_dodge(width=1.8),
  #          vjust=-0.5) +
  #theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  coord_cartesian(ylim = c(200, 350)) +
  labs(title = "기타요식(오프라인) 카드이용건수가 줄었다.", 
       subtitle = "비교: 2019년 2~4월 vs 2020년 2~4월", 
       caption = "출처: 신한카드")

# 29. R의 시각화(그래프) 기능(11) - ggplot2 사용법(기타 : 범례, 레이블, 텍스트 추가 등) https://blog.naver.com/definitice/221162502291
# 최대한 친절하게 쓴 R로 그래프 그리기(feat. ggplot2) https://kuduz.tistory.com/1077

# 연령별, 성별도 구분해보자.

shinhan_cat <- shinhan_month %>%
  separate(업종, into = c("업종번호", "업종명"), sep = "_")

shinhan_covid <- shinhan_cat %>% mutate(
  코로나 = case_when(
    일별 >= "2019-02-01" & 일별 < "2019-05-01"  ~ "2019",
    일별 >= "2020-02-01" & 일별 < "2020-05-01"  ~ "2020",
    TRUE ~ "기타"))

head(shinhan_covid)
rm(shinhan_month)

shinhan_covid <- shinhan_covid %>%
  rename("월별" = month)

shinhan_covid %>%  
  group_by(일별, 연령대별) %>% 
  summarise(평균 = mean(`카드이용건수(천건)`)) %>% 
  ggplot(aes(x=일별)) + 
  geom_line(aes(y=평균, colour = 연령대별)) + 
  labs(title="Time Series Chart", 
       subtitle="Avg. Sales from 'Shinhan Card' Dataset", 
       caption="Source: Shinhan Card", 
       y="Avg. Sales (1000)")

# 변형

shinhan_covid %>%  
  group_by(일별, 성별, 연령대별, 업종번호, 월별, 코로나) %>%
  filter() %>%
  summarise(평균 = mean(`카드이용건수(천건)`)) %>% 
  ggplot(aes(x=일별)) + 
  geom_line(aes(y=평균, colour = 연령대별)) + 
  labs(title="Time Series Chart", 
       subtitle="Avg. Sales from 'Shinhan Card' Dataset", 
       caption="Source: Shinhan Card", 
       y="Avg. Sales (1000)")

# 






