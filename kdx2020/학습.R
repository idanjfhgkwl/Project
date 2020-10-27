library(readr)
library(tidyverse)
library(dplyr)
library(readxl)
library(jsonlite)
library(ggplot2)

samsung_card <- Temporary2 %>%
  separate(소비일별, c("년월", "일"), sep = 7)

samsung_card <- samsung_card %>%
  unite(소비일자,년 , 일자)

shinhancard <- shinhancard %>%
  select(-4)



products %>%  #이거 시계열 안씀
  rename(date = "구매날짜") %>%  
  group_by(date, 카테고리명) %>% 
  summarise(mean = mean(구매금액)) %>% 
  filter(data <- 카테고리명 =="가공식품"| 카테고리명 =="농축수산물") %>%
  ggplot(aes(x=date)) + 
  geom_smooth(aes(y=mean, colour = 카테고리명), se= F) +
  geom_vline(xintercept = as.Date("2020-02-17",  format = "%Y-%m-%d"),
             linetype = 'dotted', color = 'blue') +
  geom_line(aes(y=mean, colour = 카테고리명)) + 
  labs(title="엠코퍼레이션(온라인)", 
       subtitle="식품관련 인터넷 구매 현황", 
       caption="Source: Mcorporation", 
       y="구매금액", x= "구매기간")


shinhancard %>%  #이거 시계열
  rename(date = "일별") %>%  
  filter(연령대별 %in% c("20대", "30대")) %>% 
  group_by(date, 업종, 연령대별) %>% 
  summarise(mean = mean(`카드이용건수(천건)`)) %>%
  filter(data <- 업종 =="한식"| 업종 =="일식/중식/양식"| 
           업종 =="제과/커피/패스트푸드"| 업종 =="기타요식") %>%
  ggplot(aes(x=date)) + 
  geom_line(aes(y=mean, colour = 업종)) + 
  # facet_grid(연령대별 ~ .) + 
  labs(title="신한카드(오프라인)", 
       subtitle="외식업종 카드 사용현황", 
       caption="Source: shinhancard", 
       y="이용건수", x= "구매기간") + 
  theme_classic() + 
  theme(
    legend.position = "top"
  )


shinhancard %>%    #이거
  group_by(일별, 업종, 연령) %>% head()
  count(`카드이용건수(천건)`) %>%  
  filter(data <- 업종 =="한식"| 업종 =="일식/중식/양식"| 
           업종 =="제과/커피/패스트푸드"| 업종 =="기타요식") %>%
  ggplot(aes(x = 업종, y= `카드이용건수(천건)`, fill= 코로나)) + 
  geom_bar(stat = "identity", position = 'dodge', width=.8) +
  labs(title="신한카드(오프라인)", 
       subtitle="외식업종 전년대비 카드 사용현황[2~4월]", 
       caption="source: shinhancard")






shinhancard %>%  #이거 시계열
  rename(date = "일별") %>%  
  group_by(date, 업종) %>% 
  summarise(mean = mean(`카드이용건수(천건)`)) %>%  
  filter(data <- 업종 =="한식"| 업종 =="일식/중식/양식"| 
           업종 =="제과/커피/패스트푸드"| 업종 =="기타요식") %>%
  ggplot(aes(x=date)) + 
  geom_smooth(aes(y=mean, colour = 업종대), se= F) +
  facet_grid(. ~ 연령대) +
  geom_line(aes(y=mean, colour = 업종)) + 
  labs(title="신한카드(오프라인)", 
       subtitle="외식업종 카드 사용현황", 
       caption="Source: shinhancard", 
       y="이용건수", x= "구매기간")



samsung_card1 %>%  #이거 시계열
  rename(date = "소비일자") %>%  
  group_by(date, 소비업종) %>% 
  summarise(mean = mean(소비건수)) %>% 
  filter(data <- 소비업종 =="요식/유흥") %>%
  ggplot(aes(x=date)) + 
  geom_smooth(aes(y=mean, colour = 소비업종), se= F) +
  geom_line(aes(y=mean, colour = 소비업종)) + 
  labs(title="삼성카드(오프라인)", 
       subtitle="외식업종 카드 사용현황", 
       caption="Source: samsungcard", 
       y="이용건수", x= "구매기간")


samsung_card %>%  #이거 실험
  rename(date = "소비일자") %>%  
  group_by(date, 소비업종) %>% 
  summarise(mean = mean(소비건수)) %>% 
  filter((data <- 소비업종 =="요식/유흥") & (date>="2019-02-01" & date<"2019-05-01")) %>%
  ggplot(aes(x=date)) + 
  geom_smooth(aes(y=mean), se= F) +
  geom_line(aes(y=mean)) + 
  labs(title="삼성카드(오프라인)", 
       subtitle="외식업종 카드 사용현황", 
       caption="Source: samsungcard", 
       y="이용건수", x= "구매기간")


samsungcard %>%  #이거 실험
  rename(date = "소비일자") %>%  
  group_by(date, 소비업종) %>% 
  summarise(mean = mean(소비건수)) %>% 
  filter(data <- 소비업종 =="요식/유흥") %>%
  ggplot(aes(x=date)) + 
  geom_smooth(aes(y=mean), se= F) +
  geom_vline(xintercept = as.Date("2020-02-17",  format = "%Y-%m-%d"),
             linetype = 'dotted') +
  geom_line(aes(y=mean)) + 
  labs(title="삼성카드(오프라인)", 
       subtitle="외식업종 카드 사용현황", 
       caption="Source: samsungcard", 
       y="이용건수", x= "구매기간")

  
  

shinhancard %>%    #이거
  group_by(일별, 업종, 코로나) %>%
  count(`카드이용건수(천건)`) %>%
  filter(data <- 업종 =="한식"| 업종 =="일식/중식/양식"| 
           업종 =="제과/커피/패스트푸드"| 업종 =="기타요식") %>%
  ggplot(aes(x = 업종, y= `카드이용건수(천건)`, fill= 코로나)) + 
  geom_bar(stat = "identity", position = 'dodge', width=.8) +
  labs(title="신한카드(오프라인)", 
       subtitle="외식업종 전년대비 카드 사용현황[2~4월]", 
       caption="source: shinhancard")


shinhancard %>%    #이거 바형
  group_by(업종, 일별, 코로나, 성별) %>%
  count(`카드이용건수(천건)`) %>%
  filter(data <- 업종 =="한식"| 업종 =="일식/중식/양식"| 
           업종 =="제과/커피/패스트푸드"| 업종 =="기타요식") %>%
  ggplot(aes(x = 업종, y= `카드이용건수(천건)`, fill= 코로나)) + 
  geom_bar(stat = "identity", position= 'dodge', width=.8) +
  facet_grid(. ~ 성별) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="신한카드(오프라인)", 
       subtitle="외식업종 전년대비 카드 사용현황(2~4월)", 
       caption="source: shinhancard")
  

samsung_card %>%    #이거 바형
  group_by(소비일자, 소비업종, 코로나, 성별) %>%
  count(소비건수) %>%
  filter(data <- 소비업종 =="요식/유흥") %>%
  ggplot(aes(x = 소비업종, y= 소비건수, fill= 코로나)) + 
  facet_grid(. ~ 성별) +
  geom_bar(stat = "identity", position = 'dodge', width=.8) +
  labs(title="삼성카드(오프라인)", 
       subtitle="외식업 전년대비 카드 사용현황(2~4월)", 
       caption="source: samsuncard")



samsungcard1 %>%   
  rename(date = "소비일자") %>% 
  group_by(date, 소비업종) %>% 
  summarise(mean = mean(소비건수)) %>% 
  filter(pro <- 소비업종 == "요식/유흥") %>%
  ggplot(aes(x=date)) + 
  geom_line(aes(y=mean),group=1) + 
  labs(title="삼성카드", 
       subtitle="농축수산물 온라인 판매 현황", 
       caption="Source:Mcorporation", 
       y="온라인 판매수량") 



shinhancard %>% 
  rename(date = "일별") %>% 
  group_by(date, 카테고리명) %>% 
  summarise(mean = mean(`카드이용건수(천건)`)) %>% 
  ggplot(aes(x=date)) + 
  geom_line(aes(y=mean)) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Time Series Chart", 
       subtitle="Avg. Sales from 'Shinhan Card' Dataset", 
       caption="Source: Shinhan Card", 
       y="Avg. Sales (1000)") + 
  theme_bw()




