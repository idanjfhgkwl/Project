REPO_URL <- "https://cran.seoul.go.kr/"
if (!require(foreign)) install.packages("foreign", repos=REPO_URL)
if (!require(dplyr)) install.packages("dplyr", repos=REPO_URL)
if (!require(ggplot2)) install.packages("ggplot2", repos=REPO_URL)
if (!require("reshape2"))     install.packages("reshape2", repos=REPO_URL)

library(foreign)  # SPSS용 데이터 파일을 읽어들일 수 있는 기능을 제공한다.
library(dplyr)
library(ggplot2)
library(reshape2) # 피벗테이블 구성을 위한 패키지

koweps_raw <- read.spss(file="http://itpaper.co.kr/demo/r/Koweps_hpda12_2017_beta1.sav", to.data.frame=TRUE)

# 데이터가 매우 크므로 상위 6건만 확인
head(koweps_raw, n = 1)

sex_extn <- koweps_raw %>%
  select('h12_g3')
head(sex_extn)

sex_df <- rename(sex_extn, 성별코드 = h12_g3)
head(sex_df)

table(sex_df$성별코드)

sex_df <- sex_df %>%
  mutate(성별 = ifelse(성별코드 == 1, '남자', '여자'))
head(sex_df)

options(repr.plot.width=15, repr.plot.height=10, warn=-1)

ggplot(data=sex_df) +
  geom_bar(aes(x=성별, fill=성별)) +
  # 배경을 흰색으로 설정
  theme_bw() +
  # 그래프 타이틀 설정
  ggtitle("성별 분포") +
  # x축 제목 설정
  xlab("성별") +
  # y축 제목 설정
  ylab("분포수(명)") +
  # y축 간격
  scale_y_continuous(breaks=seq(0, 10000, 1000))  +
  # 각 텍스트의 색상, 크기, 각도, 글꼴 설정
  theme(plot.title=element_text(family="NanumGothic", color="#0066ff", size=25, face="bold", hjust=0.5),
        axis.title.x=element_text(family="NanumGothic", color="#999999", size=18, face="bold"),
        axis.title.y=element_text(family="NanumGothic", color="#999999", size=18, face="bold"),
        axis.text.x=element_text(family="NanumGothic", color="#000000", size=16, angle=0),
        axis.text.y=element_text(family="NanumGothic", color="#000000", size=16, angle=0)) +
  # 범주 설정
  theme(legend.title = element_blank(),
        legend.text = element_text(family="NanumGothic", face="bold", size=15, color="#330066"),
        legend.key = element_rect(color="red", fill="white"),
        legend.key.size = unit(1,"cm"),
        legend.box.background = element_rect(fill="skyblue"),
        legend.box.margin = margin(6, 6, 6, 6))

sex_salary_extn <- koweps_raw %>%
  select('h12_g3', 'p1202_8aq1')
head(sex_salary_extn)

sex_salary_df <- rename(sex_salary_extn, 성별코드=h12_g3, 월급=p1202_8aq1)
head(sex_salary_df)

sex_salary_df <- sex_salary_df %>%
  mutate(성별 = ifelse(성별코드 == 1, '남자', '여자'))
head(sex_salary_df)

colSums(is.na(sex_salary_df))
refine_df <- sex_salary_df %>%
  filter(!is.na(월급))
head(refine_df)
colSums(is.na(refine_df))

refine_df <- refine_df %>%
  mutate(월급 = ifelse(월급 %in% c(0, 9999), NA, 월급))
head(refine_df)
colSums(is.na(refine_df))

sex_salary_compl_df <- refine_df %>%
  filter(!is.na(월급))
head(sex_salary_compl_df)
colSums(is.na(sex_salary_compl_df))

analysis_res_df <- sex_salary_compl_df %>%
  group_by(성별) %>%
  summarise(평균월급 = mean(월급, na.rm=TRUE))

analysis_res_df

ggplot(data=analysis_res_df) +
  geom_col(aes(x=성별,y=평균월급,fill=성별)) +
  # 배경을 흰색으로 설정
  theme_bw() +
  # 그래프 타이틀 설정
  ggtitle("성별 평균 월급") +
  # x축 제목 설정
  xlab("성별") +
  # y축 제목 설정
  ylab("평균월급(만원)") +
  # y축 간격
  scale_y_continuous(breaks=seq(0, 350, 50))  +
  # 각 텍스트의 색상, 크기, 각도, 글꼴 설정
  theme(plot.title=element_text(family="NanumGothic", color="#0066ff", size=25, face="bold", hjust=0.5),
        axis.title.x=element_text(family="NanumGothic", color="#999999", size=18, face="bold"),
        axis.title.y=element_text(family="NanumGothic", color="#999999", size=18, face="bold"),
        axis.text.x=element_text(family="NanumGothic", color="#000000", size=16, angle=0),
        axis.text.y=element_text(family="NanumGothic", color="#000000", size=16, angle=0)) +
  # 범주 설정
  theme(legend.title = element_blank(),
        legend.text = element_text(family="NanumGothic", face="bold", size=15, color="#330066"),
        legend.key = element_rect(color="red", fill="white"),
        legend.key.size = unit(1,"cm"),
        legend.box.background = element_rect(fill="skyblue"),
        legend.box.margin = margin(6, 6, 6, 6))










