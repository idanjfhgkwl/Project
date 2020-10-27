# kdx_guideline https://rpubs.com/Evan_Jung/672281
# 패키지 불러오기
install.packages(c("tidyverse", "readxl"))
library(tidyverse) # 데이터 가공 및 시각화
library(readxl) # 엑셀파일 불러오기 패키지 

# 데이터 불러오기

# 삼성카드 데이터
readr::guess_encoding("data/Samsungcard.csv", n_max = 100)
samsung_card <- read_xlsx("data/Samsungcard.xlsx")
samsung_card2 <- read.csv("data/Samsungcard.csv", fileEncoding = "EUC-KR")
head(samsung_card)
head(samsung_card2)
rm(samsung_card2) # 객체 지우는 함수
ls() # 현재 저장된 객체 확인하는 함수

# 신한카드 데이터
shinhancard <- read_xlsx("data/Shinhancard.xlsx")
head(shinhancard)
shinhancard <- shinhancard %>% 
  select(-c(6:8))
head(shinhancard)

# 지인플러스
gin_8a <- read_csv("data/GIN00008A.csv")
gin_9a <- read_csv("data/GIN00009A.csv")
glimpse(gin_8a)
glimpse(gin_9a)

# JSON 파일 불러오기
library(jsonlite) # tidyverse::purrr
GIN_10m <- fromJSON("data/center_GIN00010M.json")
glimpse(GIN_10m)

# SSC_Data
readr::guess_encoding("data/Mcorporation/KDX시각화경진대회_SSC_DATA.csv")
ssc_data <- read_csv("data/Mcorporation/KDX시각화경진대회_SSC_DATA.csv", locale = locale("ko", encoding = "EUC-KR"))
glimpse(ssc_data)

# 다중 엑셀파일 불러오기
list.files(path = "data/Mcorporation/상품 카테고리 데이터_KDX 시각화 경진대회 Only/")

# Multiple Excel Files import in R
# KDX_CONTEST_파일정의서.xlsx : 파일 제외
# 참고자료 https://stackoverflow.com/questions/32888757/how-can-i-read-multiple-excel-files-into-r

files <- list.files(path = "data/Mcorporation/상품 카테고리 데이터_KDX 시각화 경진대회 Only/", pattern = "*.xlsx", full.names = TRUE)

# full.names: a logical value. If TRUE, the directory path is prepended to the file names to give a relative file path. If FALSE, the file names (rather than paths) are returned. 논리 값. TRUE인 경우 디렉토리 경로가 파일 이름 앞에 추가되어 상대 파일 경로를 제공합니다. FALSE인 경우 파일 이름 (경로가 아님)이 반환됩니다.

head(files)

products <- sapply(files[2:65], read_excel, simplify=FALSE) %>% 
  bind_rows(.id = "id")

rm(products22)

# Apply a Function over a List or Vector
# sapply is a user-friendly version and wrapper of lapply by default returning a vector, matrix or, if simplify = "array", an array if appropriate, by applying simplify2array(). sapply(x, f, simplify = FALSE, USE.NAMES = FALSE) is the same as lapply(x, f).

# Efficiently bind multiple data frames by row and column
# This is an efficient implementation of the common pattern of do.call(rbind, dfs) or do.call(cbind, dfs) for binding many data frames into one.
# .id: Data frame identifier. When .id is supplied, a new column of identifiers is created to link each row to its original data frame. The labels are taken from the named arguments to bind_rows(). When a list of data frames is supplied, the labels are taken from the names of the list. If no names are found a numeric sequence is used instead.

glimpse(products)

# 데이터 시각화

# 수치형 변수 ~ 수치형 변수

temp_products <- sample_n(products, 1000)
g <- ggplot(temp_products, aes(x = 구매수, y = 구매금액))
g + geom_count(col="tomato3", show.legend=F) +
  labs(subtitle="products: count vs revenue ", 
       y="revenue", 
       x="count", 
       title="Counts Plot")

g + 
  geom_count(col="tomato3", show.legend=F) + 
  scale_x_log10() + 
  scale_y_log10() + 
  labs(subtitle="products: count vs revenue ", 
       y="revenue", 
       x="count", 
       title="Counts Plot")

# 범주형 변수 ~ 수치형 변수

cat_rev <- products %>%
  group_by(카테고리명) %>% 
  summarise(mean = mean(구매금액)) %>% 
  arrange(desc(mean)) 

cat_rev <- cat_rev[order(cat_rev$mean, decreasing = TRUE), ]  # sort
cat_rev$카테고리명 <- factor(cat_rev$카테고리명, levels = cat_rev$카테고리명)  # to retain the order in plot.
head(cat_rev)

ggplot(cat_rev %>% head(20), aes(x = 카테고리명, y = mean)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="상위 5개의 카테고리가 특히 구매금액이 높다.", 
       subtitle="카테고리명 Vs Avg. 구매금액", 
       caption="source: products")

category_list <- cat_rev$카테고리명
category_list

# 이번에는 카테고리명 ~ 평균 구매건수에 관한 시각화를 작성해본다.

cat_rev2 <- products %>%
  group_by(카테고리명) %>% 
  summarise(mean = mean(구매수)) %>% 
  arrange(desc(mean)) 

cat_rev2 <- cat_rev2[order(cat_rev2$mean, decreasing = TRUE), ]  # sort
cat_rev2$카테고리명 <- factor(cat_rev2$카테고리명, levels = cat_rev2$카테고리명)  # to retain the order in plot.
head(cat_rev2)

ggplot(cat_rev2 %>% head(20), aes(x = 카테고리명, y = mean)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="상위 3개의 구매건수가 높고, 서비스/티켓은 특히 더 높다.", 
       subtitle="카테고리명 Vs Avg. 구매수", 
       caption="source: products")

# 위 두 그래프 확장하기기

# 성별을 기준으로 산점도를 작성

temp_products <- sample_n(products, 1000)
g <- ggplot(temp_products, aes(x = 구매수, y = 구매금액))
# g + geom_count(col="tomato3", show.legend=F) +
g + geom_point(aes(colour = 고객성별)) + 
  scale_x_log10() + 
  scale_y_log10() + 
  labs(subtitle="products: count vs revenue ", 
       y="revenue", 
       x="count", 
       title="Counts Plot")

g <- ggplot(temp_products, aes(x = 구매수, y = 구매금액))
# g + geom_count(col="tomato3", show.legend=F) +
g + geom_point(col="tomato3") + 
  scale_x_log10() + 
  scale_y_log10() + 
  facet_grid(고객성별 ~.) + 
  labs(subtitle="products: count vs revenue ", 
       y="revenue", 
       x="count", 
       title="Counts Plot")

gender_products <- products %>% 
  filter(고객성별 != "없음")

dim(products); dim(gender_products)

temp_products <- sample_n(gender_products, 1000)
g <- ggplot(temp_products, aes(x = 구매수, y = 구매금액))
# g + geom_count(col="tomato3", show.legend=F) +
g + geom_point(col="tomato3") + 
  scale_x_log10() + 
  scale_y_log10() + 
  facet_grid(고객성별 ~.) + 
  labs(subtitle="products: count vs revenue ", 
       y="revenue", 
       x="count", 
       title="Counts Plot")

cat_rev <- products %>%
  group_by(카테고리명, 고객성별) %>% 
  summarise(mean = mean(구매금액)) %>% 
  arrange(desc(mean)) 

glimpse(cat_rev)

cat_rev <- cat_rev[order(cat_rev$mean, decreasing = TRUE), ]  # sort
cat_rev

cat_rev2 <- cat_rev %>% 
  filter(고객성별 != "없음")
cat_rev2

ggplot(cat_rev2, aes(x = 카테고리명, y = mean, fill = 고객성별)) +
  geom_bar(stat="identity", position = "dodge", width=.5)

i = 1
j = i+7
category_list <- as.character(category_list)

cat_rev2 %>% 
  filter(카테고리명 %in% category_list[c(i:as.numeric(i+7))]) %>% 
  ggplot(aes(x = 카테고리명, y = mean, fill = 고객성별)) +
  geom_bar(stat="identity", position = "dodge", width=.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Ordered Bar Chart", 
       subtitle="카테고리명 Vs Avg. 구매금액 by 고객성별", 
       caption="source: products")

# 가나다 순으로 카테고리별 시각화를 구현한 뒤, 고객성별로 비교할 수 있다.

category_graph <- function(df, cat_list = cat_list) {
  category_list <- sort(as.character(cat_list))
  
  print(category_list)
  for (i in seq(1, 64, by = 8)) {
    i = i
    j = i+7
    plot <- df %>% 
      filter(카테고리명 %in% category_list[i:j]) %>% 
      ggplot(aes(x = 카테고리명, y = mean, fill = 고객성별)) +
      geom_bar(stat="identity", position = "dodge", width=.5) + 
      theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
      labs(title="Ordered Bar Chart", 
           subtitle="카테고리명 Vs Avg. 구매금액 by 고객성별", 
           caption="source: products")
    
    print(plot)
  }
}

category_graph(cat_rev2, category_list)

# 카테고리, 고객성별 대신에, 고객나이, OS유형 등을 조합해서 시각화를 작성한 뒤 비교

class(economics$date)
class(products$구매날짜)

temp_date <- "20200110"
conv_date <- as.Date(temp_date, format = "%Y%m%d")
print(conv_date)
class(conv_date)

shinhancard$일별 <- as.Date(shinhancard$일별, format = "%Y%m%d")
glimpse(shinhancard)

# Allow Default X Axis Labels
ggplot(shinhancard, aes(x=일별)) + 
  geom_line(aes(y=`카드이용건수(천건)`)) + 
  labs(title="Time Series Chart", 
       subtitle="Returns Percentage from 'Economics' Dataset", 
       caption="Source: Economics", 
       y="Returns %")

shinhancard %>% 
  filter(일별 == "2019-01-01") %>% 
  dim()

ggplot(shinhancard, aes(x=일별)) + 
  geom_line(aes(y=`카드이용건수(천건)`)) + 
  labs(title="Time Series Chart", 
       subtitle="Returns Percentage from 'Economics' Dataset", 
       caption="Source: Economics", 
       y="Returns %")

shinhancard %>% 
  rename(date = "일별") %>% 
  group_by(date) %>% 
  summarise(mean = mean(`카드이용건수(천건)`)) %>% 
  ggplot(aes(x=date)) + 
  geom_line(aes(y=mean)) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Time Series Chart", 
       subtitle="Avg. Sales from 'Shinhan Card' Dataset", 
       caption="Source: Shinhan Card", 
       y="Avg. Sales (1000)") + 
  theme_bw()

shinhancard %>% 
  rename(date = "일별", gender = "성별") %>%    
  group_by(date, gender) %>% 
  summarise(mean = mean(`카드이용건수(천건)`)) %>% 
  ggplot(aes(x=date)) + 
  geom_line(aes(y=mean, colour = gender)) + 
  labs(title="Time Series Chart", 
       subtitle="Avg. Sales from 'Shinhan Card' Dataset", 
       caption="Source: Shinhan Card", 
       y="Avg. Sales (1000)")

shinhancard %>% 
  rename(date = "일별") %>%  
  group_by(date, 연령대별) %>% 
  summarise(mean = mean(`카드이용건수(천건)`)) %>% 
  ggplot(aes(x=date)) + 
  geom_line(aes(y=mean, colour = 연령대별)) + 
  labs(title="Time Series Chart", 
       subtitle="Avg. Sales from 'Shinhan Card' Dataset", 
       caption="Source: Shinhan Card", 
       y="Avg. Sales (1000)")




