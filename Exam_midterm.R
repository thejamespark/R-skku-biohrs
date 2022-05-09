## 과목: R을 이용한 빅데이터 분석 - 중간고사
## 일시: 2022년 04월 11일 (월)
## 작성자: 박성준, 2022711431

# 데이터 불러오기
data_og <- read.csv("https://raw.githubusercontent.com/jinseob2kim/R-skku-biohrs/main/data/example_g1e.csv")

# 데이터 살펴보기
head(data_og)
names(data_og)
length(names(data_og))
str(data_og)
dim(data_og)
summary(data_og)


## Problem 1:  "Q_" 로 시작하는 변수는 범주형(factor)으로, 나머지 변수는 숫자로 만들어라.

# 문제에 사용할 데이터 복사하기
data <- data_og

# "Q_"로 시작하는 변수에 대해 범주형 변수로 만들기
vars_cat <- c(grep("Q_", names(data), value = T))
vars_cat
length(vars_cat)

for (v in vars_cat){
  data[, v] <- as.factor(data[, v])
}

# 변경 여부 확인
str(data)
summary(data)

# 나머지 변수에 대해 숫자형 변수로 만들기
vars_cont <- names(data)[!(names(data) %in% vars_cat)]
vars_cont
length(vars_cont)

for (v in vars_cont){
  data[, v] <- as.numeric(data[, v])
}

# 변경 여부 확인
str(data)
summary(data)


## Problem 2: RN_INDI, HME_YYYYMM 제외한 모든 연속변수의 연도별 평균과 표준편차를 구하라.

library(magrittr)
library(dplyr)

head(data)

data %>% 
  select(vars_cont, -RN_INDI, -HME_YYYYMM) %>%
  group_by(EXMD_BZ_YYYY) %>% 
  summarize_all(list(mean = mean, sd = sd))


## Problem 3: 연도별 FBS의 Boxplot을 그리고 pptx로 저장하라.

library(ggplot2)
library(rvg)
library(officer)

head(data)

# 연도별 FBS의 Boxplot 그리기
plot1 <- ggplot(data = data, aes(x = factor(EXMD_BZ_YYYY), y = FBS)) +
  geom_boxplot() +
  ggtitle("FBS by year") +
  xlab("Year")

plot1

# 위에서 그린 Boxplot을 pptx로 저장하기기
plot_file <- read_pptx() %>% 
  add_slide() %>% ph_with(dml(ggobj = plot1), location = ph_location_type(type = "body"))

print(plot_file, target = "2022711431_plot_file.pptx")
