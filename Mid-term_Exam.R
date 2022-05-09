## R을 이용한 빅데이터 분석 - 중간고사 (2022-04-11)
## 2022711431, 박성준

library(magrittr)
library(dplyr)

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

# 데이터 복사하기
data <- data_og

# 범주형 변수로 만들기
vars_cat <- c(grep("Q_", names(data), value = T))
vars_cat
length(vars_cat)

for (v in vars_cat){
  data[, v] <- as.factor(data[, v])
}

# 확인
str(data)
summary(data)

# 숫자형 변수로 만들기
vars_cont <- names(data)[!(names(data) %in% vars_cat)]
vars_cont
length(vars_cont)

for (v in vars_cont){
  data[, v] <- as.numeric(data[, v])
}

# 확인
str(data)
summary(data)


## Problem 2: RN_INDI, HME_YYYYMM 제외한 모든 연속변수의 연도별 평균과 표준편차를 구하라.
data %>% 
  select(vars_cont, -RN_INDI, -HME_YYYYMM) %>%
  group_by(EXMD_BZ_YYYY) %>% 
  summarize_all(list(mean = mean, sd = sd))


## Problem 3: 연도별 FBS의 Boxplot을 그리고 pptx로 저장하라.
library(ggplot2)
library(rvg)
library(officer)

head(data)
plot1 <- ggplot(data = data, aes(x = factor(EXMD_BZ_YYYY), y = FBS)) +
  geom_boxplot() +
  ggtitle("FBS by year") +
  xlab("Year")

plot1

plot_file <- read_pptx() %>% 
  add_slide() %>% ph_with(dml(ggobj = plot1), location = ph_location_type(type = "body"))

print(plot_file, target = "plot_file_2022711431.pptx")

