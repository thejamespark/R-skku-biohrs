library(data.table)
library(magrittr)
library(ggplot2)
library(ggpubr)

# Problem 1
a <- read.csv("https://raw.githubusercontent.com/jinseob2kim/R-skku-biohrs/main/data/example_g1e.csv")

var.factor <- grep("Q_", names(a), value = T)
for (v in var.factor){
  a[[v]] <- factor(a[[v]])
}

a[, (var.factor) := lapply(.SD, factor), .SD = var.factor]

sapply(a, class)


# Problem 2
a[, .SD, .SDcols = -c("RN_INDI", "HMER_YYYYMM", var.factor)][, lapply(.SD, mean, na.rm = T), keyby = "EXMD_BZ_YYYY"]


# Problem 3
ggboxplot(a, "EXMD_BZ_YYYY", "FBS")

#################################
library(survival)

head(colon)

cor.test(colon$age, colon$time)

lm(time ~ age, data = colon) %>% summary %>% .$coefficients
lm(age ~ time, data = colon) %>% summary %>% .$coefficients

# 1대1 비교일 땐, 회귀분석과 t.test는 p-value가 똑같다. 
# 즉, 같은 분석이다.따라서 1대1 분석은 회귀분석을 할 필요없이 t-test를 하면된다.
lm(time ~ sex, data = colon) %>% summary %>% .$coefficients
t.test(time ~ sex, data = colon, var.equal = T)

# 3범주 이상
lm(time ~ rx, colon) %>% summary %>% .$coefficients

# 교수님이 만든 분석 패키지
#install.packages("jstable")
library(jstable)

glm_gaussian <- glm(status ~ sex + age + rx, data = colon, family = binomial)
glm_gaussian
glmshow.display(glm_gaussian, decimal = 2)$table
# OR. : Odds Ratio

# Stepwise selection <- 사용하지 마라(임상적 중요성 무시 & 통계학적 논리도 부족)
glm_gaussian1 <- glm(status ~ ., data = colon, family = binomial)
step(glm_gaussian1)

glm(status ~ sex + age + rx, data = colon, family = binomial)
# 성별 보정 대신 성별에 따른 subgroup 분석을 해라
# 위의 수식에서 sex를 아래와 같이 subgroup으로 나누고 분석 실행
glm(status ~ age + rx, data = subset(colon, sex == 0), family = binomial)
glm(status ~ age + rx, data = subset(colon, sex == 1), family = binomial)

# ?
glm(status ~ sex*age + rx, data = subset(colon, sex == 1), family = binomial)

# Cox
km <- survfit(Surv(time, status) ~ rx, data = colon)
km
plot(km)

# survminer r package
#install.packages("survminer")
library(survminer)

# 교수님이 개발한 패키지
#install.packages("jskm")
library(jskm)
jskm(km, pval = T, marks = F, table = T, surv.scale = "percent", cumhaz = T, ylab = "Cumulative incidence")

km %>% summary

# 비례위험가정 확인
# Goodness of fit
cox.zph(km)

# Logrank test
# 아노바처럼 해석하면 된다

# Cox model
cmodel <- coxph(Surv(time, status) ~ age + sex + rx, data = colon, model = T)
cmodel
cox2.display(cmodel)
cox2.display(cmodel)$table

s