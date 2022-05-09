##################
# 1. R Base 
##################

# Basic Operation
x <- c(1, 2, 3, 4, 5, 6)
y <- c(7, 8, 9, 10, 11, 12)
z <- c(13, 14, 16, 19, 23)

x + y
x * y
sqrt(x)
sum(x)
diff(z)
mean(x)
var(x)
sd(x)
median(x)
IQR(x)
max(x)
which.max(y)
max(y)
max(x, y)
max(x, z)
length(z)

y[2]
y[-2]
x[1:3]
y[1:3]
y[c(1, 2, 3)]
y[c(1, 3, 4, 5, 6)]

x >= 4
sum(x >= 4)
x[x >= 4]
y >= 10
y[y >= 10]
sum(y>=10)
sum(x[x>=4])
x %in% c(1, 2, 3)
x[x %in% c(1, 2, 3)]
y %in% c(1, 7, 9)

# Vector
v1 <- seq(-5, 5, by = .2); v1
v2 <- rep(3, 10); v2
v3 <- rep(c(1, 2, 3), 3); v3
v4 <- rep(c(1, 2, 3), each = 5); v4

# for, if/else, else if, ifelse
for (i in 1:3){
  print(i)
}

i <- 0
for (j in c(1, 2, 4, 5, 6)){
  i <- i + j
  print(i)
}

x <- 5
if (x >= 3){
  x <- x +3
}; x

x <- 2
if (x >= 10){
  print("High")
} else if (x >= 5){
  print("Medium")
} else{
  print("Low")
}

x <- 1:6
y <- ifelse(x >= 4, "yes", "no"); y

# Function
# data missing
x <- c(1:10, 12, 13, NA, NA, 15, 17)
mean(x)

# creating function
mean_narm <- function(x){
  mean(x, na.rm = T)
}

mean_narm(x)

two_mean <- function(x1, x2){
  a <- (x1 + x2)/2
  a
}
two_mean(4, 6)

# Apply - apply, sapply, lapply
mat <- matrix(1:20, nrow = 4, byrow = T); mat
out <- NULL
for (i in 1:nrow(mat)){
  out <- c(out, mean(mat[i, ]))
}; out

# sapply, lapply
sapply(1:nrow(mat), function(x){mean(mat[x, ])})
lapply(1:nrow(mat), function(x){mean(mat[x, ])})
la <- lapply(1:nrow(mat), function(x){mean(mat[x, ])})
unlist(la)

# apply
apply(mat, 1, mean)
rowMeans(mat)
rowSums(mat)

apply(mat, 2, mean)
colMeans(mat)
colSums(mat)

# Exercise 1
x <- 1:6
y <- 7:12

sapply(list(x, y), max)
lapply(list(x, y), max)

# Check & Change Working Directory
getwd()
setwd("data")
getwd()

# Read CSV file
ex_og <- read.csv("example_g1e.csv")
ex <- ex_og
head(ex)

# Read Excel, SAS, SPSS files
#install.packages(c("readxl", "haven"))
library(readxl)
ex_excel <- read_excel("example_g1e.xlsx", sheet = 1)
head(ex_excel)

library(haven)
ex_sas <- read_sas("example_g1e.sas7bdat")
head(ex_sas)
ex_spss <- read_sav("example_g1e.sav")
head(ex_spss)

# Write CSV, SAS, SPSS file
write.csv(ex, "example_g1e_ex.csv", row.names = F)
write_sas(ex_sas, "example_g1e_ex.sas7bdat")
write_sav(ex_spss, "example_g1e_ex.sav")

head(ex)
tail(ex)
head(ex, 10)
str(ex)
names(ex)
dim(ex)
nrow(ex)
ncol(ex)
class(ex)
class(ex_excel)
class(ex_sas)
class(ex_spss)
summary(ex)

ex$EXMD_BZ_YYYY
ex[, "EXMD_BZ_YYYY"]
ex[["EXMD_BZ_YYYY"]]
ex[, 1]
ex[[1]]

ex[, c("EXMD_BZ_YYYY", "RN_INDI", "BMI")]
ex[, c(1, 2, 16)]
ex[, names(ex)[c(1, 2, 16)]]

ex$EXMD_BZ_YYYY[1:50]
ex[1:50, 1]
ex[[1]][1:50]

unique(ex$EXMD_BZ_YYYY)
length(unique(ex$EXMD_BZ_YYYY))
table(ex$EXMD_BZ_YYYY)

mean(ex$BMI)
BMI_cat <- (ex$BMI >= 25); BMI_cat
unique(BMI_cat)
table(BMI_cat)
head(ex$BMI, 15)
rows <- which(ex$BMI >= 25); rows
head(rows)
length(rows)

values <- ex$BMI[ex$BMI >= 25]; values
head(values)
length(values)

BMI_HGHT_and <- (ex$BMI >= 25 & ex$HGHT >= 175); BMI_HGHT_and
BMI_HGHT_or  <- (ex$BMI >= 25 | ex$HGHT >= 175); BMI_HGHT_or

ex$zero <- 0
head(ex)
names(ex)
length(names(ex))

ex$BMI_cat <- (ex$BMI >= 25)
ex$BMI_cat_int <- as.integer(ex$BMI >= 25)
ex$BMI_cat_char <- as.character(ex$BMI >= 25)
ex$BMI_cat_if <- ifelse(ex$BMI >= 25, "1", "0")
head(ex)
table(ex$BMI_cat)
table(ex$BMI_cat_int)
table(ex$BMI_cat_char)
table(ex$BMI_cat_if)

ex[, "BMI_cat"] <- (ex$BMI >= 25)
ex[["BMI_cat"]] <- (ex$BMI >= 25)
head(ex)

# Check and set a class of variables
vars_cat <- c("RN_INDI", "Q_PHX_DX_STK", "Q_PHX_DX_HTDZ", "Q_PHX_DX_HTN", "Q_PHX_DX_DM", "Q_PHX_DX_DLD", "Q_PHX_DX_PTB", 
              "Q_HBV_AG", "Q_SMK_YN", "Q_DRK_FRQ_V09N")
vars_cat <- names(ex)[c(2, 4:12)]
vars_cat <- c("RN_INDI", grep("Q_", names(ex), value = T))

vars_conti <- setdiff(names(ex), vars_cat)
vars_conti <- names(ex)[!(names(ex) %in% vars_cat)]

# Factor Class로 변환
for (vn in vars_cat){
  ex[, vn] <- as.factor(ex[, vn])
}
summary(ex)

# Numeric Class로 변환
for (vn in vars_cat){
  ex[, vn] <- as.numeric(ex[, vn])
}
summary(ex)

ex <- ex_og
summary(ex)

# Factor --> Character --> Numeric Class로 순차적으로 변환
# factor: order가 있다.
# ==> as.integer를 바로 쓰면 [0, 1]이 [1, 2]가 될 수 있다.(R은 1부터 시작해서인듯?)
# 따라서 문자로 먼저 변환해야 한다.
table(ex$Q_PHX_DX_STK)
table(as.numeric(ex$Q_PHX_DX_STK))
table(as.numeric(as.character(ex$Q_PHX_DX_STK)))

# Date Class
head(ex$HME_YYYYMM)
addDate <- paste(ex$HME_YYYYMM, "01", sep = "")
addDate
ex$HME_YYYYMM <- as.Date(addDate, format = "%Y%m%d")
head(ex$HME_YYYYMM)
class(ex$HME_YYYYMM)

# tapply : 그룹별 통계
tapply(ex$LDL, ex$EXMD_BZ_YYYY, mean)

# 결측치 처리
tapply(ex$LDL, ex$EXMD_BZ_YYYY,
       function(x){
         mean(x, na.rm = T)
       }
)

summary(lm(LDL ~ HDL, data = ex))

# na.omit() : 결측치 처리 함수
nrow(ex)
ex_naomit <- na.omit(ex)
nrow(ex_naomit)
head(ex)


# 교수님의 결측치 처리 3원칙
# 1. 결측치가 너무 많으면(예: 10% 이상), 그 변수는 삭제
vars_ok <- sapply(names(ex), function(v){sum(is.na(ex[, v])) < nrow(ex)/10})
ex_impute <- ex[, vars_ok]

# 최빈값 함수
getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(ex$Q_PHX_DX_STK)
unique(ex$Q_PHX_DX_STK)

# 2. 연속변수는 중간값(median)으로 대체
# 3. 범주형변수는 최빈값(mode)로 대체
for (v in names(ex_impute)){
  if (is.factor(ex_impute[, v])){
    ex_impute[, v] <- ifelse(is.na(ex_impute[, v]),
                             getmode(ex_impute[, v]),
                             ex_impute[, v])
  } else if (is.numeric(ex_impute[, v])){
    ex_impute[, v] <- ifelse(is.na(ex_impute[, v]),
                             median(ex_impute[, v], na.rm = T),
                             ex_impute[, v])
  } else {
    ex_impute[, v]
  }
}

summary(ex_impute)

# Subset
ex1 <- ex_naomit
ex1
ex1_2012 <- ex1[ex1$EXMD_BZ_YYYY >= 2012, ]
head(ex1_2012)
table(ex1_2012$EXMD_BZ_YYYY)

ex1_2012 <- subset(ex1, EXMD_BZ_YYYY >= 2012)
table(ex1_2012$EXMD_BZ_YYYY)

# 그룹별 통계
aggregate(ex1[, c("WSTC", "BMI")], list(ex1$Q_PHX_DX_HTN), mean)
aggregate(cbind(WSTC, BMI) ~ Q_PHX_DX_HTN, data = ex1, mean)
aggregate(cbind(WSTC, BMI) ~ Q_PHX_DX_HTN, data = ex1, mean)

# 그룹 2개
aggregate(ex1[, c("WSTC", "BMI")], list(ex1$Q_PHX_DX_HTN, ex1$Q_PHX_DX_DM), mean)
aggregate(cbind(WSTC, BMI) ~ Q_PHX_DX_HTN + Q_PHX_DX_DM, data = ex1, mean)
aggregate(cbind(WSTC, BMI) ~ Q_PHX_DX_HTN + Q_PHX_DX_DM, data = ex1,
          function(x){
            c(mean = mean(x), sd = sd(x))
          }
)

# 모든 변수를 다 보고싶을 때
aggregate(. ~ Q_PHX_DX_HTN + Q_PHX_DX_DM, data = ex1, 
          function(x){
            c(mean = mean(x), sd = sd(x))
          }
)

# Sort
ex1$HGHT
ord <- order(ex1$HGHT)
ord
head(ord)
head(ex1$HGHT[ord])
ord_desc <- order(-ex1$HGHT)
ord_desc
head(ex1$HGHT[ord_desc])

ex1_sort <- ex1[ord, ]
head(ex1_sort)

# Wide to Long, Long to Wide
library(reshape2)
long <- melt(ex1, id = c("EXMD_BZ_YYYY", "RN_INDI"), measure.vars = c("BP_SYS", "BP_DIA"), variable.name = "BP_type", value.name = "BP")
long

wide <- dcast(long, EXMD_BZ_YYYY + RN_INDI ~ BP_type, value.var = "BP")
wide

# Merge
ex1_Q <- ex1[, c(1:3, 4:12)]
ex1_measure <- ex1[, c(1:3, 13:ncol(ex1))]
head(ex1_Q)
head(ex1_measure)

# all = T : 한 쪽에만 있는 샘플을 유지하는 옵션. 빈 변수는 NA로 채워짐
ex1_merge_T <- merge(ex1_Q, ex1_measure, by = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM"), all = T)
ex1_merge_T
# all = F : 공통인 샘플만 취하고자 할 때
ex1_merge_F <- merge(ex1_Q, ex1_measure, by = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM"), all = F)
ex1_merge_F


##################
# 2. tydyverse 
##################

# Install packages individually
#install.packages(c("readr", "magrittr", "dplyr", "purrr"))
# or
# Install all packages at once
#install.packages("tidyverse")
library(magrittr)
library(dplyr)

getwd()
setwd("data")

a <- read.csv("smc_example1.csv")
a %>% head()

a %>% head(n = 10)
10 %>% head(a, .)
10 %>% head(a, n = .)

# base
a_M <- subset(a, Sex == 'M')
head(a_M)
# tidyverse
a %>% subset(Sex == "M") %>% 
  head

# base
a$Sex
a[, "Sex"]
a[["Sex"]]
# tidyverse
a %>% .$Sex
a %>% .[, "Sex"]
a %>% .[["Sex"]]

# base
head(subset(a, Sex == "M"))
# tidyverse
a %>% subset(Sex == "M") %>% 
  head

# base
b <- subset(a, Sex == "M")
model <- glm(DM ~ Age + Weight + BMI, data = b, family = binomial)
summary(model)
summ_model <- summary(model)
summ_model$coefficients
# tidyverse
a %>% 
  subset(Sex == "M") %>% 
  glm(DM ~ Age + Weight + BMI, data = ., family = binomial) %>% 
  summary %>% 
  .$coefficients


a %>% head
a %>% 
  subset(Age >= 50) %>% 
  aggregate(. ~ Sex + Smoking, data = .,
            FUN = function(x){
              c(mean = mean(x), sd = sd(x))
            })

out <- a %>% 
  subset(Age >= 50) %>% 
  aggregate(. ~Sex + Smoking, data = .,
            FUN = function(x){
              c(mean = mean(x), sd = sd(x))
            })
out
names(a)

# filter()
a %>% 
  filter(Age >= 50) %>% 
  select(-STRESS_EXIST) %>% 
  group_by(Sex, Smoking) %>% 
  summarize_all(list(mean = mean, sd = sd))

a %>% subset(Sex == "M") %>% head
a %>% filter(Sex == "M") %>% head

a %>% subset(Age >= 50 & Age <= 60) %>% head
a %>% filter(Age >= 50, Age <= 60) %>% head
a %>% filter(between(Age, 50, 60)) %>% head

# arrange() : 정렬(default = 오름차순)
a[order(a$Age), ]
a %>% .[order(.$Age), ]
a %>% arrange(Age) %>% head

# desc() : 내림차순 정렬
a %>% arrange(desc(Age), desc(BMI)) %>% head

# select() : 변수 선택
head(a[, c("Sex", "Age", "Height")])
a %>% select(Sex, Age, Height) %>% head

a %>% select(Sex:Height) %>% head
a %>% select(2, 3, 4) %>% head
a %>% select(c(2, 3, 4)) %>% head
a %>% select(2:4) %>% head

a %>% select(-Sex, -Age, -Height) %>% head
a %>% select(-2, -3, -4) %>% head 
a %>% select(-(2:4)) %>% head

head(a[, grep("_date", names(a))])

# select() - ends_with()
a %>% select(ends_with("date")) %>% head

# select() - starts_with()
a %>% select(starts_with("D")) %>% head

# select() - contains()
a %>% select(contains("da")) %>% head  

names(a)
head(a)

a %>% 
  filter(Sex == "M") %>% 
  select(Sex:HTN) %>% 
  arrange(Age) %>% 
  head

# mutate() : 변수 생성
# base
a$old <- as.integer(a$Age >= 65)  
head(a)

# tidyverse
a %>% 
  mutate(Old = as.integer(Age >= 65), Overweight = as.integer(BMI >= 24)) %>% 
  head

# transmute : 새로 만든 변수만 보여주고자 할 때
a %>% 
  transmute(Old = as.integer(Age >= 65),
            Overweight = as.integer(BMI >= 24)) %>% 
  head

# group_by() & summarize()
a %>% 
  group_by(Sex, Smoking) %>% 
  summarize(count = n(),
            meanBMI = mean(BMI),
            sdBMI = sd(BMI))

a %>% 
  filter(Age >= 50) %>% 
  select(-STRESS_EXIST) %>% 
  group_by(Sex, Smoking) %>% 
  summarize_all(list(mean = mean, sd = sd))

# base
a %>% 
  subset(Age >= 50) %>% 
  aggregate(. ~ Sex + Smoking, data = .,
            FUN = function(x){c(mean = mean(x), sd = sd(x))})
warnings()

# purrr
library(purrr)

# map : list 형태로 반환하는 반복문 (= lapply)
map(a, class)
lapply(a, class)

# map_chr : 문자열 형태로 반환하는 반복문 (= sapply)
map_chr(a, class)
a %>% map_chr(class)
sapply(a, class)
unlist(map(a, class))
unlist(lapply(a, class))

a %>% sapply(function(x){x[1]})
a %>% sapply('[', 1)
a %>% map_chr('[', 1)

# 성별로 같은 회귀분석을 반복하는 코드
# lapply 이용
a %>% 
  group_split(Sex) %>% 
  lapply(function(x){
    lm(Death ~ Age, data = x, family = binomial)
  })

# map 이용
a %>% 
  group_split(Sex) %>% 
  map(~lm(Death ~ Age, data = ., family = binomial))

# 회귀분석 후 Age의 p-value 뽑기
# sapply
a %>% 
  group_split(Sex) %>% 
  sapply(function(x){
    lm(Death ~ Age, data = x, family = binomial) %>% 
    summary %>% 
    .$coefficients %>% 
    .[8]
  })

# map
a %>% 
  group_split(Sex) %>% 
  map_dbl(~lm(Death ~ Age, data = ., family = binomial) %>% 
            summary %>% 
            .$coefficients %>% 
            .[8])

a %>% 
  group_split(Sex) %>% 
  map(~lm(Death ~ Age, data = ., family = binomial)) %>% 
  map(summary) %>% 
  map("coefficients") %>% 
  map_dbl(8)

# 입력 변수가 2개 이상일 때
# mapply
mapply(sum, 1:5, 1:5)
sum %>% mapply(1:5, 1:5)
sum %>% mapply(1:5, 1:5, 1:5)

# map2, pmap
map2(1:5, 1:5, sum)
pmap(list(1:5, 1:5, 1:5), sum)

pmap_int(list(1:5, 1:5, 1:5), sum)
list(1:5, 1:5, 1:5) %>% pmap_int(sum)

# 두 문자열 합치기
# map2_chr
name <- c("Alice", "Bob")
place <- c("LA", "New York")
map2_chr(name, place, ~paste(.x, "was born at", .y))

# pmap_chr
life <- c("born", "died")
list(name, life, place) %>% pmap_chr(~paste(..1, "was", ..2, "at", ..3))


##################
# 3. data.table 
##################

#DT[i, j, by, order]
# where, i = row
#        j = column
#        by = group
#        order = order

#install.packages("data.table")
#install.packages("curl")
library(data.table)
library(curl)

getwd()
df <- read.table("example_g1e.csv", header = T)
dt <- fread("example_g1e.csv", header = T)
View(df); View(dt)
class(df)
class(dt)

write.csv(dt, "aa.csv", row.names = T)
fwrite(dt, "aa.csv")

head(dt)
names(dt)
dt[1:5]  
dt[c(3, 5)]
dt[BMI >= 30 & HGHT < 150]  

dt[, .(HGHT)]
dt[, c(13, 14)]  
dt[, .(HGHT, WGHT)]  
dt[, .(Height = HGHT, Weight = WGHT)]
vars <- c("HGHT", "WGHT")
dt[, ..vars]
dt[, vars, with = F]

icols = c(1:12); icols
dt[, !..icols]
dt[HGHT >= 180 & WGHT <= 80, .(m_chol = mean(TOT_CHOL), m_TG = mean(TG))]

dt[, .(mHGHT = mean(HGHT), mWGHT = mean(WGHT), mBMI =mean(BMI)), by = EXMD_BZ_YYYY]
# .SD : Subset of Data, 모든 컬럼을 의미한다
dt[, lapply(.SD, mean), by = EXMD_BZ_YYYY]

dt[HGHT >= 175, .N, by = .(EXMD_BZ_YYYY, Q_SMK_YN)]
dt[HGHT >= 175, .N, keyby = .(EXMD_BZ_YYYY, Q_SMK_YN)]

dt[, .(mHGHT = mean(HGHT), mWGHT = mean(WGHT), mBMI =mean(BMI)), by = EXMD_BZ_YYYY][order(mBMI)]
dt[, .(mHGHT = mean(HGHT), mWGHT = mean(WGHT), mBMI =mean(BMI)), by = EXMD_BZ_YYYY][order(-mBMI)]

dt[, .N, keyby = .(Q_PHX_DX_STK > 0, Q_PHX_DX_HTDZ > 0)]

setkey(dt, EXMD_BZ_YYYY)
key(dt)
setkey(dt, EXMD_BZ_YYYY, Q_HBV_AG)
key(dt)

dt[.(2011)]
dt[list(2011)]
dt[EXMD_BZ_YYYY == 2011]
dt[J(2011)]

dt[.(2011, 2)]
dt[list(2011, 2)]
dt[EXMD_BZ_YYYY == 2011 & Q_HBV_AG == 2]
dt[J(2011, 2)]

setkey(dt, NULL)
key(dt)

dt1 <- dt[c(1, 300, 500, 700, 1000)]
dt1 %>% select(EXMD_BZ_YYYY)
setkey(dt1, EXMD_BZ_YYYY)
dt2 <- dt[c(400, 600, 800, 1200, 1500)]; dt2
dt2 %>% select(EXMD_BZ_YYYY)
setkey(dt2, EXMD_BZ_YYYY)

dt1[dt2, on = 'EXMD_BZ_YYYY', nomatch = 0]
merge(dt1, dt2, by = 'EXMD_BZ_YYYY', all = F) %>% select(EXMD_BZ_YYYY)

dt2[dt1, on = 'EXMD_BZ_YYYY']
merge(dt1, dt2, by = 'EXMD_BZ_YYYY', all.x = T) %>% select(EXMD_BZ_YYYY)

dt1[dt2, on = 'EXMD_BZ_YYYY']
merge(dt1, dt2, by = 'EXMD_BZ_YYYY', all.y = T) %>% select(EXMD_BZ_YYYY)

merge(dt1, dt2, by = 'EXMD_BZ_YYYY', all = T) %>% select(EXMD_BZ_YYYY)

dt[, diff := HDL - LDL]; dt
dt[, ':=' (newHGHT = HGHT * 0.9, newWGHT = WGHT + 5)]; dt
dt[, ':=' (newHGHT = HGHT * 0.9, newWGHT = WGHT + 5)][]

dt[, newWGHT := NULL][]

# .SD : Subset of Data, 모든 컬럼을 의미한다
dt[, lapply(.SD, mean), by = EXMD_BZ_YYYY]
dt[, head(.SD, 2), by = EXMD_BZ_YYYY]
dt[, lapply(.SD, mean), by = EXMD_BZ_YYYY, .SDcols = c("HGHT", "WGHT")]
dt[LDL >= 150, .N]
dt[, c(.N, lapply(.SD, mean)), by = EXMD_BZ_YYYY, .SDcols = c("HGHT", "WGHT")]

dt_long <- melt(dt,
                id.vars = c("EXMD_BZ_YYYY", "RN_INDI", "HGHT", "WGHT"),
                measure.vars = c("TOT_CHOL", "HDL", "LDL"),
                variable.name = "measure",
                value.name = "val"); dt_long

names(dt)
col1 <- c("BP_SYS", "BP_DIA")
col2 <- c("HDL", "LDL")
melt(dt,
     measure = list(col1, col2),
     value.name = c("BP", "Chol"))

dt_wide <- dcast(dt_long, EXMD_BZ_YYYY + RN_INDI + HGHT + WGHT ~ measure,
                 value.var = "val"); dt_wide


#####################
# 4. ggplot2 & ggpubr
#####################

getwd()
data <- read.csv("example_g1e.csv") 
head(data)

hist(data$HGHT, main = "Distribution of Height", xlab = "height(cm)")
hist(data$HGHT, main = "Distribution of Height", xlab = "height(cm)",
     breaks = 30, freq = F, col = "grey", border = "white")

table <- table(data$Q_SMK_YN); table
barplot(table, main = "Distribution of Smoking",
        names.arg = c("Never", "Ex-smoker", "Current"), ylab = "frequency")

table <- table(data$Q_SMK_YN, data$EXMD_BZ_YYYY); table
barplot(table, main = "Distribution of smoking by year", ylab = "frequency",
        legend = c("Never", "Ex-smoker", "Current"))
barplot(table, main = "Distribution of smoking by year", ylab = "frequency",
        legend = c("Never", "Ex-smoker", "Current"), beside = T)

boxplot(BP_SYS ~ Q_SMK_YN, data = data, names = c("Never", "Ex-smoker", "Current"),
        main = "SBP average by smoking", ylab = "SBP(mmHg)", xlab = "Smoking")

plot(HGHT ~ WGHT, data = data, ylab = "Height(cm)", xlab = "Weight(kg)",
     pch = 16, cex = 0.5)

data2 <- data %>% filter(EXMD_BZ_YYYY %in% c(2009, 2015))
plot(HGHT ~ WGHT, data = data2, col = factor(EXMD_BZ_YYYY),
     ylab = "Height(cm)", xlab = "Weight(kg)", pch = 16, cex = 0.5)
legend(x = "bottomright", legend = c("2009", "2015"), col = 1:2, pch = 19)

table <- data %>% group_by(EXMD_BZ_YYYY) %>% 
  summarize(smoker = mean(Q_SMK_YN == 3, na.rm = T))
table

plot(table$EXMD_BZ_YYYY, table$smoker, type = 'l', xlab = "Year", ylab = "prop of current smoker")

#install.packages("ggplot2")
#install.packages("ggpubr")
library(ggplot2)

# color (factor를 꼭 써야한다)
ggplot(data = data2, aes(x = HGHT, y = WGHT, col = factor(EXMD_BZ_YYYY))) +
  geom_point()

# theme change
ggplot(data=data2, aes(x=HGHT, y=WGHT, col=factor(EXMD_BZ_YYYY))) +
  geom_point() +
  theme_classic()

# scale_color_manual
ggplot(data = data2, aes(x = HGHT, y = WGHT, col = factor(EXMD_BZ_YYYY))) +
  geom_point() +
  ggtitle("Height and weight in year 2009 and 2015") +
  xlab("Height(cm)") +
  ylab("Weight(kg)") +
  scale_color_manual(
    values = c("orange", "skyblue"),
    labels = c("Year 2009", "Year 2015"),
    name = "Exam year")

# geom_smoth
ggplot(data = data2, aes(x=HGHT, y=WGHT)) +
  geom_point(aes(col=factor(EXMD_BZ_YYYY)), alpha=0.5) +
  ggtitle("Height and weight in year 2009 and 2015") +
  xlab("Height(cm)") +
  ylab("Weight(kg)") +
  scale_color_manual(
    values = c("orange", "skyblue"),
    labels = c("Year 2009", "Year 2015"),
    name = "Exam year") +
  geom_smooth(color = "brown", size = 0.8)

data2 <- data %>% filter(!is.na(Q_SMK_YN))
ggplot(data=data2, aes(x=factor(Q_SMK_YN), y=BP_SYS)) +
  geom_boxplot() +
  ggtitle("SBP average by smoking") +
  ylab("SBP(mmHg)") +
  xlab("Smoking") +
  scale_x_discrete(labels = c("Never", "Ex-smoker", "Current"))

data2 <- data %>% filter(!is.na(Q_PHX_DX_HTN) & !is.na(Q_SMK_YN))
ggplot(data = data2, aes(x = factor(Q_SMK_YN), y = BP_SYS)) +
  geom_boxplot() +
  ggtitle("SBP average by smoking") +
  ylab("SBP(mmHg)") +
  xlab("Smoking") +
  scale_x_discrete(labels = c("Never", "Ex-smoker", "Current")) +
  facet_wrap(~Q_PHX_DX_HTN, labeller = label_both)

data2 <- data2 %>% filter(!is.na(Q_PHX_DX_DM) & !is.na(Q_SMK_YN))
HTN.labs <- c("No HTN", "HTN")
names(HTN.labs) <- c("0", "1")
DM.labs <- c("No DM", "DM")
names(DM.labs) <- c("0", "1")
DM.labs

ggplot(data = data2, aes(x = factor(Q_SMK_YN), y=BP_SYS)) +
  geom_boxplot() +
  ggtitle("SBP average by smoking") +
  xlab("Smoking") +
  ylab("SBP(mmHg)") +
  scale_x_discrete(labels = c("Never", "Ex-smoker", "Current")) +
  facet_grid(Q_PHX_DX_DM ~ Q_PHX_DX_HTN,
             labeller = labeller(Q_PHX_DX_HTN = HTN.labs, Q_PHX_DX_DM = DM.labs))

ggplot(data = data2, aes(x = factor(Q_SMK_YN))) +
  geom_bar(fill = "grey", color = "black") +
  ggtitle("Distribution of smoking") +
  xlab("Smoking") +
  scale_x_discrete(labels = c("Never", "Ex", "Current"))

ggplot(data = data2, aes(x = EXMD_BZ_YYYY, fill = factor(Q_SMK_YN))) +
  geom_bar(position = "fill", color = "black") +
  ggtitle("Distribution of smoking") +
  xlab("Year") +
  ylab("propotion") +
  scale_fill_manual(
    values = c("orange", "skyblue", "navy"),
    labels = c("Never", "Ex", "Current"),
    name = "Smoking") +
  scale_x_continuous(breaks = 2009:2015)

ggplot(data = data2, aes(x = EXMD_BZ_YYYY, fill = factor(Q_SMK_YN))) +
  geom_bar(position = "dodge", color = "black") +
  ggtitle("Distribution of smoking") +
  xlab("Year") +
  ylab("propotion") +
  scale_fill_manual(
    values = c("orange", "skyblue", "navy"),
    labels = c("Never", "Ex", "Current"),
    name = "Smoking") +
  scale_x_continuous(breaks = 2009:2015)

ggplot(data = data2, aes(x = EXMD_BZ_YYYY, fill = factor(Q_SMK_YN))) +
  geom_bar(position = "dodge", color = "black") +
  ggtitle("Distribution of smoking") +
  xlab("Year") +
  ylab("propotion") +
  scale_fill_manual(
    values = c("orange", "skyblue", "navy"),
    labels = c("Never", "Ex", "Current"),
    name = "Smoking") +
  scale_x_continuous(breaks = 2009:2015) +
  coord_flip()

library(ggpubr)
data3 <- data2 %>% mutate(HTN = as.factor(ifelse(Q_PHX_DX_HTN == 1, "Yes", "No")))
head(data3)

p <- gghistogram(data = data3, x = "WGHT", color = "HTN", fill = "HTN", add = "mean")
p
plot1 <- ggpar(p,
               main = "Weight distribution by HTN history",
               xlab = "Weight(kg)",
               legend.title = "HTN Dx history")
plot1

p <- ggboxplot(data = data3, x = "HTN", y = "WGHT", color = "HTN") +
  stat_compare_means(method = "t.test", label.x.npc = "middle")
p
plot2 <- ggpar(p,
               main = "Weight distribution by HTN history",
               ylab = "Weight(kg)",
               xlab = "HTN Dx history",
               legend = "none")
plot2

my_comparisons <- list(c("1", "2"), c("2", "3"), c("1", "3"))
p <- ggboxplot(data = data3, x = "Q_SMK_YN", y = "WGHT", color = "Q_SMK_YN") +
  stat_compare_means(comparisons = my_comparisons) +
  stat_compare_means(label.y = 150) +
  scale_x_discrete(labels = c("Never", "Ex", "Current"))
p
plot3 <- ggpar(p,
               main = "Weight distribution by smoking",
               ylab = "Weight(kg)",
               xlab = "Smoking",
               legend = "none")
plot3

p <- ggscatter(data = data3, x = "HGHT", y = "WGHT",
               add = "reg.line", conf.int = TRUE,
               add.params = list(color = "navy", fill = "lightgrey")) +
  stat_cor(method = "pearson")
p
plot4 <- ggpar(p,
               ylab = "Weight(kg)",
               xlab = "Height(cm)")
plot4

p <- ggscatter(data = data3, x = "HGHT", y = "WGHT",
               color = "HTN", alpha = 0.5,
               add = "reg.line", conf.int = TRUE) +
  stat_cor(aes(color = HTN))
p
plot5 <- ggpar(p,
               ylab = "Weight(kg)",
               xlab = "Height(cm)")
plot5

ggarrange(plot2, plot3,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

#install.packages(c("rvg", "officer"))
library(rvg)
library(officer)

plot_file <- read_pptx() %>% 
  add_slide() %>% ph_with(dml(ggobj = plot1), location = ph_location_type(type = "body")) %>% 
  add_slide() %>% ph_with(dml(ggobj = plot2), location = ph_location_type(type = "body")) %>% 
  add_slide() %>% ph_with(dml(ggobj = plot3), location = ph_location_type(type = "body")) %>% 
  add_slide() %>% ph_with(dml(ggobj = plot4), location = ph_location_type(tpye = "body")) %>% 
  add_slide() %>% ph_with(dml(ggobj = plot5), location = ph_location_type(type = "body"))

print(plot_file, target = "plot_file.pptx")
