#################################################
# 2nd_Class_Practice
#################################################

#install.packages("dplyr")
x <- c(1, 2, 3, 4, 5, 6)
y <- c(7, 8, 9, 10, 11, 12)
x + y
x * y
sqrt(x)
sum(x)
diff(x)
mean(x)
var(x)
sd(x)
median(x)
IQR(x)
max(x)
which.max(x)
max(x, y)
length(x)
x[2]
x[-2]
x[1:3]
x[c(1, 2, 3)]
x[c(1, 3, 4, 5, 6)]
x >= 4
sum(x >= 4)
x[x >= 4]
sum(x[x >= 4])
x[x %in% c(1, 3, 5)]

v1 <- seq(-5, 5, by = 0.2); v1
v2 <- rep(1, 3); v2
v3 <- rep(c(1, 2, 3), 2); v3
v4 <- rep(c(1, 2, 3), each = 2); v4

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
}
x

x <- 5
if (x >= 10){
  print("High")
} else if (x >= 5){
  print("Medium")
} else {
  print("Low")
}

x <- 1:6; x
y <- ifelse(x >= 4, "yes", "no"); y

x <- c(1:10, 12, 13, NA, NA, 15, 17)
mean(x)

mean0 <- function(x){
  mean(x, na.rm = T)
}
mean0(x)

twomean <- function(x1, x2){
  a <- (x1 + x2)/2
  a
}
twomean(4, 6)

mat <- matrix(1:20, nrow = 4, byrow = T); mat
mat1 <- matrix(1:20, nrow = 4, byrow = F); mat1

out <- NULL
for (i in 1:nrow(mat)){
  out <- c(out, mean(mat[i, ]))
}
out

sapply(1:nrow(mat), function(x){mean(mat[x, ])})
mat_mean <- lapply(1:nrow(mat), function(x){mean(mat[x, ])}); mat_mean
unlist(lapply(1:nrow(mat), function(x){mean(mat[x, ])}))

apply(mat, 1, mean)
rowMeans(mat)
rowSums(mat)

apply(mat, 2, mean)
colMeans(mat)
colSums(mat)

# ----------------------------------
# Exercise 1. 
# Find maximum of vectors below respectively by using sapply or lapply
# ----------------------------------

x <- 1:6; x
y <- 7:12; y

list(x, y)
lapply(list(x, y), max)
sapply(list(x, y), max)

getwd()
setwd("data")
getwd()

ex <- read.csv("example_g1e.csv")
head(ex)

# install.packages(c("readxl", "haven"))
library(readxl)
ex_excel <- read_excel("example_g1e.xlsx", sheet = 1)
head(ex_excel)

library(haven)
ex_sas <- read_sas("example_g1e.sas7bdat")
head(ex_sas)
ex_spss <- read_sav("example_g1e.sav")
head(ex_spss)

write.csv(ex, "example_g1e_ex.csv", row.names = F)

head(ex)
tail(ex)
str(ex)
names(ex)
dim(ex)
nrow(ex)
ncol(ex)
class(ex)
class(ex_spss)
summary(ex)

ex$EXMD_BZ_YYYY
ex[, "EXMD_BZ_YYYY"]
ex[["EXMD_BZ_YYYY"]]
ex[, 1]
ex[[1]]
head(ex[, c(1, 2, 16)])
head(ex[, names(ex)[c(1, 2, 16)]])

ex$EXMD_BZ_YYYY[1:50]
ex[1:50, 1]
ex[[1]][1:50]

unique(ex$EXMD_BZ_YYYY)
length(unique(ex$EXMD_BZ_YYYY))
table(ex$EXMD_BZ_YYYY)

mean(ex$BMI)
BMI_cat <- (ex$BMI >= 25)
table(BMI_cat)

rows <- which(ex$BMI >= 25)
rows
head(rows)
ex$BMI

values <- ex$BMI[ex$BMI >= 25]
head(values)
rows_values <- data.frame(which(ex$BMI >= 25), ex$BMI[ex$BMI >= 25])
rows_values

length(values)

BMI_HGHT_and <- (ex$BMI >= 25 & ex$HGHT >= 175); head(BMI_HGHT_and)
BMI_HGHT_or <- (ex$BMI >= 25 | ex$HGHT >= 175); head(BMI_HGHT_or)

ex$zero <- 0
head(ex)
ex$BMI_cat <- (ex$BMI >= 25)
ex$BMI_cat <- as.integer(ex$BMI >= 25)
ex$BMI_cat <- as.character(ex$BMI >= 25)
ex$BMI_cat <- ifelse(ex$BMI >= 25, "1", "0")
table(ex$BMI_cat)

ex[, "BMI_cat"] <- (ex$BMI >= 25)
ex[["BMI_cat"]] <- (ex$BMI >= 25) 

vars.cat1 <- names(ex)[c(2, 4:12)]
vars.cat2 <- c("RN_INDI", grep("Q_", names(ex), value = T))
vars.cat1
vars.cat2

vars.conti <- setdiff(names(ex), vars.cat)
vars.conti

for (vn in vars.cat){
  ex[, vn] <- as.factor(ex[, vn])
}
for (vn in vars.conti){
  ex[, vn] <- as.numeric(ex[, vn])
}
summary(ex)

table(
  as.numeric(ex$Q_PHX_DX_STK)
)
table(
  as.numeric(as.character(ex$Q_PHX_DX_STK))
)

addDate <- paste(ex$HME_YYYYMM, "01", sep="")
ex$HME_YYYYMM <- as.Date(addDate, format = "%Y%m%d")
head(ex$HME_YYYYMM)
class(ex$HME_YYYYMM)

tapply(ex$LDL, ex$EXMD_BZ_YYYY, mean)
tapply(ex$LDL, ex$EXMD_BZ_YYYY,
       function(x){
         mean(x, na.rm = T)
       })

summary(lm(LDL ~ HDL, data = ex))

ex_naomit <- na.omit(ex)
nrow(ex_naomit)

ex_impute <- ifelse(na)

#############
# Exercise 2.
#############

getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(ex$Q_PHX_DX_STK)

vars.ok <- sapply(names(ex), function(v){sum(is.na(ex[, v])) < nrow(ex)/10})
vars.ok
ex_impute <- ex[, vars.ok]
names(ex_impute)

for (v in names(ex_impute)){
  if (is.factor(ex_impute[, v])){
    ex_impute[, v] <- ifelse(is.na(ex_impute[, v]),
                             getmode(ex_impute[, v]),
                             ex_impute[, v])
  } else if (is.numeric(ex[, v])){
    ex_impute[, v] <- ifelse(is.na(ex_impute[, v]),
                             median(ex_impute[, v], na.rm = T),
                             ex_impute[, v])
  } else {
    ex_impute[, v]
  }
}

var_fine <- sapply(names(ex), function(v){sum(is.na(ex[, v])) < nrow(ex)/10})
var_fine
ex_impute <- ex[, var_fine]
names(ex_impute)
length(ex)
length(ex_impute)

for (v in names(ex_impute)){
  if (is.factor(ex_impute[, v])){
    ex_impute[, v] <- ifelse(is.na(ex_impute[, v]),
                             getmode(ex_impute[, v]),
                             ex_impute[, v])
  } else if (is.numeric(ex_impute[, v])){
    ex_impute[, v] <- ifelse(is.na(ex_impute[, v]),
                             median(ex_impute[, v], na.rm = T),
                             ex_impute[, v])
  } else{
    ex_impute[, v]
  }
}

summary(ex_impute)

ex1 <- ex.naomit
ex1_2012 <- ex1[ex1$EXMD_BZ_YYYY >= 2012, ]
table(ex1_2012$EXMD_BZ_YYYY)
ex2_2012 <- subset(ex1, EXMD_BZ_YYYY >= 2012)
table(ex2_2012$EXMD_BZ_YYYY)

aggregate(ex1[, c("WSTC", "BMI")], list(ex1$Q_PHX_DX_HTN), mean)
aggregate(cbind(WSTC, BMI) ~ Q_PHX_DX_HTN, data = ex1, mean)

aggregate(ex1[, c("WSTC", "BMI")], list(ex1$Q_PHX_DX_HTN, ex1$Q_PHX_DX_DM), mean)
aggregate(cbind(WSTC, BMI) ~ Q_PHX_DX_HTN + Q_PHX_DX_DM, data = ex1, mean)

aggregate(cbind(WSTC, BMI) ~ Q_PHX_DX_HTN + Q_PHX_DX_DM, data = ex1, function(x){c(mean = mean(x), sd = sd(x))})
aggregate(. ~ Q_PHX_DX_HTN + Q_PHX_DX_DM, data = ex1, function(x){c(mean = mean(x), sd = sd(x))})

ex1$HGHT
ord <- order(ex1$HGHT); ord
ex1$HGHT[ord]
ord_desc <- order(-ex1$HGHT)
head(ex1$HGHT[ord_desc])

ex1_sort <- ex1[ord, ]
head(ex1_sort)

library(reshape2)
long <- melt(ex1, id = c("EXMD_BZ_YYYY", "RN_INDI"), measure.vars = c("BP_SYS", "BP_DIA"), variable.name = "BP_type", value.name = "BP"); 
View(long)
head(ex1)

wide <- dcast(long, EXMD_BZ_YYYY + RN_INDI ~ BP_type, value.var = "BP")
head(wide)

ex1.Q <- ex1[, c(1:3, 4:12)]
ex1.measure <- ex1[, c(1:3, 13:ncol(ex1))]
head(ex1.Q)
head(ex1.measure)

ex1.merge <- merge(ex1.Q, ex1.measure, by = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM"), all = T)
head(ex1.merge)


#################################################
# 3rd_Class_Practice
#################################################

library(magrittr)
library(dplyr)
a <- read.csv("https://raw.githubusercontent.com/jinseob2kim/R-skku-biohrs/main/data/smc_example1.csv")
a %>% head()

a %>% 
  subset(Sex == 'M') %>% 
  head()

a %>% 
  subset(Sex == 'M') %>% 
  glm(DM ~ Age + Weight + BMI, data = ., family = binomial) %>% 
  summary %>% 
  .$coefficients

out <- a %>% 
  subset(Age >= 50) %>% 
  aggregate(. ~Sex + Smoking, data = ., 
            FUN = function(x){c(mean = mean(x), sd = sd(x))})

out

a %>% 
  filter(Age >= 50) %>% 
  select(-STRESS_EXIST) %>% 
  group_by(Sex, Smoking) %>% 
  summarize_all(list(mean = mean, sd = sd))

a %>% 
  arrange(Age) %>% 
  head

a %>%
  arrange(Age, desc(BMI))

a %>% 
  select(Sex, Age, Height) %>% 
  head

a %>% 
  select(starts_with("S")) %>% 
  head

a %>% 
  select(ends_with("date")) %>% 
  head

a %>% 
  select(contains("_")) %>% 
  head

a %>% 
  filter(Sex == 'M') %>% 
  select(Sex:HTN) %>% 
  arrange(Age)

a %>% 
  mutate(Old = as.integer(Age >= 65), Overweight = as.integer(BMI >= 27)) %>% 
  head

a %>% 
  transmute(Old = as.integer(Age >= 65),
            Overweight = as.integer(BMI >= 27)) %>% 
  head

a %>% 
  group_by(Sex, Smoking) %>% 
  summarize(count = n(),
            meanBMI = mean(BMI),
            sdBMI = sd(BMI))

a %>% 
  filter(Age >= 50) %>% 
  group_by(Sex, Smoking) %>% 
  summarize_all(mean)

a %>% 
  filter(Age >= 50) %>% 
  select(-STRESS_EXIST) %>% 
  group_by(Sex, Smoking) %>% 
  summarize_all(list(mean = mean, sd = sd))

a %>% 
  filter(Age >= 50) %>%
  select(-STRESS_EXIST) %>% 
  group_by(Sex, Smoking) %>% 
  summarize_all(list(mean = mean, sd = sd))


#################################################
# 4th_Class_Practice
#################################################
install.packages("data.table")
install.packages("curl")
library(data.table)
library(curl)
library(magrittr)

getwd()
setwd("C:/Users/James/Documents/GitHub/R-skku-biohrs")
getwd()

data_file <- "C:/Users/James/Documents/GitHub/R-skku-biohrs/data/example_g1e.csv"

df <- read.table(data_file, header = T)
df %>% head

dt <- fread(data_file, header = T)
dt %>% head

print(class(df))
print(class(dt))

dt[c(3, 5)]
dt[1:5]
dt[, 1:2]
dt[BMI >= 30 & HGHT < 150]

dt[, list(HGHT, WGHT)]
dt[, .(HGHT, WGHT)]
dt[, .(Height = HGHT, Weight = WGHT)]   # Rename
dt[, c("HGHT", "WGHT")]                 

dt[, c(HGHT, WGHT)]                     # Vector

# 변수로 열 선택
col_vars <- grep("Q_", names(dt), value = T)
col_vars
dt[, ..col_vars]
dt[, col_vars, with = F]
dt[, .SD, .SDcols = col_vars]

# 열 제외
dt[, !..col_vars]
dt[, -..col_vars]

dt[, .(mean(HGHT), mean(WGHT), mean(BMI))]
dt[, .(mHGHT = mean(HGHT), mwGHT = mean(WGHT), mBMI = mean(BMI))]
dt[, lapply(.SD, mean), .SDcols = c("HGHT", "WGHT", "BMI")]

# by: 그룹별 요약통계량
dt[, .(mHGHT = mean(HGHT), mwGHT = mean(WGHT), mBMI = mean(BMI)), by = EXMD_BZ_YYYY]
dt[, .(mHGHT = mean(HGHT), mwGHT = mean(WGHT), mBMI = mean(BMI)), by = "EXMD_BZ_YYYY"]
dt[, lapply(.SD, mean), .SDcols = c("HGHT", "WGHT", "BMI"), by = EXMD_BZ_YYYY]
dt[HGHT >= 175, .N, by = .(EXMD_BZ_YYYY, Q_SMK_YN)]

# keyby: 그룹별 정렬기능 추가
dt[HGHT >= 175, .N, keyby = .(EXMD_BZ_YYYY, Q_SMK_YN)]
dt[HGHT >= 175, .N, by = .(EXMD_BZ_YYYY, Q_SMK_YN)][order(Q_SMK_YN)]
dt[HGHT >= 175, .N, by = .(EXMD_BZ_YYYY, Q_SMK_YN)][order(N)]

dt[HGHT >= 175, .N, keyby = .(EXMD_BZ_YYYY >= 2015, Q_PHX_DX_STK == 1)]
dt[HGHT >= 175, .N, keyby = .(get("EXMD_BZ_YYYY") >= 2015, get("Q_PHX_DX_STK") == 1)]
dt[HGHT >= 175, .N, keyby = .(Y2015 = ifelse(EXMD_BZ_YYYY >= 2015, ">=2015", "<2015"))]

# Merge
dt1 <- dt[1:10, .SD, .SDcols = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM", col_vars)]; dt1
dt2 <- dt[6:15, -..col_vars]; dt2

merge(dt1, dt2, by = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM"), all = T)    # Full join
merge(dt1, dt2, by = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM"), all = F)    # Inner join
merge(dt1, dt2, by = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM"), all.x = T)  # Left join
merge(dt1, dt2, by = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM"), all.y = T)  # Right join

dt1[!dt2, on = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM")]                   # Left anti join
dt2[!dt1, on = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM")]                   # Right anti join

# 열 생성/수정/삭제
dt[, diff := HDL - LDL] %>% 
  head
dt[, ':='(HGHT = HGHT*0.9, WGHT = WGHT+5)] %>% 
  head

dt[, diff := HDL-LDL][]
dt[, ':='(HGHT = HGHT*0.9, WGHT = WGHT+5)][]

dt[, BMI := NULL][]
dt[, diff := NULL][]

# .SD(Subset of Data): 모든 칼럼 대상으로 연산 수행
dt[, .SD]
dt[, lapply(.SD, class)]
dt[, lapply(.SD, mean), keyby = EXMD_BZ_YYYY]

dt[, head(.SD, 2), by = EXMD_BZ_YYYY]

# .SDcols: 연산 대상이 되는 특정 칼럼 지정
dt[, lapply(.SD, mean), by = EXMD_BZ_YYYY, .SDcols=c("HGHT", "WGHT")]

# .N: 부분 데이터의 행의 갯수
dt[LDL >= 150, .N]
dt[, c(.N, lapply(.SD, mean)), keyby = EXMD_BZ_YYYY, .SDcols = c("HGHT", "WGHT")]

# 사람(RN_INDI)별 방문이력(HME_YYYYMM)에서 첫 번째 진단일/마지막 진단일
dt[order(HME_YYYYMM), .SD[1], keyby = "RN_INDI"]
dt[order(HME_YYYYMM), .SD[.N], keyby = "RN_INDI"]

dt[order(EXMD_BZ_YYYY), .SD[1], .SDcols = col_vars, keyby = "RN_INDI"]
dt[, .N, keyby = "RN_INDI"]

# Melt(Wide to long)
dt_long1 <- melt(dt,
                 id.vars = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM"),
                 measure.vars = c("TOT_CHOL", "TG", "HDL", "LDL"),
                 variable.name = "Lipid",
                 value.name = "Value")
View(dt_long1)

# Enhanced melt
col1 <- c("BP_SYS", "BP_DIA")
col2 <- c("HDL", "LDL")
dt_long2 <- melt(dt,
                 id.vars = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM"),
                 measure = list(col1, col2),
                 value.name = c("BP", "VA"))
View(dt_long2)

dt_long2[, variable := factor(variable, labels = c("SBP/VA_LT", "DBP/VA_RT"))]; dt_long2

# dcast(long to wide)
dt_wide1 <- dcast(dt_long1, EXMD_BZ_YYYY + RN_INDI + HME_YYYYMM ~ Lipid, value.var = "Value"); dt_wide1

# aggregate
dt_wide2 <- dcast(dt_long1, RN_INDI ~ Lipid, value.var = "Value", fun.aggregate = mean, na.rm = T); dt_wide2

# Enhanced dcast
dt_wide3 <- dcast(dt_long2, ... ~ variable, value.var = c("BP", "VA")); dt_wide3



