#install.packages(c("data.table", "magrittr", "fst", "ggplot2", "ggpubr", "officer", "rvg", "tableone", "gtsummary", "MatchIt", "twang", "usethis", "gitcreds"))

### R 기초연산 : 벡터(vector) -------------------------------------------------------------------
x <- c(1, 2, 3, 4, 5 ,6); x
y <- c(7, 8, 9, 10, 11, 12); y
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
max(y)
which.max(y)
max(x, y)
length(y)

x[2]
x[-2]
x[1:3]
x[c(1, 2, 3)]
y[c(1, 3, 4, 5, 6)]
x >= 4
sum(x >= 4)
y[x >= 4]
sum(y[x >= 4])
x %in% c(1, 3, 5)
y[x %in% c(1, 3, 5)]


### 벡터만들기 --------------------------------------------------------------------
v1 <- seq(-5, 5, by = 0.2); v1
v2 <- rep(1, 3); v2
v3 <- rep(c(1, 2, 3), 2); v3
v4 <- rep(c(1, 2, 3), each = 2); v4


### for loop -----------------------------------------------------------------------
for (i in 1:3){
  print(i)
}

i <- 0
for (j in c(1, 2, 4, 5, 6)){
  i <- i+j
}


x <- 5
if (x >= 3){
  x <- x+3
}
x

### if/else loop -------------------------------------------------------------------
x <- 5
if(x >= 10){
  print("High")
} else if(x >= 5){
  print("Medium")
} else {
  print("Low")
}

### ifelse loop -------------------------------------------------------------------
x <- 1:6; x
y <- ifelse(x >= 4, "Yes", "No"); y

### 함수 만들기 -------------------------------------------------------------------
x <- c(1:10, 12, 13, NA, NA, 15, 17); x
mean(x)

mean0 <- function(x){
  mean(x, na.rm = T)
}
mean0(x)

twomean <- function(x1, x2){
  a <- (x1 + x2) / 2
  a
}
twomean(4, 6)

mat <- matrix(1:20, nrow = 4, byrow = T); mat
out <- NULL; out
for (i in 1:nrow(mat)){
  out <- c(out, mean(mat[i, ]))
}
out

### Apply 문 : apply, sapply, lapply -----------------------------------------------
# sapply : easy way for vector, matrix
sapply(1:nrow(mat), function(x){mean(mat[x, ])})

# lapply : easy way for vector, matrix
lapply(1:nrow(mat), function(x){mean(mat[x, ])})
unlist(lapply(1:nrow(mat), function(x){mean(mat[x, ])}))

# apply / by row
apply(mat, 1, mean)
rowMeans(mat)
rowSums(mat)

# apply / by column 
apply(mat, 2, mean)
colMeans(mat)
colSums(mat)

### 연습문제 1 ----------------------------------------------------------------------
# sapply나 lapply를 이용하여, 아래 두 벡터의 최대값을 각각 구하여라.
x <- 1:6; x
y <- 7:12; y

sapply(list(x, y), max)
unlist(lapply(list(x, y), max))

### 데이터 불러오기, 저장하기 --------------------------------------------------------
getwd()
setwd("data")
getwd()

dir()
ex <- read.csv("example_g1e.csv")
ex_og <- ex
head(ex)

### pkg "readxl" for Excel files --------------------------------------------------------
#install.packages(c("readxl", "haven"))
library(readxl)
ex.excel <- read_excel("example_g1e.xlsx", sheet = 1)
head(ex.excel)

### pkg "haven" for SAS, SPSS files ------------------------------------------------------
library(haven)
ex.sas <- read_sas("example_g1e.sas7bdat")
ex.spss <- read_sav("example_g1e.sav")
head(ex.sas)
head(ex.spss)

write.csv(ex, "example_g1e_ex2.csv", row.names = F)
write_sas(ex.sas, "example_g1e_ex2.sas7bdat")
write_sav(ex.spss, "example_g1e_ex2.sav")

### 데이터 살펴보기 ----------------------------------------------------------------------
head(ex)
tail(ex)
head(ex, 10)
str(ex)
names(ex)
dim(ex)
nrow(ex)
ncol(ex)
class(ex)
class(ex.excel)
class(ex.sas)
class(ex.spss)
summary(ex)

### 특정 변수 보기 --------------------------------------------------------------------
ex$EXMD_BZ_YYYY

# Matrix 스타일
ex[, "EXMD_BZ_YYYY"]
ex[, 1]

# List 스타일
ex[["EXMD_BZ_YYYY"]]
ex[[1]]

# 2개 이상 변수선택은 행렬 스타일로
head(ex[, c("EXMD_BZ_YYYY", "RN_INDI", 'BMI')])
head(ex[, c(1, 2, 16)])
head(ex[, names(ex)[c(1, 2, 16)]])

ex$EXMD_BZ_YYYY[1:50]
ex[1:50, 1]
ex[[1]][1:50]

# unique, table
unique(ex$EXMD_BZ_YYYY)
length(unique(ex$EXMD_BZ_YYYY))
table(ex$EXMD_BZ_YYYY)

### 새로운 변수 만들기 --------------------------------------------------------------------
head(ex$BMI)
mean(ex$BMI)
BMI_cat <- (ex$BMI >= 25)
head(BMI_cat)
table(BMI_cat)

rows <- which(ex$BMI >= 25)
head(rows)

values <- ex$BMI[ex$BMI >= 25]
head(values)
length(values)

BMI_HGHT_and <- (ex$BMI >= 25 & ex$HGHT >= 175)
BMI_HGHT_or  <- (ex$BMI >= 25 | ex$HGHT >= 175)

ex$zero <- 0
names(ex)
ex$BMI_cat <- (ex$BMI >= 25)
ex$BMI_cat <- as.integer(ex$BMI >= 25)
head(ex)
ex$BMI_cat <- as.character(ex$BMI >= 25)
ex$BMI_cat <- ifelse(ex$BMI >= 25, "1", "0")
table(ex$BMI_cat)

# Matrix style
ex[, "BMI_cat"] <- (ex$BMI >= 25)
# List Style
ex[["BMI_cat"]] <- (ex$BMI >= 25)
table(ex$BMI_cat)

### 변수 클래스 설정: 데이터 읽은 후 가장 먼저 해야할 것 ---------------------------------------
# 범주형 변수만 선택
vars.cat <- c("RN_INDI", "Q_PHX_DX_STK", "Q_PHX_DX_HTDZ", "Q_PHX_DX_HTN", "Q_PHX_DX_DM", "Q_PHX_DX_DLD",
              "Q_PHX_DX_PTB", "Q_HBV_AG", "Q_SMK_YN", "Q_DRK_FRQ_V09N")
#vars.cat <- names(ex)[c(2, 4:12)]
#vars.cat <- c("RN_INDI", grep("Q_", names(ex), value = T))
vars.cat

# 연속형 범주만 선택
vars.conti <- setdiff(names(ex), vars.cat)
#vars.conti <- names(ex)[!(names(ex) %in% vars.cat)]
vars.conti

# numeric, character, factor classes
for (vn in vars.cat){
  ex[, vn] <- as.factor(ex[, vn])
}

for (vn in vars.conti){
  ex[, vn] <- as.numeric(ex[, vn])
}

summary(ex)
table(ex$Q_PHX_DX_STK)
table(as.numeric(ex$Q_PHX_DX_STK))
table(as.numeric(as.character(ex$Q_PHX_DX_STK)))

# Date Class
head(ex$HME_YYYYMM)
class(ex$HME_YYYYMM)
addDate <- paste(ex$HME_YYYYMM, "01", sep = "")
head(addDate)
ex$HME_YYYYMM <- as.Date(addDate, format = "%Y%m%d")
head(ex$HME_YYYYMM)
class(ex$HME_YYYYMM)

### 결측치 다루기 --------------------------------------------------------------------
head(ex$LDL)
tapply(ex$LDL, ex$EXMD_BZ_YYYY, mean)
tapply(ex$LDL, ex$EXMD_BZ_YYYY, function(x){mean(x, na.rm = T)})

# R은 분석에서 결측치를 제외함
summary(lm(LDL ~ HDL, data = ex))

### 연습문제 2: 결측치 처리 --------------------------------------------------------------------
ex.naomit <- na.omit(ex)
nrow(ex.naomit)
nrow(ex)
 
### 결측치 처리 3가지 원칙 --------------------------------------------------------------
# 1. 결측치 너무 많으면(예: 10% 이상) 그 변수는 삭제
# 2. 연속변수는 중간값(median)
# 3. 범주형변수는 최빈값(mode)

getMode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getMode(ex$Q_PHX_DX_STK)

# 1. 결측치 너무 많으면(예: 10% 이상) 그 변수는 삭제
vars.ok <- sapply(names(ex), function(v){sum(is.na(ex[, v])) < nrow(ex)/10})
ex.impute <- ex[, vars.ok]

# 2. 연속변수는 중간값(median)
# 3. 범주형변수는 최빈값(mode)
for (v in names(ex.impute)){
  if (is.factor(ex.impute[, v])){
    ex.impute[, v] <- ifelse(is.na(ex.impute[, v]), getMode(ex.impute[, v]), ex.impute[, v])
  } else if (is.numeric(ex[, v])){
    ex.impute[]
  }
}
