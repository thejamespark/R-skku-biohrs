library(survey)
library(dplyr)
a <- haven::read_sav("data/hn20_all.sav")
a

## See variable label
names(a)

# spss 파일에서는 Label을 볼 수 있다
a$sex
a$age
# 모든 변수의 label 확인
sapply(a, function(x){attr(x, "label")})

## For practice: some variable
a <- haven::read_sav("data/hn20_all.sav", col_select = c("year", "sex", "age", "allownc", "ainc", "marri_1", "edu", "EC1_1", 
                                                         "HE_sbp", "HE_dbp", "HE_glu", "HE_HCHOL", "HE_BMI", "psu", "kstrata", "wt_itvex", "wt_tot"))

# label 확인
sapply(a, function(x){attr(x, "label")})

# 범주형 변수
var.factor <- c("sex", "edu", "allownc", "marri_1", "EC1_1", "HE_HCHOL")
for (v in var.factor){
  a[[v]] <- factor(a[[v]])
}
summary(a)

# 결측치 처리도 해야한다
# NA's, 9, 99, 8(응답없음) 등등


## survey object
# 미국 국건영 데이터도 똑같이 ids, strata, weights 
# 이건 에러날거다
data.design <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_itvex, data = a)  ## Error: missing in weights
# wt_itvex 변수에서 결측치를 날려버리고 실행
# filter 함수 대신 subset을 사용했음
data.design <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_itvex, data = subset(a, !is.na(wt_itvex)))
data.design
data.design$variables     # 원래 데이터 확인

# 이제부터 분석 시작 가능 (data.design 만들어야 분석 가능)

## Table 1
#library(tableone)
library(jstable)  # tableone 패키지를 교수님이 수정한 패키지

# 이렇게 하면 안된다.
CreateTableOneJS(vars = setdiff(names(a), c("year", "kstrata", "psu", "wt_itvex", "wt_tot")), strata = "sex", data = subset(a, !is.na(wt_itvex)))

tb1 <- CreateTableOneJS(vars = setdiff(names(a), c("year", "kstrata", "psu", "wt_itvex", "wt_tot")), strata = "sex", data = subset(a, !is.na(wt_itvex)))
tb1$table %>% write.csv("tb1.csv")

# 또는
vars_tb1 <- setdiff(names(a), c("year", "kstrata", "psu", "wt_itvex", "wt_tot"))
CreateTableOneJS(vars = vars_tb1, strata = "sex", data = a)

# 국건영 데이터는 위에걸로 하면 안되고, 이걸로 해야 한다.
svyCreateTableOneJS(vars = setdiff(names(a), c("year", "kstrata", "psu", "wt_itvex", "wt_tot")), strata = "sex", data = data.design)
svyCreateTableOneJS(vars = vars_tb1, strata = "sex", data = data.design)


## Regression
# 국건영 데이터는 이렇게 하면 안된다
reg1 <- glm(HE_glu ~ age + sex + HE_BMI +  edu + marri_1, data = a)
glmshow.display(reg1)

# 국건영 데이터는 이걸 써야한다
reg2 <- svyglm(HE_glu ~ age + sex + HE_BMI + edu + marri_1, design = data.design)
svyregress.display(reg2)

## Logistic
# 0, 1 데이터 분석
# 국건영 데이터는 이렇게 하면 안된다
reg1 <- glm(HE_HCHOL ~ age + sex + HE_BMI +  edu + marri_1, data = a, family = binomial)
glmshow.display(reg1)

# 국건영 데이터는 이걸 써야한다
# family = quasibinomial()
reg2 <- svyglm(HE_HCHOL ~ age + sex + HE_BMI + edu + marri_1, design = data.design, family = quasibinomial())
svyregress.display(reg2)


