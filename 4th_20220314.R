# imported from /code/datatable.R
# 편의성보다는 빠른 실행 속도를 위한 방법
# 공단, 심평원 데이터를 다루기 위해서는 필수!
# 앞의 방법과 문법이 또 달라서 난이도가 조금 높을 수 있다.
library(data.table); library(magrittr)

getDTthreads()
setDTthreads(0)   # core 다 쓰겠다. (병렬로 데이터 불러들임)
getDTthreads()

## Exam data: 09-15
df <- read.csv("https://raw.githubusercontent.com/jinseob2kim/lecture-snuhlab/master/data/example_g1e.csv")
df %>% 
  head

# fread 가 가장 빠르게 데이터를 불러올 수 있는 방법이다
dt <- fread("https://raw.githubusercontent.com/jinseob2kim/lecture-snuhlab/master/data/example_g1e.csv")
class(df); class(dt)

# Save file
write.csv(dt, "as.csv", row.names = F)
fwrite(dt, "exam0915.csv")

# Only specific column
#dt <- fread("https://raw.githubusercontent.com/jinseob2kim/lecture-snuhlab/master/data/example_g1e.csv", select = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM"))
#dt <- fread("https://raw.githubusercontent.com/jinseob2kim/lecture-snuhlab/master/data/example_g1e.csv", select = 1:5)
#dt <- fread("https://raw.githubusercontent.com/jinseob2kim/lecture-snuhlab/master/data/example_g1e.csv", drop = 6:10)


## Row 
dt[1:10]    # or dt[1:10, ]

# data.frame의 subset과 동일
# df[(df$EXMD_BZ_YYYY %in% 2009:2012) & (df$BMI >= 25), ]
dt[(EXMD_BZ_YYYY %in% 2009:2012) & (BMI >= 25)]

dt[order(HME_YYYYMM)]
dt[order(HME_YYYYMM, -HGHT)]
dt[(EXMD_BZ_YYYY %in% 2009:2012) & (BMI >= 25)][order(HGHT)]        ## chain
dt[(EXMD_BZ_YYYY %in% 2009:2012) & (BMI >= 25)] %>% .[order(HGHT)]  ## same


## Column
dt[, 1:10]
dt[, c("HGHT", "WGHT")]
dt[, .(HGHT, WGHT)]   # dt[, list(HGHT, WGHT)], 여기서의 . 은 list 의 생략
dt[, .(Height = HGHT, Weight = WGHT)]   # rename

dt[, .(HGHT)] 
dt[, "HGHT"]
dt[, HGHT]   ## vector


colvars <- grep("Q_", names(dt), value = T)
dt[, ..colvars]           # 이걸 제일 많이 사용. data.frame 은 df[, colvars]
dt[, colvars, with = F]   # 이제는 잘 안쓴다.
dt[, .SD, .SDcols = colvars]   # .SD : Subset of Data, 모든 컬럼을 의미한다


dt[(EXMD_BZ_YYYY %in% 2009:2012) & (BMI >= 25), ..colvars]

# 해당 변수 제외
dt[, !..colvars]        
dt[, -..colvars]
dt[, .SD, .SDcols = -colvars]

## Column summary
dt[, .(mean(HGHT), mean(WGHT), mean(BMI))]
dt[, .(HGHT = mean(HGHT), WGHT = mean(WGHT), BMI = mean(BMI))]
dt[, lapply(.SD, mean), .SDcols = c("HGHT", "WGHT", "BMI")]

## by : 그룹별 요약통계량 구할 때
dt[, .(HGHT = mean(HGHT), WGHT = mean(WGHT), BMI = mean(BMI)), by = EXMD_BZ_YYYY]
dt[, .(HGHT = mean(HGHT), WGHT = mean(WGHT), BMI = mean(BMI)), by = "EXMD_BZ_YYYY"]
dt[, lapply(.SD, mean), .SDcols = c("HGHT", "WGHT", "BMI"), by = EXMD_BZ_YYYY]
dt[, lapply(.SD, mean), by = EXMD_BZ_YYYY, .SDcols = c("HGHT", "WGHT", "BMI")]    # by와 .SDcols 는 순서가 바껴도 된다

dt[HGHT >= 175, .N, by= .(EXMD_BZ_YYYY, Q_SMK_YN)]        # .N: special symbol
dt[HGHT >= 175, .N, by= c("EXMD_BZ_YYYY", "Q_SMK_YN")]

dt[HGHT >= 175, .N, keyby= c("EXMD_BZ_YYYY", "Q_SMK_YN")] ## Keyby: keep order

dt[HGHT >= 175, .N, keyby= .(EXMD_BZ_YYYY >= 2015, Q_PHX_DX_STK == 1)]
dt[HGHT >= 175, .N, keyby= .(get("EXMD_BZ_YYYY") >= 2015, get("Q_PHX_DX_STK") == 1)]
dt[HGHT >= 175, .N, keyby= .(Y2015 = ifelse(EXMD_BZ_YYYY >= 2015, ">=2015", "<2015"))]

# keyby 를 쓰면 알아서 메모리에 저장해주기 때문에 setkey() 를 쓸일이 없다.
# 또한 다른 사람이 코드 읽기에 불편

## Merge example
dt1 <- dt[1:10, .SD, .SDcols = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM", colvars)]
dt2 <- dt[6:15, -..colvars]

merge(dt1, dt2, by = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM"), all = T)    # Full join
merge(dt1, dt2, by = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM"), all = F)    # Inner join, all = F 생략 가능 (즉, all의 default=F)
merge(dt1, dt2, by = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM"), all.x = T)  # left join
merge(dt1, dt2, by = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM"), all.y = T)  # right join


dt2[dt1, on = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM")]  # data.frame 에서의 left join
dt1[dt2, on = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM")]  # data.frame 에서의 right join

# merge 함수의 anti join 은 없는 것으로 파악됨
# 따라서 아래의 방법으로 써야함
dt1[!dt2, on = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM")] # left anti join
dt2[!dt1, on = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM")] # right anti join


## New variable 
## data.frame 에서 새로운 변수 만드는 법: dt$BMI2 <- dt$WGHT/(dt$HGHT/100)^2
dt[, BMI2 := round(WGHT/(HGHT/100)^2, 1)]
dt[, `:=`(BP_SYS140 = factor(as.integer(BP_SYS >= 140)), BMI25 = factor(as.integer(BMI >= 25)))]
dt[, BMI2 := NULL]                                         # remove


## Specific symbol .N, .SD, .SDcols
dt[, .SD]   # all column
dt[, lapply(.SD, class)]   # 모든 변수의 클래스

dt[order(EXMD_BZ_YYYY), .SD[1], keyby = "RN_INDI"]     # 이거 보다는 HME_YYYYMM (방문이력)으로 설명하는 게 더 좋겠다.
dt[order(EXMD_BZ_YYYY), .SD[.N], keyby = "RN_INDI"]

# EXMD_BZ_YYYY --> HME_YYYY
dt[order(HME_YYYYMM), .SD[1], keyby = "RN_INDI"]  # 사람(RN_INDI)별 방문이력(HME_YYYYMM)으로 정렬한 후 첫 번째 진단일을 보여줌 
dt[order(HME_YYYYMM), .SD[.N], keyby = "RN_INDI"]  # 사람(RN_INDI)별 방문이력(HME_YYYYMM)으로 정렬한 후 마지막 진단일을 보여줌 

dt[order(EXMD_BZ_YYYY), .SD[1], .SDcols = colvars, keyby = "RN_INDI"]
dt[, .N, keyby = "RN_INDI"]


## Melt(wide to long), dcast(long to wide)
dt.long1 <- melt(dt, 
                 id.vars = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM"),   ## keep variable
                 measure.vars = c("TOT_CHOL", "TG", "HDL", "LDL"),       ## wide to long variable
                 variable.name = "Lipid",                                ## name
                 value.name = "Value")

# Enhanced melt: multiple group. 많이 쓴다
col1 <- c("BP_SYS", "BP_DIA")
col2 <- c("VA_LT", "VA_RT")
dt.long2 <- melt(dt,
                 id.vars = c("EXMD_BZ_YYYY", "RN_INDI", "HME_YYYYMM"),
                 measure = list(col1, col2),
                 value.name = c("BP", "VA"))

# 변수 이름 설정
dt.long2[, variable := factor(variable, labels = c("SBP/VA_LT", 'DBP/VA_RT'))]


# dcast: long to wide (melt의 반대. 실무에서는 반복 측정 변수를 내리기 위해 melt를 더 많이 쓴다)
dt.wide1 <- dcast(dt.long1, EXMD_BZ_YYYY + RN_INDI + HME_YYYYMM ~ Lipid, value.var = "Value")

# aggregate
dt.wide2 <- dcast(dt.long1, RN_INDI ~ Lipid, value.var = "Value", fun.aggregate = mean, na.rm =T)

# Enhanced dcast
dt.wide3 <- dcast(dt.long2, ... ~ variable, value.var = c("BP", "VA"))    # ... : 나와있는 변수(variable, BP, VA)를 제외한 나머지








