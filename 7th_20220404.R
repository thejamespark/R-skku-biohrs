#install.packages("rcompanion")
library(data.table);library(magrittr);library(ggpubr)
set.seed(222)

## T-test
data.t <- data.frame(sex = sample(c("Male", "Female"), 30, replace = T), tChol = round(rnorm(30, mean = 150, sd = 30)))
rownames(data.t) <- paste("person", 1:30)
data.t

# var.equal = F 가 디폴트(생략가능)
nev.ttest <- t.test(tChol ~ sex, data = data.t, var.equal = F);nev.ttest

# 등분산 Test 할 필요없다.(var.equal = T 할 필요없음)
# 아무 근거없이 분산이 같다고 가정하는 것은 위험한 가정이다
ev.ttest <- t.test(tChol ~ sex, data = data.t, var.equal = T);ev.ttest

# p-value만 
t.test(tChol ~ sex, data = data.t)$p.value

# ggarrange: 여러개의 그림을 한 화면에
ggarrange(
  ggboxplot(data.t, "sex", "tChol", fill = "sex"),
  ggbarplot(data.t, "sex", "tChol", fill = "sex", add = "mean_sd")
)

# 그림 그릴때는 var.equal = T 가 디폴트라 var.equal = F 써줘야 함
ggboxplot(data.t, "sex", "tChol", fill = "sex", add = "dotplot") + 
  stat_compare_means(method = "t.test", method.args = list(var.equal = F))


ggviolin(data.t, "sex", "tChol", fill = "sex", add = "boxplot") + 
  stat_compare_means(method = "t.test", method.args = list(var.equal = T), label.y = 250)


## Wilcox
res.wilcox <- wilcox.test(tChol ~ sex, data = data.t);res.wilcox

ggboxplot(data.t, "sex", "tChol", fill = "sex") + 
  stat_compare_means(method = "wilcox.test")


# 3그룹 이상인데, order 가 있다? --> trend test 사용

# 3그룹 이상: one-way ANOVA
## ANOVA
data.aov <- data.frame(group = sample(c("A", "B", "C"), 30, replace = T), tChol = round(rnorm(30, mean = 150, sd = 30)))
rownames(data.aov) <- paste("person", 1:30)
data.aov

res.aov1 <- oneway.test(tChol ~ group, data = data.aov, var.equal = F);res.aov1
res.aov2 <- oneway.test(tChol ~ group, data = data.aov, var.equal = T);res.aov2

ggboxplot(data.aov, "group", "tChol", fill = "group", order = c("A", "B", "C")) + 
  stat_compare_means(method = "anova")


ggboxplot(data.aov, "group", "tChol", fill = "group", order = c("A", "B", "C")) + 
  stat_compare_means(method = "anova", label.y = 250) + 
  stat_compare_means(method = "t.test", comparisons = list(c("A", "B"), c("B", "C"), c("C", "A")))

## Kruskal test
res.kruskal <- kruskal.test(tChol ~ group, data = data.aov);res.kruskal

ggboxplot(data.aov, "group", "tChol", fill = "group", order = c("A", "B", "C")) + 
  stat_compare_means(method = "kruskal.test")


## Categorical: Chisq test
data.chi <- data.frame(HTN_medi = round(rbinom(50, 1, 0.4)), DM_medi = round(rbinom(50, 1, 0.4)))
rownames(data.chi) <- paste("person", 1:50)
data.chi

tb.chi <- table(data.chi);tb.chi
res.chi <- chisq.test(tb.chi);res.chi

## Fisher
data.fisher <- data.frame(HTN_medi = round(rbinom(50, 1, 0.2)), DM_medi = round(rbinom(50, 1, 0.2)))
rownames(data.fisher) <- paste("person", 1:50)

tb.fisher <- table(data.fisher);tb.fisher
chisq.test(tb.fisher)
res.fisher <- fisher.test(tb.fisher);res.fisher


## Paired test: continuous
data.pt <- data.frame(SBP_hand = round(rnorm(30, mean = 125, sd = 5)), SBP_machine = round(rnorm(30, mean = 125, sd = 5)))
rownames(data.pt) <- paste("person", 1:30)

pt.ttest <- t.test(data.pt$SBP_hand, data.pt$SBP_machine);pt.ttest
pt.ttest.pair <- t.test(data.pt$SBP_hand, data.pt$SBP_machine, paired = T);pt.ttest.pair

ggpaired(data.pt, cond1 = "SBP_hand", cond2 = "SBP_machine", fill = "condition", palette = "jco") + 
  stat_compare_means(method = "t.test", paired = T)

pt.wilcox.pair <- wilcox.test(data.pt$SBP_hand, data.pt$SBP_machine, paired = T);pt.wilcox.pair

ggpaired(data.pt, cond1 = "SBP_hand", cond2 = "SBP_machine", fill = "condition", palette = "jco") + 
  stat_compare_means(method = "wilcox.test", paired = T)


## Paired test: categorical
data.mc <- data.frame(Pain_before = round(rbinom(30, 1, 0.5)), Pain_after = round(rbinom(30, 1, 0.5)))
rownames(data.mc) <- paste("person", 1:30)

# 참고로, 교수님은 mcnemar test를 별로 안좋아한다.
table.mc <- table(data.mc);table.mc
mc.chi <- chisq.test(table.mc);mc.chi
mc.mcnemar <- mcnemar.test(table.mc);mc.mcnemar


## Paired test: >= 3 category
library(rcompanion)
data(AndersonRainGarden)  # Example data
AndersonRainGarden
nominalSymmetryTest(AndersonRainGarden)


# repeated measure ANOVA 는 거의 쓰지 않는다.
# 짝지은 3개 그룹 자체가 흔하지 않다.


# tableone
# install.packages("data.table")
# install.packages("curl")
# install.packages("tableone")
library(tableone)
url <- "https://raw.githubusercontent.com/jinseob2kim/lecture-snuhlab/master/data/example_g1e.csv"
dt <- fread(url,header=T)

vars.tb1 <- names(dt)[-c(1:3)]
CreatTableOne(vars.tb1, data = dt)

# factorVars 를 쓸 필요없게 하는 전처리
for (v in grep("Q_", vars.tb1, value = T)){
  dt[[v]] <- factor(dt[[v]])
}

zz <- CreatTableOne(vars.tb1, data = dt, strata = "Q_PHX_DX_STK")
write.csv(print(zz), "tb1.csv", row.names = T)

## gtsummary
#install.packages("gtsummary")
library(gtsummary)
tbl_summary(dt[, ..vars.tb1])


# save as word file
#installpackages("flextable")
MyTbl %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(MyTbl, path = "/cloud/project/table.docx")

# 중간고사
# 1. data management (범주형 변수 변환 for 문)
# 2. 그룹별통계랑 (aggregation, groupby summary)
# 3. ggplot 그래프 그리기
