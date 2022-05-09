# ggpubr package
# install.packages(c("ggpubr", "rvg", "officer"))
library(ggpubr)
# Save plots
library(rvg); library(officer)

## histogram
data3 <- data2 %>% mutate(HTN = as.factor(ifelse(Q_PHX_DX_HTN==1, "Yes", "No")))

gghistogram(data=data3, x="WGHT",
            color="HTN", fill = "HTN", add="mean")

p <- gghistogram(data=data3, x="WGHT",
                 color="HTN", fill = "HTN", add="mean")
plot1 <- ggpar(p,
               main="Weight distrubution by HTN history",
               xlab="Weight(kg)",
               legend.title="HTN Dx history")
print(plot1)

## box plot
ggboxplot(data=data3, x="HTN", y="WGHT", color="HTN")

# T-test, p-value 추가
ggboxplot(data=data3, x="HTN", y="WGHT", color="HTN") +
  stat_compare_means(method = "t.test", label.x.npc = "middle")

p <- ggboxplot(data=data3, x="HTN", y="WGHT", color="HTN") +
  stat_compare_means(method = "t.test", label.x.npc = "middle")

plot2 <- ggpar(p,
               main="Weight distrubution by HTN history",
               ylab="Weight(kg)",
               xlab="HTN Dx history",
               legend="none")
print(plot2)

# 3그룹 이상
my_comparisons <- list(c("1", "2"), c("2", "3"), c("1", "3"))

ggboxplot(data=data3, x="Q_SMK_YN", y="WGHT", color="Q_SMK_YN")

ggboxplot(data=data3, x="Q_SMK_YN", y="WGHT", color="Q_SMK_YN") +
  stat_compare_means(comparisons = my_comparisons) +
  stat_compare_means(label.y = 150) +
  scale_x_discrete(labels=c("Never", "Ex-smoker", "Current"))

p <- ggboxplot(data=data3, x="Q_SMK_YN", y="WGHT", color="Q_SMK_YN") +
  stat_compare_means(comparisons = my_comparisons) +
  stat_compare_means(label.y = 150) +
  scale_x_discrete(labels=c("Never", "Ex-smoker", "Current"))

plot3 <- ggpar(p,
               main="Weight distrubution by smoking",
               ylab="Weight(kg)",
               xlab="Smoking",
               legend="none")
print(plot3)

## scatter plot
ggscatter(data=data3, x="HGHT", y="WGHT")

# stat_cor: correlation
ggscatter(data=data3, x="HGHT", y="WGHT", 
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "navy", fill = "lightgray")) +
  stat_cor(method = "pearson")

p <- ggscatter(data=data3, x="HGHT", y="WGHT", 
               add = "reg.line", conf.int = TRUE,
               add.params = list(color = "navy", fill = "lightgray")) +
  stat_cor(method = "pearson")

plot4 <- ggpar(p,
               ylab="Weight(kg)",
               xlab="Height(cm)")
print(plot4)

p <- ggscatter(data=data3, x="HGHT", y="WGHT", color="HTN", alpha=0.5,
               add = "reg.line", conf.int = TRUE) +
  stat_cor(aes(color = HTN))
plot5 <- ggpar(p,
               ylab="Weight(kg)",
               xlab="Height(cm)")
print(plot5)

# facet.by = 도 추가 가능

# line
ggline(data3, x = "EXMD_BZ_YYYY", y = "GGT", add = "median")
ggbarplot(data3, x = "EXMD_BZ_YYYY", y = "GGT", add = "mean_se")

## ggarange
ggarrange(plot2, plot3,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

# Save plots
ggsave("zzz.pdf", plot = p, width = 10, height = 7, units= "in", dpi = 600)


# ppt로 저장 --> 강추!!!!
# 그림을 하나하나 다 수정 가능!!
# ggplot 으로 그린 그림
library(rvg); library(officer)

plot_file <- read_pptx() %>%
  add_slide() %>% ph_with(dml(ggobj = plot1), location=ph_location_type(type="body")) %>%
  add_slide() %>% ph_with(dml(ggobj = plot4), location=ph_location_type(type="body")) %>%
  add_slide() %>% ph_with(dml(ggobj = plot5), location=ph_location_type(type="body"))

print(plot_file, target = "plot_file.pptx")

help(ph_with)
help(dml)

# ggplot 이 아닌 base R 로 그린 그림은 dml(code = hist~~~)를 써야함
plot_file <- read_pptx() %>%
  add_slide() %>% ph_with(dml(code = hist(data$HGHT, main="Distribution of height", xlab="height(cm)")), location=ph_location_type(type="body"))

# 대신 너무 복잡해지니까 hist~~ 를 함수로 저장해서 dml(code = pp()) 형식으로 함수 로드 가능
pp <- function(){
  hist(data$HGHT, main="Distribution of height", xlab="height(cm)")
}

plot_file <- read_pptx() %>%
  add_slide() %>% ph_with(dml(code = pp()), location=ph_location_type(type="body"))

print(plot_file, target = "plot_file1.pptx")
