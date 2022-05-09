library(dplyr); library(ggplot2); library(ggpubr)

# Load data
data <- read.csv("https://raw.githubusercontent.com/jinseob2kim/R-skku-biohrs/main/data/example_g1e.csv")
head(data)

# 화면 분할해서 그림 여러개 동시에 그리기
par(mfrow = c(1, 2))

# Base package
## histogram
hist(data$HGHT, main="Distribution of height", xlab="height(cm)")

# freq = F --> y축이 density (다 더하면 1)
hist(data$HGHT, main="Distribution of height", xlab="height(cm)",
     breaks = 30, freq=F, col="grey", border="white")

## bar plot
table <- table(data$Q_SMK_YN)
print(table)
barplot(table, main="Distribution of smoking", names.arg=c("Never", "Ex-smoker", "Current"), ylab="frequency")

table2 <- table(data$Q_SMK_YN, data$EXMD_BZ_YYYY)
print(table2)
barplot(table2, main="Distribution of smoking by year", ylab="frequency",
        legend=c("Never", "Ex-smoker", "Current"))

barplot(table2, main="Distribution of smoking by year", ylab="frequency",
        legend=c("Never", "Ex-smoker", "Current"), beside=T)

## box plot
boxplot(BP_SYS ~ Q_SMK_YN, data = data, names=c("Never", "Ex-smoker", "Current"), 
        main="SBP average by smoking", ylab="SBP(mmHg)", xlab="Smoking")

## scatter plot
plot(HGHT ~ WGHT, data=data,
     ylab="Height(cm)", xlab="Weight(kg)",
     pch=16, cex=0.5)

# same
plot(data$WGHT, data$HGHT,
     ylab="Height(cm)", xlab="Weight(kg)",
     pch=16, cex=0.5)

# color
data2 <- data %>% filter(EXMD_BZ_YYYY %in% c(2009, 2015))
plot(HGHT ~ WGHT, data=data2, col=factor(EXMD_BZ_YYYY),
     ylab="Height(cm)", xlab="Weight(kg)",
     pch=16, cex=0.5)
legend(x="bottomright", legend=c("2009", "2015"), col=1:2, pch = 19)

## line plot
table3 <- data %>% group_by(EXMD_BZ_YYYY) %>% 
  summarize(smoker= mean(Q_SMK_YN==3, na.rm=T))
print(table3)

# type = "l": line
plot(table3$EXMD_BZ_YYYY, table3$smoker, type="l",
     xlab="Year", ylab="prop of current smoker")

# type = "p": point
plot(table3$EXMD_BZ_YYYY, table3$smoker, type="p",
     xlab="Year", ylab="prop of current smoker")

# type = "b": both line and poing
plot(table3$EXMD_BZ_YYYY, table3$smoker, type="b",
     xlab="Year", ylab="prop of current smoker")

# save file
help(jpeg)

# 저장하겠다고 선언 후 그림 그리고 dev.off()
jpeg("test.jpg", unit = "in", width = 10, height = 7, res = 600) # res = 해상도
hist(data$HGHT, main="Distribution of height", xlab="height(cm)")
dev.off()
