# ggplot package
library(ggplot2)

## scatter plot
# 'data=' 안써도 된다.
ggplot(data2, aes(x=HGHT, y=WGHT, col=factor(EXMD_BZ_YYYY)))
ggplot(data=data2, aes(x=HGHT, y=WGHT, col=factor(EXMD_BZ_YYYY)))

# theme change
ggplot(data=data2, aes(x=HGHT, y=WGHT, col=factor(EXMD_BZ_YYYY))) +
  geom_point() +
  theme_classic()

# color (factor를 꼭 써야한다)
ggplot(data=data2, aes(x=HGHT, y=WGHT, col=factor(EXMD_BZ_YYYY))) +
  geom_point()

# scale_color_manual()
ggplot(data=data2, aes(x=HGHT, y=WGHT, col=factor(EXMD_BZ_YYYY))) +
  geom_point() +
  ggtitle("Height and weight in year 2009 and 2015") + xlab("Height(cm)") + ylab("Weight(cm)") +
  scale_color_manual(
    values = c("orange", "skyblue"),
    labels = c("Year 2009", "Year 2015"),
    name = "Exam year")

# alpha: 투명도, size: 선의 굵기
ggplot(data=data2, aes(x=HGHT, y=WGHT)) +
  geom_point(aes(col=factor(EXMD_BZ_YYYY)), alpha=0.5) +
  ggtitle("Height and weight in year 2009 and 2015") + xlab("Height(cm)") + ylab("Weight(cm)") +
  scale_color_manual(
    values = c("orange", "skyblue"),
    labels = c("Year 2009", "Year 2015"),
    name = "Exam year") +
  geom_smooth(color="brown", size=0.8)
# scale_XXXX_manual() : XXXX자리에 color, x, y 등이 올 수 있다

# method = 'lm'
ggplot(data=data2, aes(x=HGHT, y=WGHT)) +
  geom_point(aes(col=factor(EXMD_BZ_YYYY)), alpha=0.5) +
  ggtitle("Height and weight in year 2009 and 2015") + xlab("Height(cm)") + ylab("Weight(cm)") +
  scale_color_manual(
    values = c("orange", "skyblue"),
    labels = c("Year 2009", "Year 2015"),
    name = "Exam year") +
  geom_smooth(color="brown", size=0.8, method = 'lm')

## box plot
data2 <- data %>% filter(!is.na(Q_SMK_YN))
ggplot(data=data2, aes(x=factor(Q_SMK_YN), y=BP_SYS)) +
  geom_boxplot() +
  ggtitle("SBP average by smoking") + ylab("SBP(mmHg)") + xlab("Smoking") +
  scale_x_discrete(labels=c("Never", "Ex-smoker", "Current"))   # 연속변수면 scale_x_continuous

# facet_wrap : 분할
data2 <- data2 %>% filter(!is.na(Q_PHX_DX_HTN))
ggplot(data=data2, aes(x=factor(Q_SMK_YN), y=BP_SYS)) +
  geom_boxplot() +
  ggtitle("SBP average by smoking") + ylab("SBP(mmHg)") + xlab("Smoking") +
  scale_x_discrete(labels=c("Never", "Ex-smoker", "Current")) +
  facet_wrap(~Q_PHX_DX_HTN, labeller=label_both)

# facet_grid : 분할
data2 <- data2 %>% filter(!is.na(Q_PHX_DX_DM))
HTN.labs <- c("No HTN", "HTN")
names(HTN.labs) <- c("0", "1")
DM.labs <- c("No DM", "DM")
names(DM.labs) <- c("0", "1")
ggplot(data=data2, aes(x=factor(Q_SMK_YN), y=BP_SYS)) +
  geom_boxplot() +
  ggtitle("SBP average by smoking") + ylab("SBP(mmHg)") + xlab("Smoking") +
  scale_x_discrete(labels=c("Never", "Ex-smoker", "Current")) +
  facet_grid(Q_PHX_DX_DM~Q_PHX_DX_HTN,
             labeller = labeller(Q_PHX_DX_HTN = HTN.labs, Q_PHX_DX_DM = DM.labs))

## bar plot
ggplot(data=data2, aes(x=factor(Q_SMK_YN))) +
  geom_bar(fill="grey", color="black") +
  ggtitle("Distribution of smoking") + xlab("Smoking") +
  scale_x_discrete(labels=c("Never", "Ex-smoker", "Current"))

ggplot(data=data2, aes(x=EXMD_BZ_YYYY, fill=factor(Q_SMK_YN))) +
  geom_bar(position="fill", color="grey") +
  ggtitle("Distribution of smoking by year") + xlab("Year") + ylab("proportion") +
  scale_fill_manual(
    values = c("orange", "skyblue", "navy"),
    labels = c("Never", "Ex-smoker", "Current"),
    name = "Smoking") +
  scale_x_continuous(breaks=2009:2015)

# position = "dodge"
# coord_flip()
ggplot(data=data2, aes(x=EXMD_BZ_YYYY, fill=factor(Q_SMK_YN))) +
  geom_bar(position="dodge", color="grey") +
  ggtitle("Distribution of smoking by year") + xlab("Year") + ylab("count") +
  scale_fill_manual(
    values = c("orange", "skyblue", "navy"),
    labels = c("Never", "Ex-smoker", "Current"),
    name = "Smoking") +
  scale_x_continuous(breaks=2009:2015) +
  coord_flip()

# Line Plot
ggplot(data=data2, aes(x=HGHT, y=WGHT)) +
  geom_line(aes(col=factor(EXMD_BZ_YYYY)), alpha=0.5)

# Line + Point
ggplot(data=data2, aes(x=HGHT, y=WGHT)) +
  geom_line(aes(col=factor(EXMD_BZ_YYYY)), alpha=0.5) +
  geom_point()

