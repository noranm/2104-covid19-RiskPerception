library(readxl)
library(ggplot2)
library(dplyr)
#library(ggpmisc)

risk <- data.frame(read_excel("./data/covid_risk.xlsx", sheet=1))
risk_summary <- data.frame(read_excel("./data/covid_summary.xlsx"))

head(risk)
# interested in : BK5, BK6
risk_BK5 <- risk %>% select(DATE, ARA, AGE, SEX, BK5, SAGE, WT2)
risk_BK5$DATE <- as.Date(as.character(risk$DATE), "%y%m%d")
risk_BK5$WEEK <- cut(as.Date(as.character(risk$DATE), "%y%m%d"), breaks="week")
risk_BK5$WEEK2 <- as.Date(risk_BK5$WEEK, "%y%m%d")

week <- unique(risk_BK5$WEEK2)

risk_BK5$BK5[risk_BK5$BK5==9999] <- NA
table(risk_BK5$BK5)
head(risk_BK5)
risk_BK5$WORRY <- as.numeric(risk_BK5$BK5 == 1)

theme_set(theme_bw())
# group by SEX :
risk_BK5$WORRY2 <- risk_BK5$WORRY  * risk_BK5$WT2
BK5_SEX <- risk_BK5 %>% group_by(WEEK2, SEX) %>% summarize(BK5Sum = sum(WORRY2, na.rm = TRUE),
                                                           wtSum = sum(WT2, na.rm = TRUE))
BK5_SEX$m_BK5 <- BK5_SEX$BK5Sum/BK5_SEX$wtSum
# 대구 신천지 집단 감염 시작 20.02.19
# 이태원 확진 20.05.08 & 20.05.13
# 광화문 집회 20.08.15
BK5_SEX$SEX <- factor(BK5_SEX$SEX, levels=c(1,2), labels=c("Male", "Female"))

ggplot(BK5_SEX, aes(x = WEEK2, y = m_BK5)) + geom_line(aes(color = factor(SEX)), na.rm = TRUE, size = 1)+
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week2,date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +
  ggtitle("Worried about COVID-19 infection by sex (%)") + xlab("date (week)") + ylab("Worry (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) 
  
#  annotate("text", x = as.Date("2020-02-19"), y = 0.1, label = "02-19") +
#  annotate("text", x = as.Date("2020-05-04"), y = 0.1, label = "05-04")

# group by AGE :
BK5_AGE <- risk_BK5 %>% group_by(WEEK2, AGE) %>% summarize(m_BK5 = mean(WORRY2, na.rm = TRUE))
BK5_AGE$AGE <- factor(BK5_AGE$AGE, levels=c(1,2,3,4,5), labels=c("18-29", "30-39", "40-49", "50-59", "60+"))

ggplot(BK5_AGE, aes(x = WEEK2, y = m_BK5)) + geom_line(aes(color = factor(AGE)), na.rm = TRUE, size = 1)+
  scale_colour_brewer(palette="Set2") + 
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  scale_y_continuous(limits=c(0,1.05)) +
  ggtitle("Worried about COVID-19 infection by age(%)") + xlab("date (week)") + ylab("Worry (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2)

# group by ARA :
BK5_ARA <- risk_BK5 %>% group_by(WEEK2, ARA) %>% summarize(m_BK5 = mean(WORRY2, na.rm = TRUE))
BK5_ARA$ARA <- factor(BK5_ARA$ARA, levels=c(1,2,3,4,5,6,7,8), 
                         labels=c("서울","인천/경기","강원","대전/충청/세종","광주/전라","대구/경북","부산/울산/경남","제주"))

ggplot(BK5_ARA, aes(x = WEEK2, y = m_BK5)) + geom_line(aes(color = factor(ARA)), na.rm = TRUE, size = 1)+
  scale_colour_brewer(palette="Set2") + 
  theme(legend.position = "bottom", legend.title = element_blank()) + 
#  scale_y_continuous(limits=c(0,1.05)) +
  ggtitle("Worried about COVID-19 infection by ARA(%)") + xlab("date (week)") + ylab("Worry (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2)

# group by SAGE :
BK5_SAGE <- risk_BK5 %>% group_by(WEEK2, SAGE, SEX, AGE) %>% summarize(m_BK5 = mean(WORRY2, na.rm = TRUE))
BK5_SAGE$SAGE <- factor(BK5_SAGE$SAGE, levels=c(11:15,21:25), 
                      labels=c("남자:20대","남자:30대","남자:40대","남자:50대","남자:60대+",
                               "여자:20대","여자:30대","여자:40대","여자:50대","여자:60대+"))
BK5_SAGE$AGE <- factor(BK5_SAGE$AGE, levels=c(1,2,3,4,5), labels=c("18-29", "30-39", "40-49", "50-59", "60+"))
BK5_SAGE$SEX <- factor(BK5_SAGE$SEX, levels=c(1,2), labels=c("Male", "Female"))

ggplot(BK5_SAGE, aes(x = WEEK2, y = m_BK5)) + geom_line(aes(color = factor(AGE)), na.rm = TRUE, size = 1)+
  scale_colour_brewer(palette="Set2") + 
  theme(legend.position = "bottom", legend.title = element_blank()) + 
#  scale_y_continuous(limits=c(0,1.2)) +
  ggtitle("Worried about COVID-19 infection by sex, age (%)") + xlab("date (week)") + ylab("Worry (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) + facet_grid(SEX ~ ., scales = "free_y", space = "free_y") 

