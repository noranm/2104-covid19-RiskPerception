# first EDA R code
# 각 그룹 별 시계열 그림

library(readxl)
library(ggplot2)
library(dplyr)
#library(ggpmisc)

risk <- data.frame(read_excel("./data/covid_risk.xlsx", sheet=1))
risk_summary <- data.frame(read_excel("./data/covid_summary.xlsx"))

head(risk)
# interested in : BK5, BK6
risk_BK5 <- risk %>% select(DATE, ARA, AGE, SEX, BK5, SAGE, WT2, JOB, XD2, XD3, BK9, BQ1, BQ3)
risk_BK5$DATE <- as.Date(as.character(risk$DATE), "%y%m%d")
risk_BK5$WEEK <- cut(as.Date(as.character(risk$DATE), "%y%m%d"), breaks="week")
risk_BK5$WEEK2 <- as.Date(risk_BK5$WEEK)

week <- unique(risk_BK5$WEEK2)

risk_BK5$BK5[risk_BK5$BK5==9999] <- NA

head(risk_BK5)
risk_BK5$WORRY <- as.numeric(risk_BK5$BK5 == 1)
risk_BK5$WORRY2 <- risk_BK5$WORRY  * risk_BK5$WT2

theme_set(theme_bw())

# group by SEX :
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

# group by SEX :
BK5_AGE <- risk_BK5 %>% group_by(WEEK2, AGE) %>% summarize(BK5Sum = sum(WORRY2, na.rm = TRUE),
                                                           wtSum = sum(WT2, na.rm = TRUE))

BK5_AGE$m_BK5 <- BK5_AGE$BK5Sum/BK5_AGE$wtSum
BK5_AGE$AGE <- factor(BK5_AGE$AGE, levels=c(1,2,3,4,5), labels=c("18-29", "30-39", "40-49", "50-59", "60+"))

ggplot(BK5_AGE, aes(x = WEEK2, y = m_BK5)) + geom_line(aes(color = factor(AGE)), na.rm = TRUE, size = 1) + 
  scale_colour_brewer(palette="Set2") +
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week2, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01))+ 
  ggtitle("Worried about COVID-19 infection by AGE (%)") + xlab("date (week)") + ylab("Worry (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) 

# group by ARA :
BK5_ARA <- risk_BK5 %>% group_by(WEEK2, ARA) %>% summarize(BK5Sum = sum(WORRY2, na.rm = TRUE),
                                                           wtSum = sum(WT2, na.rm=TRUE))
BK5_ARA$m_BK5 <- BK5_ARA$BK5Sum/BK5_ARA$wtSum
BK5_ARA$ARA <- factor(BK5_ARA$ARA, levels=c(1,2,3,4,5,6,7,8), 
                      labels=c("서울","인천/경기","강원","대전/충청/세종","광주/전라","대구/경북","부산/울산/경남","제주"))

ggplot(BK5_ARA, aes(x = WEEK2, y = m_BK5)) + geom_line(aes(color = factor(ARA)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week2, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set1") + 
  ggtitle("Worried about COVID-19 infection by area (%)") + xlab("date (week)") + ylab("Worry (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) 

# group by SAGE :
BK5_SAGE <- risk_BK5 %>% group_by(WEEK2, SAGE, SEX, AGE) %>% summarize(BK5Sum = sum(WORRY2, na.rm = TRUE),
                                                                       wtSum = sum(WT2, na.rm=TRUE))
BK5_SAGE$m_BK5 <- BK5_SAGE$BK5Sum/BK5_SAGE$wtSum

BK5_SAGE$SAGE <- factor(BK5_SAGE$SAGE, levels=c(11:15,21:25), 
                        labels=c("남자:20대","남자:30대","남자:40대","남자:50대","남자:60대+",
                                 "여자:20대","여자:30대","여자:40대","여자:50대","여자:60대+"))
BK5_SAGE$AGE <- factor(BK5_SAGE$AGE, levels=c(1,2,3,4,5), labels=c("18-29", "30-39", "40-49", "50-59", "60+"))
BK5_SAGE$SEX <- factor(BK5_SAGE$SEX, levels=c(1,2), labels=c("Male", "Female"))

ggplot(BK5_SAGE, aes(x = WEEK2, y = m_BK5)) + geom_line(aes(color = factor(AGE)), na.rm = TRUE, size = 1)+
  scale_colour_brewer(palette="Set2") +
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week2, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) + 
  ggtitle("Worried about COVID-19 infection by sex, age (%)") + xlab("date (week)") + ylab("Worry (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) + facet_grid(SEX ~ ., scales = "free_y", space = "free_y") 

# group by JOB : 
BK5_JOB <- risk_BK5 %>% group_by(WEEK2, JOB) %>% summarize(BK5Sum = sum(WORRY2, na.rm = TRUE),
                                                           wtSum = sum(WT2, na.rm=TRUE))
BK5_JOB$m_BK5 <- BK5_JOB$BK5Sum/BK5_JOB$wtSum
BK5_JOB$JOB <- factor(BK5_JOB$JOB, levels=c(1,2,3,4,5,6,7), 
                      labels=c("농/임/어업","자영업","기능노무/서비스","사무/관리", "전업주부","학생","무직/기타"))

ggplot(BK5_JOB, aes(x = WEEK2, y = m_BK5)) + geom_line(aes(color = factor(JOB)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week2, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set1") + 
  ggtitle("Worried about COVID-19 infection by job (%)") + xlab("date (week)") + ylab("Worry (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) 

# group by XD2 : 
BK5_XD2 <- risk_BK5 %>% group_by(WEEK2, XD2) %>% summarize(BK5Sum = sum(WORRY2, na.rm = TRUE),
                                                           wtSum = sum(WT2, na.rm=TRUE))
BK5_XD2$m_BK5 <- BK5_XD2$BK5Sum/BK5_XD2$wtSum
BK5_XD2$m_BK5[BK5_XD2$XD2==9999] <- NA
BK5_XD2$XD2 <- factor(BK5_XD2$XD2, levels=c(1,2,3,9999), 
                      labels=c("보수","중도","진보","모름/응답거절"))

ggplot(BK5_XD2, aes(x = WEEK2, y = m_BK5)) + geom_line(aes(color = factor(XD2)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week2, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set1") + 
  ggtitle("Worried about COVID-19 infection by political tendency (%)") + xlab("date (week)") + ylab("Worry (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) 

# group by XD3 : 
BK5_XD3 <- risk_BK5 %>% group_by(WEEK2, XD3) %>% summarize(BK5Sum = sum(WORRY2, na.rm = TRUE),
                                                           wtSum = sum(WT2, na.rm=TRUE))
BK5_XD3$m_BK5 <- BK5_XD3$BK5Sum/BK5_XD3$wtSum
BK5_XD3$m_BK5[BK5_XD3$XD3==9999] <- NA
BK5_XD3$XD3 <- factor(BK5_XD3$XD3, levels=c(1,3,4,5,9999), 
                      labels=c("상/중상", "중", "중하", "하", "모름/응답거절"))

ggplot(BK5_XD3, aes(x = WEEK2, y = m_BK5)) + geom_line(aes(color = factor(XD3)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week2, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set1") + 
  ggtitle("Worried about COVID-19 infection by standard of living (%)") + xlab("date (week)") + ylab("Worry (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) 

# group by BQ3 : 
BK5_BQ3 <- risk_BK5 %>% group_by(WEEK2, BQ3) %>% summarize(BK5Sum = sum(WORRY2, na.rm = TRUE),
                                                           wtSum = sum(WT2, na.rm=TRUE))
BK5_BQ3$m_BK5 <- BK5_BQ3$BK5Sum/BK5_BQ3$wtSum
BK5_BQ3$m_BK5[!BK5_BQ3$BQ3 %in% c(1,2,3,4,5)] <- NA
BK5_BQ3$BQ3 <- factor(BK5_BQ3$BQ3, levels=c(1,2,3,4,5, 35,37,39, 51,53,54,55,56, 9997, 9998, 9999), 
                      labels=c( "국민의당", "국민의힘", "더불어민주당", "열린민주당", "정의당",
                                "기타", "기타", "기타" , "기타", "기타", "기타", "기타", "기타" , "기타", "기타", "기타"))

ggplot(BK5_BQ3, aes(x = WEEK2, y = m_BK5)) + geom_line(aes(color = factor(BQ3)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week2, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set2") + 
  ggtitle("Worried about COVID-19 infection by party (%)") + xlab("date (week)") + ylab("Worry (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) 

# group by BQ1 : 
BK5_BQ1 <- risk_BK5 %>% group_by(WEEK2, BQ1) %>% summarize(BK5Sum = sum(WORRY2, na.rm = TRUE),
                                                           wtSum = sum(WT2, na.rm=TRUE))
BK5_BQ1$m_BK5 <- BK5_BQ1$BK5Sum/BK5_BQ1$wtSum
BK5_BQ1$m_BK5[BK5_BQ1$BQ1 == 9999] <- NA
BK5_BQ1$BQ1 <- factor(BK5_BQ1$BQ1, levels=c(1,2,3, 9999), 
                      labels=c( "잘 하고 있다", "못 하고 있다", "어느 쪽도 아니다", "모름/응답거절"))

ggplot(BK5_BQ1, aes(x = WEEK2, y = m_BK5)) + geom_line(aes(color = factor(BQ1)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week2, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set2") + 
  ggtitle("Worried about COVID-19 infection by evaluation (%)") + xlab("date (week)") + ylab("Worry (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) 

# group by 주관적 경제수준(XD3) x 정치적 성향(XD2)
BK5_XD23 <- risk_BK5 %>% group_by(WEEK2, XD3, XD2) %>% summarize(BK5Sum = sum(WORRY2, na.rm = TRUE),
                                                                 wtSum = sum(WT2, na.rm=TRUE))

BK5_XD23$m_BK5 <- BK5_XD23$BK5Sum/BK5_XD23$wtSum
BK5_XD23_2 <- BK5_XD23[BK5_XD23$XD2 != 9999,]
BK5_XD23_3 <- BK5_XD23_2[BK5_XD23_2$XD3 != 9999,]

BK5_XD23_3$XD2 <- factor(BK5_XD23_3$XD2, levels=c(1,2,3,9999), 
                      labels=c("보수","중도","진보","모름/응답거절"))
BK5_XD23_3$XD3 <- factor(BK5_XD23_3$XD3, levels=c(1,3,4,5,9999), 
                      labels=c("상/중상", "중", "중하", "하", "모름/응답거절"))

ggplot(BK5_XD23_3, aes(x = WEEK2, y = m_BK5)) + geom_line(aes(color = factor(XD3)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set1") + 
  ggtitle("Worried about COVID-19 infection (%)") + xlab("date (week)") + ylab("Worry (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) + 
  facet_grid(XD2 ~ ., scales = "free_y", space = "free_y") 

ggplot(BK5_XD23_3, aes(x = WEEK2, y = m_BK5)) + geom_line(aes(color = factor(XD2)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set1") + 
  ggtitle("Worried about COVID-19 infection (%)") + xlab("date (week)") + ylab("Worry (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) + 
  facet_grid(XD3 ~ ., scales = "free_y", space = "free_y") 

# interested in : BK5, BK6
risk_BK6 <- risk %>% select(DATE, ARA, AGE, SEX, BK6, SAGE, WT2, JOB, XD2, XD3, BK9, BQ1, BQ3)
risk_BK6$DATE <- as.Date(as.character(risk$DATE), "%y%m%d")
risk_BK6$WEEK <- cut(as.Date(as.character(risk$DATE), "%y%m%d"), breaks="week")
risk_BK6$WEEK2 <- as.Date(risk_BK6$WEEK)

week <- unique(risk_BK6$WEEK2)

risk_BK6$BK6[risk_BK6$BK6==9999] <- NA
table(risk_BK6$BK6)
head(risk_BK6)
risk_BK6$Likelihood <- as.numeric(risk_BK6$BK6 == 1)
risk_BK6$Likelihood2 <- risk_BK6$Likelihood  * risk_BK6$WT2

theme_set(theme_bw())

# group by SEX :
BK6_SEX <- risk_BK6 %>% group_by(WEEK2, SEX) %>% summarize(BK6Sum = sum(Likelihood2, na.rm = TRUE),
                                                           wtSum = sum(WT2, na.rm = TRUE))
BK6_SEX$m_BK6 <- BK6_SEX$BK6Sum/BK6_SEX$wtSum
# 대구 신천지 집단 감염 시작 20.02.19
# 이태원 확진 20.05.08 & 20.05.13
# 광화문 집회 20.08.15
BK6_SEX$SEX <- factor(BK6_SEX$SEX, levels=c(1,2), labels=c("Male", "Female"))

ggplot(BK6_SEX, aes(x = WEEK2, y = m_BK6)) + geom_line(aes(color = factor(SEX)), na.rm = TRUE, size = 1)+
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week2,date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +
  ggtitle("Likelihood about COVID-19 infection by sex (%)") + xlab("date (week)") + ylab("Likelihood (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) 

# group by SEX :
BK6_AGE <- risk_BK6 %>% group_by(WEEK2, AGE) %>% summarize(BK6Sum = sum(Likelihood2, na.rm = TRUE),
                                                           wtSum = sum(WT2, na.rm = TRUE))

BK6_AGE$m_BK6 <- BK6_AGE$BK6Sum/BK6_AGE$wtSum
BK6_AGE$AGE <- factor(BK6_AGE$AGE, levels=c(1,2,3,4,5), labels=c("18-29", "30-39", "40-49", "50-59", "60+"))

ggplot(BK6_AGE, aes(x = WEEK2, y = m_BK6)) + geom_line(aes(color = factor(AGE)), na.rm = TRUE, size = 1) + 
  scale_colour_brewer(palette="Set2") +
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week2, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01))+ 
  ggtitle("Likelihood about COVID-19 infection by AGE (%)") + xlab("date (week)") + ylab("Likelihood (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) 

# group by ARA :
BK6_ARA <- risk_BK6 %>% group_by(WEEK2, ARA) %>% summarize(BK6Sum = sum(Likelihood2, na.rm = TRUE),
                                                           wtSum = sum(WT2, na.rm=TRUE))
BK6_ARA$m_BK6 <- BK6_ARA$BK6Sum/BK6_ARA$wtSum
BK6_ARA$ARA <- factor(BK6_ARA$ARA, levels=c(1,2,3,4,5,6,7,8), 
                      labels=c("서울","인천/경기","강원","대전/충청/세종","광주/전라","대구/경북","부산/울산/경남","제주"))

ggplot(BK6_ARA, aes(x = WEEK2, y = m_BK6)) + geom_line(aes(color = factor(ARA)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week2, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set1") + 
  ggtitle("Likelihood about COVID-19 infection by area (%)") + xlab("date (week)") + ylab("Likelihood (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) 

# group by SAGE :
BK6_SAGE <- risk_BK6 %>% group_by(WEEK2, SAGE, SEX, AGE) %>% summarize(BK6Sum = sum(Likelihood2, na.rm = TRUE),
                                                                       wtSum = sum(WT2, na.rm=TRUE))
BK6_SAGE$m_BK6 <- BK6_SAGE$BK6Sum/BK6_SAGE$wtSum

BK6_SAGE$SAGE <- factor(BK6_SAGE$SAGE, levels=c(11:15,21:25), 
                        labels=c("남자:20대","남자:30대","남자:40대","남자:50대","남자:60대+",
                                 "여자:20대","여자:30대","여자:40대","여자:50대","여자:60대+"))
BK6_SAGE$AGE <- factor(BK6_SAGE$AGE, levels=c(1,2,3,4,5), labels=c("18-29", "30-39", "40-49", "50-59", "60+"))
BK6_SAGE$SEX <- factor(BK6_SAGE$SEX, levels=c(1,2), labels=c("Male", "Female"))

ggplot(BK6_SAGE, aes(x = WEEK2, y = m_BK6)) + geom_line(aes(color = factor(AGE)), na.rm = TRUE, size = 1)+
  scale_colour_brewer(palette="Set2") +
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week2, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) + 
  ggtitle("Likelihood about COVID-19 infection by sex, age (%)") + xlab("date (week)") + ylab("Likelihood (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) + facet_grid(SEX ~ ., scales = "free_y", space = "free_y") 

# group by JOB : 
BK6_JOB <- risk_BK6 %>% group_by(WEEK2, JOB) %>% summarize(BK6Sum = sum(Likelihood2, na.rm = TRUE),
                                                           wtSum = sum(WT2, na.rm=TRUE))
BK6_JOB$m_BK6 <- BK6_JOB$BK6Sum/BK6_JOB$wtSum
BK6_JOB$JOB <- factor(BK6_JOB$JOB, levels=c(1,2,3,4,5,6,7), 
                      labels=c("농/임/어업","자영업","기능노무/서비스","사무/관리", "전업주부","학생","무직/기타"))

ggplot(BK6_JOB, aes(x = WEEK2, y = m_BK6)) + geom_line(aes(color = factor(JOB)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week2, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set1") + 
  ggtitle("Likelihood about COVID-19 infection by job (%)") + xlab("date (week)") + ylab("Likelihood (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) 

# group by XD2 : 
BK6_XD2 <- risk_BK6 %>% group_by(WEEK2, XD2) %>% summarize(BK6Sum = sum(Likelihood2, na.rm = TRUE),
                                                           wtSum = sum(WT2, na.rm=TRUE))
BK6_XD2$m_BK6 <- BK6_XD2$BK6Sum/BK6_XD2$wtSum
BK6_XD2$m_BK6[BK6_XD2$XD2==9999] <- NA
BK6_XD2$XD2 <- factor(BK6_XD2$XD2, levels=c(1,2,3,9999), 
                      labels=c("보수","중도","진보","모름/응답거절"))

ggplot(BK6_XD2, aes(x = WEEK2, y = m_BK6)) + geom_line(aes(color = factor(XD2)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week2, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set1") + 
  ggtitle("Likelihood about COVID-19 infection by political tendency (%)") + xlab("date (week)") + ylab("Likelihood (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) 

# group by XD3 : 
BK6_XD3 <- risk_BK6 %>% group_by(WEEK2, XD3) %>% summarize(BK6Sum = sum(Likelihood2, na.rm = TRUE),
                                                           wtSum = sum(WT2, na.rm=TRUE))
BK6_XD3$m_BK6 <- BK6_XD3$BK6Sum/BK6_XD3$wtSum
BK6_XD3$m_BK6[BK6_XD3$XD3==9999] <- NA
BK6_XD3$XD3 <- factor(BK6_XD3$XD3, levels=c(1,3,4,5,9999), 
                      labels=c("상/중상", "중", "중하", "하", "모름/응답거절"))

ggplot(BK6_XD3, aes(x = WEEK2, y = m_BK6)) + geom_line(aes(color = factor(XD3)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week2, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set1") + 
  ggtitle("Likelihood about COVID-19 infection by standard of living (%)") + xlab("date (week)") + ylab("Likelihood (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) 

# group by BQ3 : 
BK6_BQ3 <- risk_BK6 %>% group_by(WEEK2, BQ3) %>% summarize(BK6Sum = sum(Likelihood2, na.rm = TRUE),
                                                           wtSum = sum(WT2, na.rm=TRUE))
BK6_BQ3$m_BK6 <- BK6_BQ3$BK6Sum/BK6_BQ3$wtSum
BK6_BQ3$m_BK6[!BK6_BQ3$BQ3 %in% c(1,2,3,4,5)] <- NA
BK6_BQ3$BQ3 <- factor(BK6_BQ3$BQ3, levels=c(1,2,3,4,5, 35,37,39, 51,53,54,55,56, 9997, 9998, 9999), 
                      labels=c( "국민의당", "국민의힘", "더불어민주당", "열린민주당", "정의당",
                                "기타", "기타", "기타" , "기타", "기타", "기타", "기타", "기타" , "기타", "기타", "기타"))

ggplot(BK6_BQ3, aes(x = WEEK2, y = m_BK6)) + geom_line(aes(color = factor(BQ3)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week2, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set2") + 
  ggtitle("Likelihood about COVID-19 infection by party (%)") + xlab("date (week)") + ylab("Likelihood (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) 

# group by BQ1 : 
BK6_BQ1 <- risk_BK6 %>% group_by(WEEK2, BQ1) %>% summarize(BK6Sum = sum(Likelihood2, na.rm = TRUE),
                                                           wtSum = sum(WT2, na.rm=TRUE))
BK6_BQ1$m_BK6 <- BK6_BQ1$BK6Sum/BK6_BQ1$wtSum
BK6_BQ1$m_BK6[BK6_BQ1$BQ1 == 9999] <- NA
BK6_BQ1$BQ1 <- factor(BK6_BQ1$BQ1, levels=c(1,2,3, 9999), 
                      labels=c( "잘 하고 있다", "못 하고 있다", "어느 쪽도 아니다", "모름/응답거절"))

ggplot(BK6_BQ1, aes(x = WEEK2, y = m_BK6)) + geom_line(aes(color = factor(BQ1)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set2") + 
  ggtitle("Likelihood about COVID-19 infection by evaluation (%)") + xlab("date (week)") + ylab("Likelihood (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) 

# group by 주관적 경제수준(XD3) x 정치적 성향(XD2)
BK6_XD23 <- risk_BK6 %>% group_by(WEEK2, XD3, XD2) %>% summarize(BK6Sum = sum(Likelihood2, na.rm = TRUE),
                                                                 wtSum = sum(WT2, na.rm=TRUE))

BK6_XD23$m_BK6 <- BK6_XD23$BK6Sum/BK6_XD23$wtSum
BK6_XD23_2 <- BK6_XD23[BK6_XD23$XD2 != 9999,]
BK6_XD23_3 <- BK6_XD23_2[BK6_XD23_2$XD3 != 9999,]

BK6_XD23_3$XD2 <- factor(BK6_XD23_3$XD2, levels=c(1,2,3,9999), 
                         labels=c("보수","중도","진보","모름/응답거절"))
BK6_XD23_3$XD3 <- factor(BK6_XD23_3$XD3, levels=c(1,3,4,5,9999), 
                         labels=c("상/중상", "중", "중하", "하", "모름/응답거절"))

ggplot(BK6_XD23_3, aes(x = WEEK2, y = m_BK6)) + geom_line(aes(color = factor(XD3)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set1") + 
  ggtitle("Likelihood about COVID-19 infection (%)") + xlab("date (week)") + ylab("Likelihood (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) + 
  facet_grid(XD2 ~ ., scales = "free_y", space = "free_y") 

ggplot(BK6_XD23_3, aes(x = WEEK2, y = m_BK6)) + geom_line(aes(color = factor(XD2)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set1") + 
  ggtitle("Likelihood about COVID-19 infection (%)") + xlab("date (week)") + ylab("Likelihood (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508"),"%Y%m%d"), linetype=2) + 
  facet_grid(XD3 ~ ., scales = "free_y", space = "free_y") 
