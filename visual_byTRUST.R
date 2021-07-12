# 2nd EDA
# 국정 지지도에 따라~
library(readxl)
library(ggplot2)
library(dplyr)
library(gridExtra)

risk <- data.frame(read_excel("./data/covid_risk.xlsx", sheet=1))
risk_summary <- data.frame(read_excel("./data/covid_summary.xlsx"))

head(risk)
# interested in : BK5, BK6
# Presidential job approval rating (Trust 지표)
# (1) Approval (2) Disapproval (3) Neutral
risk_TRUST <- risk %>% select(DATE, ARA, AGE, SEX, BK5, BK6, SAGE, WT2, JOB, XD2, XD3, BQ1, BQ3)
risk_TRUST$DATE <- as.Date(as.character(risk$DATE), "%y%m%d")
risk_TRUST$WEEK <- cut(as.Date(as.character(risk$DATE), "%y%m%d"), breaks="week")
risk_TRUST$WEEK2 <- as.Date(risk_TRUST$WEEK)

risk_TRUST$BK5[risk_TRUST$BK5==9999] <- NA
risk_TRUST$Anxiety <- as.numeric(risk_TRUST$BK5 == 1)
risk_TRUST$Anxiety2 <- risk_TRUST$Anxiety  * risk_TRUST$WT2

risk_TRUST$BK6[risk_TRUST$BK6==9999] <- NA
risk_TRUST$Likelihood <- as.numeric(risk_TRUST$BK6 == 1)
risk_TRUST$Likelihood2 <- risk_TRUST$Likelihood  * risk_TRUST$WT2

week <- unique(risk_TRUST$WEEK2)
theme_set(theme_bw())
head(risk_TRUST)

# group by SEX : 
TRUST_SEX <- risk_TRUST %>% group_by(WEEK2, BQ1, SEX) %>% summarize(anxSum = sum(Anxiety2, na.rm = TRUE),
                                                                  likSum = sum(Likelihood2, na.rm = TRUE),
                                                                  wtSum = sum(WT2, na.rm=TRUE))

TRUST_SEX$m_anx <- TRUST_SEX$anxSum/TRUST_SEX$wtSum
TRUST_SEX$m_lik <- TRUST_SEX$likSum/TRUST_SEX$wtSum


TRUST_SEX2 <- TRUST_SEX[TRUST_SEX$BQ1 != 9999,]
TRUST_SEX2$BQ1 <- factor(TRUST_SEX2$BQ1, levels=c(1,2,3, 9999), 
                      labels=c( "Approval", "Dispproval", "Neutral", "No Opinion"))
TRUST_SEX2$SEX <- factor(TRUST_SEX2$SEX, levels=c(1,2), labels=c("Male", "Female"))

sex_a = ggplot(TRUST_SEX2, aes(x = WEEK2, y = m_anx)) + geom_line(aes(color = factor(SEX)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set2") + 
  ggtitle("Affective about COVID-19 infection (%)") + xlab("date (week)") + ylab("Anxiety (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508","20200815"),"%Y%m%d"), linetype=2) + 
  facet_grid(BQ1 ~ ., scales = "free_y", space = "free_y") 

sex_c = ggplot(TRUST_SEX2, aes(x = WEEK2, y = m_lik)) + geom_line(aes(color = factor(SEX)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set2") + 
  ggtitle("Cognitive about COVID-19 infection (%)") + xlab("date (week)") + ylab("Likelihood (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508","20200815"),"%Y%m%d"), linetype=2) + 
  facet_grid(BQ1 ~ ., scales = "free_y", space = "free_y") 

grid.arrange(sex_a, sex_c, nrow=1, ncol=2)

# group by AGE : 
TRUST_AGE <- risk_TRUST %>% group_by(WEEK2, BQ1, AGE) %>% summarize(anxSum = sum(Anxiety2, na.rm = TRUE),
                                                                    likSum = sum(Likelihood2, na.rm = TRUE),
                                                                    wtSum = sum(WT2, na.rm=TRUE))

TRUST_AGE$m_anx <- TRUST_AGE$anxSum/TRUST_AGE$wtSum
TRUST_AGE$m_lik <- TRUST_AGE$likSum/TRUST_AGE$wtSum

TRUST_AGE2 <- TRUST_AGE[TRUST_AGE$BQ1 != 9999,]
TRUST_AGE2$BQ1 <- factor(TRUST_AGE2$BQ1, levels=c(1,2,3, 9999), 
                         labels=c( "Approval", "Dispproval", "Neutral", "No Opinion"))
TRUST_AGE2$AGE <- factor(TRUST_AGE2$AGE, levels=c(1,2,3,4,5), labels=c("18-29", "30-39", "40-49", "50-59", "60+"))

age_a <- ggplot(TRUST_AGE2, aes(x = WEEK2, y = m_anx)) + geom_line(aes(color = factor(AGE)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set2") + 
  ggtitle("Affective about COVID-19 infection (%)") + xlab("date (week)") + ylab("Anxiety (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508","20200815"),"%Y%m%d"), linetype=2) + 
  facet_grid(BQ1 ~ ., scales = "free_y", space = "free_y") 

age_c <- ggplot(TRUST_AGE2, aes(x = WEEK2, y = m_lik)) + geom_line(aes(color = factor(AGE)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set2") + 
  ggtitle("Cognitive about COVID-19 infection (%)") + xlab("date (week)") + ylab("Likelihood (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508","20200815"),"%Y%m%d"), linetype=2) + 
  facet_grid(BQ1 ~ ., scales = "free_y", space = "free_y") 

grid.arrange(age_a, age_c, nrow=1, ncol=2)

# group by Occupation (직업) : JOB
TRUST_JOB <- risk_TRUST %>% group_by(WEEK2, BQ1, JOB) %>% summarize(anxSum = sum(Anxiety2, na.rm = TRUE),
                                                                    likSum = sum(Likelihood2, na.rm = TRUE),
                                                                    wtSum = sum(WT2, na.rm=TRUE))

TRUST_JOB$m_anx <- TRUST_JOB$anxSum/TRUST_JOB$wtSum
TRUST_JOB$m_lik <- TRUST_JOB$likSum/TRUST_JOB$wtSum

TRUST_JOB2 <- TRUST_JOB[TRUST_JOB$BQ1 != 9999,]
TRUST_JOB2$BQ1 <- factor(TRUST_JOB2$BQ1, levels=c(1,2,3, 9999), 
                         labels=c( "Approval", "Dispproval", "Neutral", "No Opinion"))
TRUST_JOB2$JOB <- factor(TRUST_JOB2$JOB, levels=c(7, 1, 2, 3, 4, 5, 6), 
                         labels=c("Unemployed/ETC", "Farming/Forestry/Fishery", "Self-employed", "Blue-collar", "White-collar", "Home maker and Student", "Home maker and Student"))

job_a <- ggplot(TRUST_JOB2, aes(x = WEEK2, y = m_anx)) + geom_line(aes(color = factor(JOB)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set2") + 
  ggtitle("Affective about COVID-19 infection (%)") + xlab("date (week)") + ylab("Anxiety (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508","20200815"),"%Y%m%d"), linetype=2) + 
  facet_grid(BQ1 ~ ., scales = "free_y", space = "free_y") 

job_c <- ggplot(TRUST_JOB2, aes(x = WEEK2, y = m_lik)) + geom_line(aes(color = factor(JOB)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set2") + 
  ggtitle("Cognitive about COVID-19 infection (%)") + xlab("date (week)") + ylab("Likelihood (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508","20200815"),"%Y%m%d"), linetype=2) + 
  facet_grid(BQ1 ~ ., scales = "free_y", space = "free_y") 

grid.arrange(job_a, job_c, nrow=1, ncol=2)

# group by household status : XD3
TRUST_household <- risk_TRUST %>% group_by(WEEK2, BQ1, XD3) %>% summarize(anxSum = sum(Anxiety2, na.rm = TRUE),
                                                                    likSum = sum(Likelihood2, na.rm = TRUE),
                                                                    wtSum = sum(WT2, na.rm=TRUE))

TRUST_household$m_anx <- TRUST_household$anxSum/TRUST_household$wtSum
TRUST_household$m_lik <- TRUST_household$likSum/TRUST_household$wtSum

TRUST_household2 <- TRUST_household[TRUST_household$BQ1 != 9999,]
TRUST_household2$BQ1 <- factor(TRUST_household2$BQ1, levels=c(1,2,3, 9999), 
                         labels=c( "Approval", "Dispproval", "Neutral", "No Opinion"))
TRUST_household2$household <- factor(TRUST_household2$XD3, levels=c(1, 3, 4, 5, 9999), 
                         labels=c("Upper/Upper Middle", "Middle", "Lower Middle/Lower", "Lower Middle/Lower", "No opinion"))

household_a <- ggplot(TRUST_household2, aes(x = WEEK2, y = m_anx)) + geom_line(aes(color = factor(household)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set2") + 
  ggtitle("Affective about COVID-19 infection (%)") + xlab("date (week)") + ylab("Anxiety (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508","20200815"),"%Y%m%d"), linetype=2) + 
  facet_grid(BQ1 ~ ., scales = "free_y", space = "free_y") 

household_c <- ggplot(TRUST_household2, aes(x = WEEK2, y = m_lik)) + geom_line(aes(color = factor(household)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set2") + 
  ggtitle("Cognitive about COVID-19 infection (%)") + xlab("date (week)") + ylab("Likelihood (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508","20200815"),"%Y%m%d"), linetype=2) + 
  facet_grid(BQ1 ~ ., scales = "free_y", space = "free_y") 

grid.arrange(household_a, household_c, nrow=1, ncol=2)

# group by AREA : ARA
TRUST_AREA <- risk_TRUST %>% group_by(WEEK2, BQ1, ARA) %>% summarize(anxSum = sum(Anxiety2, na.rm = TRUE),
                                                                          likSum = sum(Likelihood2, na.rm = TRUE),
                                                                          wtSum = sum(WT2, na.rm=TRUE))

TRUST_AREA$m_anx <- TRUST_AREA$anxSum/TRUST_AREA$wtSum
TRUST_AREA$m_lik <- TRUST_AREA$likSum/TRUST_AREA$wtSum

TRUST_AREA2 <- TRUST_AREA[TRUST_AREA$BQ1 != 9999,]
TRUST_AREA2$BQ1 <- factor(TRUST_AREA2$BQ1, levels=c(1,2,3, 9999), 
                               labels=c( "Approval", "Dispproval", "Neutral", "No Opinion"))
# "Index"=c("수도권(서울/인천/경기)", "충청권(대전/충청/세종)", "호남권(광주/전라)", "영남권(대구/경북/경남/부산/울산)",  "기타(강원/제주)" ),
# "cnt"=c( sum(df.each$ARA %in% c(1,2) ) , sum(df.each$ARA==4), sum(df.each$ARA==5), sum(df.each$ARA %in% c(6,7) ), sum(df.each$ARA %in% c(3,8) ) ),

TRUST_AREA2$AREA <- factor(TRUST_AREA2$ARA, levels=c(1, 2, 4, 5, 6, 7, 3, 8), 
                                     labels=c("수도권(서울/인천/경기)", "수도권(서울/인천/경기)", 
                                              "충청권(대전/충청/세종)", 
                                              "호남권(광주/전라)", 
                                              "영남권(대구/경북/경남/부산/울산)", "영남권(대구/경북/경남/부산/울산)", 
                                              "기타(강원/제주)", "기타(강원/제주)" ))

AREA_a <- ggplot(TRUST_AREA2, aes(x = WEEK2, y = m_anx)) + geom_line(aes(color = factor(AREA)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set2") + 
  ggtitle("Affective about COVID-19 infection (%)") + xlab("date (week)") + ylab("Anxiety (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508","20200815"),"%Y%m%d"), linetype=2) + 
  facet_grid(BQ1 ~ ., scales = "free_y", space = "free_y") 

AREA_c <- ggplot(TRUST_AREA2, aes(x = WEEK2, y = m_lik)) + geom_line(aes(color = factor(AREA)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set2") + 
  ggtitle("Cognitive about COVID-19 infection (%)") + xlab("date (week)") + ylab("Likelihood (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508","20200815"),"%Y%m%d"), linetype=2) + 
  facet_grid(BQ1 ~ ., scales = "free_y", space = "free_y") 

grid.arrange(AREA_a, AREA_c, nrow=1, ncol=2)

# group by AREA : ARA
TRUST_AREA <- risk_TRUST %>% group_by(WEEK2, BQ1, ARA) %>% summarize(anxSum = sum(Anxiety2, na.rm = TRUE),
                                                                     likSum = sum(Likelihood2, na.rm = TRUE),
                                                                     wtSum = sum(WT2, na.rm=TRUE))

TRUST_AREA$m_anx <- TRUST_AREA$anxSum/TRUST_AREA$wtSum
TRUST_AREA$m_lik <- TRUST_AREA$likSum/TRUST_AREA$wtSum

TRUST_AREA2 <- TRUST_AREA[TRUST_AREA$BQ1 != 9999,]
TRUST_AREA2$BQ1 <- factor(TRUST_AREA2$BQ1, levels=c(1,2,3, 9999), 
                          labels=c( "Approval", "Dispproval", "Neutral", "No Opinion"))
# "Index"=c("수도권(서울/인천/경기)", "충청권(대전/충청/세종)", "호남권(광주/전라)", "영남권(대구/경북/경남/부산/울산)",  "기타(강원/제주)" ),
# "cnt"=c( sum(df.each$ARA %in% c(1,2) ) , sum(df.each$ARA==4), sum(df.each$ARA==5), sum(df.each$ARA %in% c(6,7) ), sum(df.each$ARA %in% c(3,8) ) ),

TRUST_AREA2$AREA <- factor(TRUST_AREA2$ARA, levels=c(1, 2, 4, 5, 6, 7, 3, 8), 
                           labels=c("수도권(서울/인천/경기)", "수도권(서울/인천/경기)", 
                                    "충청권(대전/충청/세종)", 
                                    "호남권(광주/전라)", 
                                    "영남권(대구/경북/경남/부산/울산)", "영남권(대구/경북/경남/부산/울산)", 
                                    "기타(강원/제주)", "기타(강원/제주)" ))

AREA_a <- ggplot(TRUST_AREA2, aes(x = WEEK2, y = m_anx)) + geom_line(aes(color = factor(AREA)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set2") + 
  ggtitle("Affective about COVID-19 infection (%)") + xlab("date (week)") + ylab("Anxiety (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508","20200815"),"%Y%m%d"), linetype=2) + 
  facet_grid(BQ1 ~ ., scales = "free_y", space = "free_y") 

AREA_c <- ggplot(TRUST_AREA2, aes(x = WEEK2, y = m_lik)) + geom_line(aes(color = factor(AREA)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set2") + 
  ggtitle("Cognitive about COVID-19 infection (%)") + xlab("date (week)") + ylab("Likelihood (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508","20200815"),"%Y%m%d"), linetype=2) + 
  facet_grid(BQ1 ~ ., scales = "free_y", space = "free_y") 

grid.arrange(AREA_a, AREA_c, nrow=1, ncol=2)

# group by PARTY : 
#party <- data.frame("Index"=c("conservative", "neutral", "progressive", "No opinion"),
#                    "cnt"=c(sum(df.each$XD2==1), sum(df.each$XD2==2), sum(df.each$XD2==3), sum(df.each$XD2==9999)),
#                    "weighted cnt"=c( sum(as.numeric(df.each$XD2==1) * df.each$WT2), sum(as.numeric(df.each$XD2==2) * df.each$WT2), sum(as.numeric(df.each$XD2==3) * df.each$WT2), sum(as.numeric(df.each$XD2==9999) * df.each$WT2))  )

TRUST_party <- risk_TRUST %>% group_by(WEEK2, BQ1, XD2) %>% summarize(anxSum = sum(Anxiety2, na.rm = TRUE),
                                                                     likSum = sum(Likelihood2, na.rm = TRUE),
                                                                     wtSum = sum(WT2, na.rm=TRUE))

TRUST_party$m_anx <- TRUST_party$anxSum/TRUST_party$wtSum
TRUST_party$m_lik <- TRUST_party$likSum/TRUST_party$wtSum

TRUST_party2 <- TRUST_party[TRUST_party$BQ1 != 9999,]
TRUST_party2$BQ1 <- factor(TRUST_party2$BQ1, levels=c(1,2,3, 9999), 
                          labels=c( "Approval", "Dispproval", "Neutral", "No Opinion"))

TRUST_party2$party <- factor(TRUST_party2$XD2, levels=c(1, 3, 2, 9999), 
                             labels=c("conservative", "progressive", "neutral", "No opinion") )


party_a <- ggplot(TRUST_party2, aes(x = WEEK2, y = m_anx)) + geom_line(aes(color = factor(party)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set1") + 
  ggtitle("Affective about COVID-19 infection (%)") + xlab("date (week)") + ylab("Anxiety (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508","20200815"),"%Y%m%d"), linetype=2) + 
  facet_grid(BQ1 ~ ., scales = "free_y", space = "free_y") 

party_c <- ggplot(TRUST_party2, aes(x = WEEK2, y = m_lik)) + geom_line(aes(color = factor(party)), na.rm = TRUE, size = 1)+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, size=6)) + 
  scale_x_date(breaks=week, date_labels="%m/%d") +
  scale_y_continuous(limits=c(0,1.01)) +  scale_colour_brewer(palette="Set1") + 
  ggtitle("Cognitive about COVID-19 infection (%)") + xlab("date (week)") + ylab("Likelihood (%)") + 
  geom_vline(xintercept=as.Date(c("20200219","20200508","20200815"),"%Y%m%d"), linetype=2) + 
  facet_grid(BQ1 ~ ., scales = "free_y", space = "free_y") 

grid.arrange(party_a, party_c, nrow=1, ncol=2)