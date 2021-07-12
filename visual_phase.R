library(readxl)
library(ggplot2)
library(dplyr)
library(geepack)
library(gridExtra)

risk <- data.frame(read_excel("./data/covid_risk.xlsx", sheet=1))
risk_summary <- data.frame(read_excel("./data/covid_summary.xlsx"))
risk$WEEK <- cut(as.Date(as.character(risk$DATE), "%y%m%d"), breaks="week")
risk$WEEK2 <- as.Date(risk$WEEK)
week <- unique(risk$WEEK2)

survName <- paste0("Survey", c(1:23))
# phase 구분
# 제1기  2020.1.20-2020.2.7 
# ---------------> "2020-02-03" "2020-02-10" "2020-02-17"
# 제2기(1차 유행)  2020.2.18-2020.5.5
# ---------------> "2020-02-24" "2020-03-02" "2020-03-09" "2020-03-16" "2020-03-23" "2020-04-06" "2020-04-20" "2020-05-04"
# 제3기   2020.5.6-2020.8.11
# ---------------> "2020-05-18" "2020-06-01" "2020-06-15" "2020-06-29" "2020-07-13"
# 제4기(2차유행)  2020.8.12-2020.11.12
# ---------------> "2020-08-17" "2020-09-14" "2020-10-19"
# 제5기(3차유행)  2020.11.13-2021.1.19
# ---------------> "2020-11-16" "2020-12-07" "2021-01-18" "2021-02-15"

phaseName <- c(rep("Phase1", 3), rep("Phase2", 8), rep("Phase3", 5), rep("Phase4", 3), rep("Phase5", 4))
survey <- data.frame("WEEK2"=week, survName,  phaseName)

risk <- merge(risk, survey, all.x=TRUE, by="WEEK2")
head(risk)

# interested in : BK5, BK6
risk_base <- risk %>% select(DATE, ARA, AGE, SEX, BK5, BK6, SAGE, WT2, JOB, XD2, XD3, BQ1, BQ3, survName, phaseName)
risk_base$DATE <- as.Date(as.character(risk$DATE), "%y%m%d")
risk_base$WEEK <- cut(as.Date(as.character(risk$DATE), "%y%m%d"), breaks="week")
risk_base$WEEK2 <- as.Date(risk_base$WEEK)

risk_base$BK5[risk_base$BK5==9999] <- NA
risk_base$Anxiety <- as.numeric(risk_base$BK5 == 1)
risk_base$Anxiety2 <- risk_base$Anxiety * risk_base$WT2

risk_base$Likelihood <- as.numeric(risk_base$BK6 == 1)
risk_base$Likelihood2 <- risk_base$Likelihood * risk_base$WT2

week <- unique(risk_base$WEEK2)
theme_set(theme_bw())
head(risk_base)
# Trust : Ordered BQ1
risk_base$BQ1[risk_base$BQ1==9999] <- NA
#risk_base$TRUST <- factor(risk_base$BQ1, levels=c(2,1,3, 9999), 
#                          labels=c("Dispproval", "Approval", "Neutral", "No Opinion"))

# Disapproval 0 0, Approval 1 0, Neutral 0 1
risk_base$TrustApproval <- as.numeric(risk_base$BQ1==1)
risk_base$TrustNeutral <- as.numeric(risk_base$BQ1==3)
table(risk_base$TrustApproval)
table(risk_base$TrustNeutral)

# Gender : Binary, Male & Female = 0 & 1
# risk_base$SEX <-factor(risk_base$SEX, levels=c(1,2), labels=c("Male", "Female"))
risk_base$SEX <- as.numeric(risk_base$SEX == 2)
table(risk_base$SEX)

# Age
# risk_base$AGE <- factor(risk_base$AGE, levels=c(1,2,3,4,5),
#                        labels=c("18-29", "30-39", "40-49", "50-59", "60+"))
# 18-29 0 0 0 0, 30-39 1 0 0 0, 40-49 0 1 0 0, 50-59 0 0 1 0, 60+ 0 0 0 1
risk_base$AGE3039 <- as.numeric(risk_base$AGE == 2)
risk_base$AGE4049 <- as.numeric(risk_base$AGE == 3)
risk_base$AGE5059 <- as.numeric(risk_base$AGE == 4)
risk_base$AGE60 <- as.numeric(risk_base$AGE == 5)

table(risk_base$AGE3039)
table(risk_base$AGE4049)
table(risk_base$AGE5059)
table(risk_base$AGE60)

# Occupation : Categorical
#risk_base$JOB <- factor(risk_base$JOB, levels=c(7, 1, 2, 3, 4, 5, 6), 
#                        labels=c("Unemployed/ETC", "Farming/Forestry/Fishery", "Self-employed", "Blue-collar", "White-collar", "Home maker and Student", "Home maker and Student"))
# Unemployed/ETC 0 0 0 0 0, Farming/Forestry/Fishery 1 0 0 0 0, Self-employed 0 1 0 0 0, 
# Blue-collar 0 0 1 0 0, White-collar 0 0 0 1 0,, Home maker and Student 0 0 0 0 1
risk_base$FarmForFish <- as.numeric(risk_base$JOB == 1)
risk_base$SelfEmployed <- as.numeric(risk_base$JOB == 2)
risk_base$Blue <- as.numeric(risk_base$JOB == 3)
risk_base$White <- as.numeric(risk_base$JOB == 4)
risk_base$HomeMakerStudent <- as.numeric(risk_base$JOB %in% c(5,6))

table(risk_base$FarmForFish)
table(risk_base$SelfEmployed)
table(risk_base$Blue)
table(risk_base$White)
table(risk_base$HomeMakerStudent)

# Self-reported economic status : Ordered
risk_base$XD3[risk_base$XD3==9999] <- NA
#risk_base$household <- factor(risk_base$XD3, levels=c(1, 3, 4, 5, 9999), 
#                              labels=c("Upper/Upper Middle", "Middle", "Lower Middle/Lower", "Lower Middle/Lower", "No opinion"))
risk_base$Middle <- as.numeric(risk_base$XD3 == 3)
risk_base$LowerMiddle <- as.numeric(risk_base$XD3 %in% c(4,5))

table(risk_base$Middle)
table(risk_base$LowerMiddle)

# AREA : Categorical
#risk_base$AREA <- factor(risk_base$ARA, levels=c(1, 2, 4, 5, 6, 7, 3, 8), 
#                         labels=c("수도권(서울/인천/경기)", "수도권(서울/인천/경기)", 
#                                  "충청권(대전/충청/세종)", 
#                                  "호남권(광주/전라)", 
#                                  "영남권(대구/경북/경남/부산/울산)", "영남권(대구/경북/경남/부산/울산)", 
#                                  "기타(강원/제주)", "기타(강원/제주)" ))
# 수도권 0 0 0 0, 충청권 1 0 0 0, 영남권 0 1 0 0, 호남권 0 0 1 0, 기타 0 0 0 1
risk_base$Chung <- as.numeric(risk_base$ARA == 4)
risk_base$Yeong <- as.numeric(risk_base$ARA %in% c(6,7))
risk_base$Ho <- as.numeric(risk_base$ARA == 5)
risk_base$AreaETC <- as.numeric(risk_base$ARA %in% c(3,8))

table(risk_base$Chung)
table(risk_base$Yeong)
table(risk_base$Ho)
table(risk_base$AreaETC)

# Party Identification
# Party X TRUST
#risk_base$party <- factor(risk_base$XD2, levels=c(1, 3, 2, 9999), 
#                          labels=c("conservative", "progressive", "neutral", "No opinion") )
risk_base$PartyProgressive <- as.numeric(risk_base$XD2 == 3)
risk_base$PartyNeutral <- as.numeric(risk_base$XD2 == 2)
risk_base$PartyNoOp <- as.numeric(risk_base$XD2 == 9999)

table(risk_base$PartyProgressive)
table(risk_base$PartyNeutral)
table(risk_base$PartyNoOp)

risk_base_2 <- na.omit(risk_base)
id1 <- 1:nrow(risk_base_2)
dim(risk_base_2)
colnames(risk_base_2)
phase <- unique(phaseName)
final1 <- NULL
for (i in phase){
  each.Surv <- risk_base_2[risk_base_2$phaseName == i, ]
  
  id1 <- 1:nrow(each.Surv)
  print(dim(each.Surv))
  
  fit <- glm(Anxiety~ TrustApproval + TrustNeutral + SEX + AGE3039 + AGE4049 + AGE5059 + AGE60 + 
                  FarmForFish + SelfEmployed + Blue + White + HomeMakerStudent + 
                  Middle + LowerMiddle + Chung + Yeong + Ho +AreaETC +
                  PartyProgressive + PartyNeutral + PartyNoOp, data=each.Surv, weights=each.Surv$WT2, family="binomial")
  
  fit.var <- data.frame("date" = i,
                        "variable" = names(fit$coefficients),
                        "exp" = exp(fit$coefficients),
                        "lower" = exp(summary(fit)$coef[,1] - 1.96 * summary(fit)$coef[,2]), # CI
                        "upper" = exp(summary(fit)$coef[,1] + 1.96 * summary(fit)$coef[,2]), 
                        "p-value" = summary(fit)$coef[,4])
  
  final1 <- rbind(final1, fit.var)
}
rownames(final1) <- NULL
head(final1)

fit <- glm(Anxiety~ TrustApproval + TrustNeutral +
                PartyProgressive + PartyNeutral + PartyNoOp +
                TrustApproval * PartyProgressive+ TrustApproval*PartyNeutral + TrustApproval * PartyNoOp+
                TrustNeutral * PartyProgressive+ TrustNeutral*PartyNeutral + TrustNeutral * PartyNoOp, data=each.Surv, weights=each.Surv$WT2, family="binomial")


final2 <- NULL
for (i in phase){
  each.Surv <- risk_base_2[risk_base_2$phaseName == i, ]
  date <- i
  id1 <- 1:nrow(each.Surv)
  print(dim(each.Surv))
  
  fit <- glm(Likelihood~ TrustApproval + TrustNeutral + SEX + AGE3039 + AGE4049 + AGE5059 + AGE60 + 
                  FarmForFish + SelfEmployed + Blue + White + HomeMakerStudent + 
                  Middle + LowerMiddle + Chung + Yeong + Ho +AreaETC +
                  PartyProgressive + PartyNeutral + PartyNoOp, data=each.Surv, weights=each.Surv$WT2, family="binomial")
  
  fit.var <- data.frame("date" = i,
                        "variable" = names(fit$coefficients),
                        "exp" = exp(fit$coefficients),
                        "lower" = exp(summary(fit)$coef[,1] - 1.96 * summary(fit)$coef[,2]), # CI
                        "upper" = exp(summary(fit)$coef[,1] + 1.96 * summary(fit)$coef[,2]), 
                        "p-value" = summary(fit)$coef[,4])
  
  final2 <- rbind(final2, fit.var)
}
rownames(final2) <- NULL
head(final2)

file_format <- file('./[210526]each_survey_Affective.csv',encoding="euc-kr")
write.csv(final1, file_format)

file_format <- file('./[210526]each_survey_Cognitive.csv',encoding="euc-kr")
write.csv(final2, file_format)

# AGE PLOT
Results_AGE1 <- final1[final1$variable %in% c("AGE3039", "AGE4049", "AGE5059", "AGE60"),]

age_a <- ggplot(data=Results_AGE1,
       aes(x = date,y = exp, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept =1, linetype=2)+
  xlab('Phase')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.5,cex=1) + 
  facet_wrap(~variable, strip.position="left",nrow=9,scales = "free_y")+ 
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(size=6)) + 
  scale_y_continuous(limits=c(0,3)) + ggtitle("Affective")

Results_AGE2 <- final2[final2$variable %in% c("AGE3039", "AGE4049", "AGE5059", "AGE60"),]

age_c <- ggplot(data=Results_AGE2,
       aes(x = date,y = exp, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept =1, linetype=2)+
  xlab('Phase')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.5,cex=1) + 
  facet_wrap(~variable, strip.position="left",nrow=9,scales = "free_y")+ 
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(size=6)) + 
  scale_y_continuous(limits=c(0,3)) + ggtitle("Cognitive")

table(final1$variable)

# AREA plot
Results_AREA1 <- final1[final1$variable %in% c("Chung", "Ho", "Yeong", "AreaETC"),]
area_a <- ggplot(data=Results_AREA1,
       aes(x = date,y = exp, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept =1, linetype=2)+
  xlab('Phase')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.5,cex=1) + 
  facet_wrap(~variable, strip.position="left",nrow=9,scales = "free_y")+ 
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(size=6)) + 
  scale_y_continuous(limits=c(0,3)) + ggtitle("Affective")

Results_AREA2 <- final2[final2$variable %in% c("Chung", "Ho", "Yeong", "AreaETC"),]
area_c <- ggplot(data=Results_AREA2,
       aes(x = date,y = exp, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept =1, linetype=2)+
  xlab('Phase')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.5,cex=1) + 
  facet_wrap(~variable, strip.position="left",nrow=9,scales = "free_y")+ 
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(size=6)) + 
  scale_y_continuous(limits=c(0,3)) + ggtitle("Cognitive")

# Job plot
table(final1$variable)
Results_JOB1 <- final1[final1$variable %in% c("FarmForFish", "Blue", "White", "HomeMakerStudent", "SelfEmployed"),]
job_a <- ggplot(data=Results_JOB1,
                 aes(x = date,y = exp, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept =1, linetype=2)+
  xlab('Phase')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.5,cex=1) + 
  facet_wrap(~variable, strip.position="left",nrow=9,scales = "free_y")+ 
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(size=6)) + 
  scale_y_continuous(limits=c(0,3)) + ggtitle("Affective")

Results_JOB2 <- final2[final2$variable %in% c("FarmForFish", "Blue", "White", "HomeMakerStudent", "SelfEmployed"),]
job_c <- ggplot(data=Results_JOB2,
                 aes(x = date,y = exp, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept =1, linetype=2)+
  xlab('Phase')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.5,cex=1) + 
  facet_wrap(~variable, strip.position="left",nrow=9,scales = "free_y")+ 
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(size=6)) + 
  scale_y_continuous(limits=c(0,3)) + ggtitle("Cognitive")

# TRUST plot
table(final1$variable)
Results_trust1 <- final1[final1$variable %in% c("TrustApproval", "TrustNeutral"),]
trust_a <- ggplot(data=Results_trust1,
                aes(x = date,y = exp, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept =1, linetype=2)+
  xlab('Phase')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.5,cex=1) + 
  facet_wrap(~variable, strip.position="left",nrow=9,scales = "free_y")+ 
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(size=6)) + 
  scale_y_continuous(limits=c(0,3)) + ggtitle("Affective")

Results_trust2 <- final2[final2$variable %in% c("TrustApproval", "TrustNeutral"),]
trust_c <- ggplot(data=Results_trust2,
                aes(x = date,y = exp, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept =1, linetype=2)+
  xlab('Phase')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.5,cex=1) + 
  facet_wrap(~variable, strip.position="left",nrow=9,scales = "free_y")+ 
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(size=6)) + 
  scale_y_continuous(limits=c(0,3)) + ggtitle("Cognitive")

# Party Identification plot
table(final1$variable)
Results_party1 <- final1[final1$variable %in% c("PartyProgressive", "PartyNeutral", "PartyNoOp"),]
party_a <- ggplot(data=Results_party1,
                  aes(x = date,y = exp, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept =1, linetype=2)+
  xlab('Phase')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.5,cex=1) + 
  facet_wrap(~variable, strip.position="left",nrow=9,scales = "free_y")+ 
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(size=6)) + 
  scale_y_continuous(limits=c(0,3)) + ggtitle("Affective")

Results_party2 <- final2[final2$variable %in% c("PartyProgressive", "PartyNeutral", "PartyNoOp"),]
party_c <- ggplot(data=Results_party2,
                  aes(x = date,y = exp, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept =1, linetype=2)+
  xlab('Phase')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.5,cex=1) + 
  facet_wrap(~variable, strip.position="left",nrow=9,scales = "free_y")+ 
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(size=6)) + 
  scale_y_continuous(limits=c(0,3)) + ggtitle("Cognitive")

# Sex plot
table(final1$variable)
Results_sex1 <- final1[final1$variable %in% c("SEX"),]
sex_a <- ggplot(data=Results_sex1,
                  aes(x = date,y = exp, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept =1, linetype=2)+
  xlab('Phase')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.5,cex=1) + 
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(size=6)) + 
  scale_y_continuous(limits=c(0,3)) + ggtitle("Affective")

Results_sex2 <- final2[final2$variable %in% c("SEX"),]
sex_c <- ggplot(data=Results_sex2,
                  aes(x = date,y = exp, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept =1, linetype=2)+
  xlab('Phase')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.5,cex=1) + 
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(size=6)) + 
  scale_y_continuous(limits=c(0,3)) + ggtitle("Cognitive")

grid.arrange(age_a, age_c, nrow=1, ncol=2)
grid.arrange(sex_a, sex_c, nrow=1, ncol=2)
grid.arrange(area_a, area_c, nrow=1, ncol=2)
grid.arrange(job_a, job_c, nrow=1, ncol=2)
grid.arrange(trust_a, trust_c, nrow=1, ncol=2)
grid.arrange(party_a, party_c, nrow=1, ncol=2)

# income plot
Results_income1 <- final1[final1$variable %in% c("Middle", "LowerMiddle"),]
income_a <- ggplot(data=Results_income1,
                aes(x = date,y = exp, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept =1, linetype=2)+
  xlab('Phase')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.5,cex=1) + 
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(size=6)) + 
  scale_y_continuous(limits=c(0,3)) + ggtitle("Affective")  + 
  facet_wrap(~variable, strip.position="left",nrow=9,scales = "free_y")

Results_income2 <- final2[final2$variable %in% c("Middle", "LowerMiddle"),]

income_c <- ggplot(data=Results_income2,
                aes(x = date,y = exp, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept =1, linetype=2)+
  xlab('Phase')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.5,cex=1) + 
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(size=6)) + 
  scale_y_continuous(limits=c(0,3)) + ggtitle("Cognitive") + 
  facet_wrap(~variable, strip.position="left",nrow=9,scales = "free_y")

grid.arrange(income_a, income_c, nrow=1, ncol=2)

