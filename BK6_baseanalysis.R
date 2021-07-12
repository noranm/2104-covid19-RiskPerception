library(readxl)
library(ggplot2)
library(dplyr)
#library(gridExtra)

risk <- data.frame(read_excel("./data/covid_risk.xlsx", sheet=1))
risk_summary <- data.frame(read_excel("./data/covid_summary.xlsx"))
risk$WEEK <- cut(as.Date(as.character(risk$DATE), "%y%m%d"), breaks="week")
risk$WEEK2 <- as.Date(risk$WEEK)
week <- unique(risk$WEEK2)

survName <- paste0("Survey", c(1:23))
phaseName <- c(rep("Phase1",8), rep("Phase2", 8), rep("Phase3", 7))
survey <- data.frame("WEEK2"=week, survName,  phaseName)

risk <- merge(risk, survey, all.x=TRUE, by="WEEK2")
head(risk)

# interested in : BK6, BK6
risk_BK6 <- risk %>% select(DATE, ARA, AGE, SEX, BK6, SAGE, WT2, JOB, XD2, XD3, BQ1, BQ3, survName, phaseName)
risk_BK6$DATE <- as.Date(as.character(risk$DATE), "%y%m%d")
risk_BK6$WEEK <- cut(as.Date(as.character(risk$DATE), "%y%m%d"), breaks="week")
risk_BK6$WEEK2 <- as.Date(risk_BK6$WEEK)


risk_BK6$BK6[risk_BK6$BK6==9999] <- NA
risk_BK6$Likelihood <- as.numeric(risk_BK6$BK6 == 1)
risk_BK6$Likelihood2 <- risk_BK6$Likelihood  * risk_BK6$WT2

week <- unique(risk_BK6$WEEK2)
theme_set(theme_bw())
head(risk_BK6)

# Trust : Ordered BQ1
# Gender : Binary, Male & Female = 0 & 1
# Age : Ordered
# Occupation : Categorical
# Self-reported economic status : Ordered
# AREA : Categorical
# Party Identification
# Party X TRUST
risk_BK6$BQ1[risk_BK6$BQ1==9999] <- NA
risk_BK6$AGE <- factor(risk_BK6$AGE, levels=c(1,2,3,4,5),
                       labels=c("18-29", "30-39", "40-49", "50-59", "60+"))
risk_BK6$TRUST <- factor(risk_BK6$BQ1, levels=c(2,1,3, 9999), 
                         labels=c("Dispproval", "Approval", "Neutral", "No Opinion"))
risk_BK6$SEX <- factor(risk_BK6$SEX, levels=c(1,2), labels=c("Male", "Female"))
risk_BK6$JOB <- factor(risk_BK6$JOB, levels=c(7, 1, 2, 3, 4, 5, 6), 
                       labels=c("Unemployed/ETC", "Farming/Forestry/Fishery", "Self-employed", "Blue-collar", "White-collar", "Home maker and Student", "Home maker and Student"))
risk_BK6$XD3[risk_BK6$XD3==9999] <- NA
risk_BK6$household <- factor(risk_BK6$XD3, levels=c(1, 3, 4, 5, 9999), 
                             labels=c("Upper/Upper Middle", "Middle", "Lower Middle/Lower", "Lower Middle/Lower", "No opinion"))

risk_BK6$AREA <- factor(risk_BK6$ARA, levels=c(1, 2, 4, 5, 6, 7, 3, 8), 
                        labels=c("수도권(서울/인천/경기)", "수도권(서울/인천/경기)", 
                                 "충청권(대전/충청/세종)", 
                                 "호남권(광주/전라)", 
                                 "영남권(대구/경북/경남/부산/울산)", "영남권(대구/경북/경남/부산/울산)", 
                                 "기타(강원/제주)", "기타(강원/제주)" ))
risk_BK6$party <- factor(risk_BK6$XD2, levels=c(1, 3, 2, 9999), 
                         labels=c("conservative", "progressive", "neutral", "No opinion") )

risk_BK6_2 <- na.omit(risk_BK6)

dim(risk_BK6_2)
table(risk_BK6_2$survName)
colnames(risk_BK6_2)

phase <- unique(survey$phaseName)

fit <- glm(Likelihood~ AGE+SEX+AREA+JOB+TRUST+household+party+phaseName, data=risk_BK6_2, weights=risk_BK6_2$WT2, family="binomial")

fit.var <- data.frame("variable" = names(fit$coefficients),
                      "exp" = exp(fit$coefficients))
fit.coef <- data.frame("variable" = rownames(summary(fit)$coef), # 계수
                       #"s.e" = summary(fit)$coef[,2], # 표준 오차
                       "lower" = exp(summary(fit)$coef[,1] - 1.96 * summary(fit)$coef[,2]), # CI
                       "upper" = exp(summary(fit)$coef[,1] + 1.96 * summary(fit)$coef[,2]), 
                       "p-value" = summary(fit)$coef[,4])

overall.fit <- merge(fit.var, fit.coef, by="variable", all.x=TRUE)
colN <- colnames(overall.fit)[2:5]
colnames(overall.fit)[2:5] <- paste0("Overall", ":", colN)

file_format <- file('./overall_BK6_fit210508.csv',encoding="euc-kr")
write.csv(overall.fit, file_format, row.names=FALSE)

fit <- glm(Likelihood~ AGE+SEX+AREA+JOB+TRUST+household+party+survName, data=risk_BK6_2, weights=risk_BK5_2$WT2, family="binomial")
fit.var <- data.frame("variable" = names(fit$coefficients),
                      "exp" = exp(fit$coefficients))
fit.coef <- data.frame("variable" = rownames(summary(fit)$coef), # 계수
                       #"s.e" = summary(fit)$coef[,2], # 표준 오차
                       "lower" = exp(summary(fit)$coef[,1] - 1.96 * summary(fit)$coef[,2]), # CI
                       "upper" = exp(summary(fit)$coef[,1] + 1.96 * summary(fit)$coef[,2]), 
                       "p-value" = summary(fit)$coef[,4])

overall.fit <- merge(fit.var, fit.coef, by="variable", all.x=TRUE)
colN <- colnames(overall.fit)[2:5]
colnames(overall.fit)[2:5] <- paste0("Overall", ":", colN)

file_format <- file('./overall_BK6_fit2_210508.csv',encoding="euc-kr")
write.csv(overall.fit, file_format, row.names=FALSE)


for (i in phase){
  each.Surv <- risk_BK6_2[risk_BK6_2$phaseName == i, ]
  print(dim(each.Surv))
  
  fit <- glm(Likelihood~ AGE+SEX+AREA+JOB+TRUST+household+party+TRUST*party, data=each.Surv, weights=each.Surv$WT2, family="binomial")
  fit.var <- data.frame("variable" = names(fit$coefficients),
                        "exp" = exp(fit$coefficients))
  fit.coef <- data.frame("variable" = rownames(summary(fit)$coef), # 계수
                         #"s.e" = summary(fit)$coef[,2], # 표준 오차
                         "lower" = exp(summary(fit)$coef[,1] - 1.96 * summary(fit)$coef[,2]), # CI
                         "upper" = exp(summary(fit)$coef[,1] + 1.96 * summary(fit)$coef[,2]), 
                         "p-value" = summary(fit)$coef[,4])
  
  each.fit <- merge(fit.var, fit.coef, by="variable", all.x=TRUE)
  colN <- colnames(each.fit)[2:5]
  colnames(each.fit)[2:5] <- paste0(i, ":", colN)
  
  final <- merge(final, each.fit, by="variable", all.x=TRUE)
  
}
final
file_format <- file('./BK6_OddsRatio_eachPhase.csv',encoding="UTF-8")
write.csv(final, file_format, row.names=FALSE)
