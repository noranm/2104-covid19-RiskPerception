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
# Gender : Binary, Male & Female = 0 & 1
# Age : Ordered
# Occupation : Categorical
# Self-reported economic status : Ordered
# AREA : Categorical
# Party Identification
# Party X TRUST
risk_base$BQ1[risk_base$BQ1==9999] <- NA
risk_base$AGE <- factor(risk_base$AGE, levels=c(1,2,3,4,5),
                        labels=c("18-29", "30-39", "40-49", "50-59", "60+"))
table(risk_base$AGE)
risk_base$TRUST <- factor(risk_base$BQ1, levels=c(2,1,3, 9999), 
                          labels=c("Dispproval", "Approval", "Neutral", "No Opinion"))

risk_base$SEX <- factor(risk_base$SEX, levels=c(1,2), labels=c("Male", "Female"))
risk_base$JOB <- factor(risk_base$JOB, levels=c(7, 1, 2, 3, 4, 5, 6), 
                        labels=c("Unemployed/ETC", "Farming/Forestry/Fishery", "Self-employed", "Blue-collar", "White-collar", "Home maker and Student", "Home maker and Student"))
risk_base$XD3[risk_base$XD3==9999] <- NA
risk_base$household <- factor(risk_base$XD3, levels=c(1, 3, 4, 5, 9999), 
                              labels=c("Upper/Upper Middle", "Middle", "Lower Middle/Lower", "Lower Middle/Lower", "No opinion"))

risk_base$AREA <- factor(risk_base$ARA, levels=c(1, 2, 4, 5, 6, 7, 3, 8), 
                         labels=c("수도권(서울/인천/경기)", "수도권(서울/인천/경기)", 
                                  "충청권(대전/충청/세종)", 
                                  "호남권(광주/전라)", 
                                  "영남권(대구/경북/경남/부산/울산)", "영남권(대구/경북/경남/부산/울산)", 
                                  "기타(강원/제주)", "기타(강원/제주)" ))
risk_base$party <- factor(risk_base$XD2, levels=c(1, 3, 2, 9999), 
                          labels=c("conservative", "progressive", "neutral", "No opinion") )

risk_base_2 <- na.omit(risk_base)
id1 <- 1:nrow(risk_base_2)
dim(risk_base_2)
colnames(risk_base_2)

# weights=risk_base_2$WT2
fit1 <- glm(Anxiety~ AGE+SEX+AREA+JOB+TRUST+household+party+phaseName, data=risk_base_2, weights=risk_base_2$WT2, family="binomial")
fit1.var <- data.frame("variable" = names(fit1$coefficients),
                       "exp" = round(exp(fit1$coefficients),4),
                       "lower" = round(exp( summary(fit1)$coef[,1]-1.96 * summary(fit1)$coef[,2] ) ,4), # CI
                       "upper" = round(exp(summary(fit1)$coef[,1]+1.96 * summary(fit1)$coef[,2] ), 4), 
                       "p-value" = round(summary(fit1)$coef[,4], 4) )

colN <- colnames(fit1.var)[2:5]
colnames(fit1.var)[2:5] <- paste0("Affective", ":", colN)

fit2 <- glm(Likelihood~ AGE+SEX+AREA+JOB+TRUST+household+party+phaseName, data=risk_base_2, weights=risk_base_2$WT2, family="binomial")
fit2.var <- data.frame("variable" = names(fit2$coefficients),
                       "exp" = round(exp(fit2$coefficients),4),
                       "lower" = round(exp( summary(fit2)$coef[,1]-1.96 * summary(fit2)$coef[,2] ) ,4), # CI
                       "upper" = round(exp(summary(fit2)$coef[,1]+1.96 * summary(fit2)$coef[,2] ), 4), 
                       "p-value" = round(summary(fit2)$coef[,4], 4) )

colN <- colnames(fit2.var)[2:5]
colnames(fit2.var)[2:5] <- paste0("Cognitive", ":", colN)
overall.var <- cbind(fit1.var, fit2.var)

file_format <- file('./[210508]overall_phase.csv',encoding="euc-kr")
write.csv(overall.var, file_format, row.names=FALSE)

fit1 <- glm(Anxiety~ AGE+SEX+AREA+JOB+TRUST+household+party+survName, data=risk_base_2, weights=risk_base_2$WT2, family="binomial")
fit1.var <- data.frame("variable" = names(fit1$coefficients),
                       "exp" = round(exp(fit1$coefficients),4),
                       "lower" = round(exp( summary(fit1)$coef[,1]-1.96 * summary(fit1)$coef[,2] ) ,4), # CI
                       "upper" = round(exp(summary(fit1)$coef[,1]+1.96 * summary(fit1)$coef[,2] ), 4), 
                       "p-value" = round(summary(fit1)$coef[,4], 4) )

colN <- colnames(fit1.var)[2:5]
colnames(fit1.var)[2:5] <- paste0("Affective", ":", colN)

fit2 <- glm(Likelihood~ AGE+SEX+AREA+JOB+TRUST+household+party+survName, data=risk_base_2, weights=risk_base_2$WT2, family="binomial")
fit2.var <- data.frame("variable" = names(fit2$coefficients),
                       "exp" = round(exp(fit2$coefficients),4),
                       "lower" = round(exp( summary(fit2)$coef[,1]-1.96 * summary(fit2)$coef[,2] ) ,4), # CI
                       "upper" = round(exp(summary(fit2)$coef[,1]+1.96 * summary(fit2)$coef[,2] ), 4), 
                       "p-value" = round(summary(fit2)$coef[,4], 4) )

colN <- colnames(fit2.var)[2:5]
colnames(fit2.var)[2:5] <- paste0("Cognitive", ":", colN)
overall.var <- cbind(fit1.var, fit2.var)

file_format <- file('./[210508]overall_surv.csv',encoding="euc-kr")
write.csv(overall.var, file_format, row.names=FALSE)
