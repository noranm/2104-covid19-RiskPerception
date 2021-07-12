library(readxl)
library(ggplot2)
library(dplyr)

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

# colnames(df.all)
# "YEAR"         "QUARTER"      "MONTH"        "WEEK"         "DATE"         "respondentID"
# "SIDO"         "SQ1"          "K5"           "K6"           "K9"           "DQ1"         
# "DQ2"          "DQ3"          "ARA"          "AGE"          "SEX"          "BQ1"         
# "BQ3"          "BQ4"          "BK5"          "BK6"          "BK9"          "JOB"         
# "XD2"          "XD3"          "SAGE"         "AGE5"         "X1"           "WT2"     

w.sd <- function(xi, xbar, w){
  s1 <- sum( (xi-rep(xbar,length(xi)))^2 * w )
  a <- sum(w) / ( (sum(w))^2 - sum(w^2) )
  return ( sqrt( s1 * a) )
}
# Baseline characteristics
df.all <- risk
pName <- unique(phaseName)

Final <- NULL
for (i in pName) { 
  df.each <- df.all[df.all$phaseName == i, ]

  # 0. N
  N <- data.frame(Index="N", "cnt"=nrow(df.each), "weighted cnt"=sum(df.each$WT2))
  
  # 1. Age
  AGE <- data.frame(Index=c("mean", "sd", "19-29", "30-39", "40-49", "50-59", "60+"), 
                    "cnt"=c( mean(df.each$SQ1), sd(df.each$SQ1),
                           sum(df.each$AGE==1), sum(df.each$AGE==2), sum(df.each$AGE==3), sum(df.each$AGE==4), sum(df.each$AGE==5) ), 
                    "weighted cnt"=c( sum(df.each$SQ1 * df.each$WT2)/sum(df.each$WT2), w.sd(df.each$SQ1, sum(df.each$SQ1 * df.each$WT2)/sum(df.each$WT2), df.each$WT2),
                             sum(as.numeric(df.each$AGE==1)*df.each$WT2), sum(as.numeric(df.each$AGE==2)*df.each$WT2), sum(as.numeric(df.each$AGE==3)*df.each$WT2), sum(as.numeric(df.each$AGE==4)*df.each$WT2), sum(as.numeric(df.each$AGE==5)*df.each$WT2) ))
  
  # 2. Sex
  SEX <- data.frame("Index"=c("Male", "Female"),
                    "cnt"=c(sum(df.each$SEX==1), sum(df.each$SEX==2)),
                    "weighted cnt" = c( sum(as.numeric(df.each$SEX==1) * df.each$WT2), sum(as.numeric(df.each$SEX==2) * df.each$WT2 ) ))
  
  # 3. JOB
  # 210429,  Unemployed, Self-employed, Blue-collar, White-collar, Home maker and Student (합치기)
  JOB <- data.frame("Index"=c("Unemployed", "Farming/Forestry/Fishery", "Self-employed", "Blue-collar", "White-collar", "Home maker and Student"),
                    "cnt"=c(sum(df.each$DQ1==7), sum(df.each$DQ1==1), sum(df.each$DQ1==2), sum(df.each$DQ1==3), sum(df.each$DQ1==4), sum(df.each$DQ1 %in% c(5,6)) ),
                    "weighted cnt"=c( sum(as.numeric(df.each$DQ1==7) * df.each$WT2), sum(as.numeric(df.each$DQ1==1) * df.each$WT2), sum(as.numeric(df.each$DQ1==2) * df.each$WT2), sum(as.numeric(df.each$DQ1==3) * df.each$WT2), sum(as.numeric(df.each$DQ1==4) * df.each$WT2), sum(as.numeric(df.each$DQ1 %in% c(5,6)) * df.each$WT2)) )
  
  #JOB <- data.frame("Index"=c("Unemployed", "Farming/Forestry/Fishery", "Self-employed", "Blue-collar", "White-collar", "Home maker", "Student"),
  #                  "cnt"=c(sum(df.each$DQ1==7), sum(df.each$DQ1==1), sum(df.each$DQ1==2), sum(df.each$DQ1==3), sum(df.each$DQ1==4), sum(df.each$DQ1==5), sum(df.each$DQ1==6)),
  #                  "weighted cnt"=c( sum(as.numeric(df.each$DQ1==7) * df.each$WT2), sum(as.numeric(df.each$DQ1==1) * df.each$WT2), sum(as.numeric(df.each$DQ1==2) * df.each$WT2), sum(as.numeric(df.each$DQ1==3) * df.each$WT2), sum(as.numeric(df.each$DQ1==4) * df.each$WT2), sum(as.numeric(df.each$DQ1==5) * df.each$WT2), sum(as.numeric(df.each$DQ1==6) * df.each$WT2)) )
  
  # 4. Self-reported household economic status
  # 210429, Upper/Upper Middle, Middle, Lower Middle/Lower 
  ECONOMICstatus <- data.frame("Index"=c("Upper/Upper Middle", "Middle", "Lower Middle/Lower", "No opinion"),
                               "cnt"=c(sum(df.each$XD3==1), sum(df.each$XD3==3), sum(df.each$XD3 %in% c(4,5) ), sum(df.each$XD3==9999)),
                               "weighted cnt"=c( sum(as.numeric(df.each$XD3==1) * df.each$WT2), sum(as.numeric(df.each$XD3==3) * df.each$WT2), sum(as.numeric(df.each$XD3 %in% c(4,5)) * df.each$WT2), sum(as.numeric(df.each$XD3==9999) * df.each$WT2)) )
  
  #ECONOMICstatus <- data.frame("Index"=c("Upper/Upper Middle", "Middle", "Lower Middle", "Lower", "No opinion"),
  #                  "cnt"=c(sum(df.each$XD3==1), sum(df.each$XD3==3), sum(df.each$XD3==4), sum(df.each$XD3==5), sum(df.each$XD3==9999)),
  #                  "weighted cnt"=c( sum(as.numeric(df.each$XD3==1) * df.each$WT2), sum(as.numeric(df.each$XD3==3) * df.each$WT2), sum(as.numeric(df.each$XD3==4) * df.each$WT2), sum(as.numeric(df.each$XD3==5) * df.each$WT2), sum(as.numeric(df.each$XD3==9999) * df.each$WT2)) )
  
  # 5. Residential area
  # 210429, Residential Area : 
  AREA <- data.frame("Index"=c("수도권(서울/인천/경기)", "충청권(대전/충청/세종)", "호남권(광주/전라)", "영남권(대구/경북/경남/부산/울산)",  "기타(강원/제주)" ),
                    "cnt"=c( sum(df.each$ARA %in% c(1,2) ) , sum(df.each$ARA==4), sum(df.each$ARA==5), sum(df.each$ARA %in% c(6,7) ), sum(df.each$ARA %in% c(3,8) ) ),
                    "weighted cnt"=c( sum(as.numeric(df.each$ARA %in% c(1,2) ) * df.each$WT2), sum(as.numeric(df.each$ARA==4) * df.each$WT2), sum(as.numeric(df.each$ARA==5) * df.each$WT2), sum(as.numeric(df.each$ARA %in% c(6,7)) * df.each$WT2), sum(as.numeric( df.each$ARA %in% c(3,8) ) * df.each$WT2) ) )
  
  # 6. Presidential job approval rating
  Rate <- data.frame("Index"=c("Approval", "Dispproval", "Neutral", "No Opinion"),
                     "cnt"=c(sum(df.each$BQ1==1), sum(df.each$BQ1==2), sum(df.each$BQ1==3), sum(df.each$BQ1==9999)),
                     "weighted cnt"=c( sum(as.numeric(df.each$BQ1==1) * df.each$WT2), sum(as.numeric(df.each$BQ1==2) * df.each$WT2), sum(as.numeric(df.each$BQ1==3) * df.each$WT2), sum(as.numeric(df.each$BQ1==9999) * df.each$WT2))  )
  
  
  # 7. Party identification (XD2)
  party <- data.frame("Index"=c("conservative", "neutral", "progressive", "No opinion"),
                      "cnt"=c(sum(df.each$XD2==1), sum(df.each$XD2==2), sum(df.each$XD2==3), sum(df.each$XD2==9999)),
                      "weighted cnt"=c( sum(as.numeric(df.each$XD2==1) * df.each$WT2), sum(as.numeric(df.each$XD2==2) * df.each$WT2), sum(as.numeric(df.each$XD2==3) * df.each$WT2), sum(as.numeric(df.each$XD2==9999) * df.each$WT2))  )
  
  # 8. Affective risk perception
  affective <- data.frame("Index"=c("Worried", "Not worried", "No opinion"),
                          "cnt"=c(sum(df.each$BK5==1), sum(df.each$BK5==2), sum(df.each$BK5==9999)),
                          "weighted cnt"=c( sum(as.numeric(df.each$BK5==1) * df.each$WT2), sum(as.numeric(df.each$BK5==2) * df.each$WT2), sum(as.numeric(df.each$BK5==9999) * df.each$WT2))  )

  # 9. Cognitive risk perception
  cognitive <- data.frame("Index"=c("Probable", "Not probable", "No opinion"),
                          "cnt"=c(sum(df.each$BK6==1), sum(df.each$BK6==2), sum(df.each$BK6==9999)),
                          "weighted cnt"=c( sum(as.numeric(df.each$BK6==1) * df.each$WT2), sum(as.numeric(df.each$BK6==2) * df.each$WT2), sum(as.numeric(df.each$BK6==9999) * df.each$WT2))  )

  eachSurv <- rbind(N, AGE, SEX, JOB, ECONOMICstatus, AREA, Rate, party, affective, cognitive)
  colnames(eachSurv) <- c(paste0(i,"_idx"), paste0(i, "_cnt"), paste0(i, "_w.cnt") )
  
  if (i == "Phase1") {
    Final <- eachSurv
  } else {
    Final <- cbind(Final, eachSurv)
  }
}

Final <- BasicTable(df.all)
Final2 <- cbind(all_Table, Final)

file_format <- file('./[210612]baseline.csv', encoding="euc-kr")
write.csv(Final, file_format, row.names=FALSE)
