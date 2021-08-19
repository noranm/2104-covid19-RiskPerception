library(ggplot2)
library(dplyr)

confirmed_global <- read.csv("./time_series_covid19_confirmed_global.csv", header=TRUE)
head(confirmed_global)
korea_ts <- confirmed_global[confirmed_global$Country.Region == "Korea, South", 5:510]
theme_set(theme_bw())

korea_ts2 <- data.frame(t(korea_ts))$X161
date=substr(colnames(confirmed_global)[5:510], 2, 9)
confirmed <- data.frame("Date" = gsub(pattern="[.]", replacement="/", x=date),
                        "ALL_Confirmed" = korea_ts2,
                        "Daily_confirmed" = c(c(korea_ts2, 0) - c(0, korea_ts2))[1:506])

confirmed$week = cut(as.Date(confirmed$Date, "%m/%d/%y"), breaks="week")
confirmed_week <- confirmed %>% group_by(week) %>% summarise(weekly_cases = sum(Daily_confirmed)) %>% data.frame()
confirmed_week$WEEK2 <- as.Date(confirmed_week$week)
confirmed_week2 <- confirmed_week[1:58,]

ggplot(confirmed_week2, aes(x = WEEK2, y = weekly_cases)) + 
  geom_bar(stat = "identity") +
  scale_x_date(breaks=confirmed_week$WEEK2, date_labels="%m/%d") +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 30, size=5)) + 
  scale_y_continuous(limits=c(0,8000))+ 
  ggtitle("Weekly COVID-19 Confirmed Cases") + xlab("Date") + ylab("No. of cases")

# interested in : BK5, BK6
risk <- data.frame(read_excel("./data/covid_risk.xlsx", sheet=1))
risk$DATE2 <- as.Date(as.character(risk$DATE), "%y%m%d")
risk$WEEK <- cut(risk$DATE2, breaks="week")
risk$WEEK2 <- as.Date(risk$WEEK)
head(risk)

week <- unique(risk$WEEK2)
survName <- paste0("Survey", c(1:23))
phaseName <- c(rep("Phase1", 3), rep("Phase2", 8), rep("Phase3", 5), rep("Phase4", 3), rep("Phase5", 4))
survey <- data.frame("WEEK2"=as.Date(week), survName,  phaseName)

WEEK_SURVEY <- merge(confirmed_week2, survey, by="WEEK2", all=TRUE)

risk <- merge(risk, survey, by="WEEK2")

risk_base <- risk %>% select(WEEK2, BK5, BK6, WT2, survName, phaseName)
risk_base$Affective <- as.numeric(risk_base$BK5 == 1)
risk_base$Cognitive <- as.numeric(risk_base$BK6 == 1)

head(risk_base)
survReport <- risk_base %>% group_by(survName) %>% 
  summarise(Affective = sum(Affective * WT2)/ sum(WT2),
            Cognitive = sum(Cognitive * WT2)/ sum(WT2)) %>% data.frame()
confirmed_week$WEEK2 <- as.Date(confirmed_week$week)

confirmed_final <- merge(WEEK_SURVEY, survReport, by="survName", all.x=TRUE)
head(confirmed_final)

theme_set(theme_bw())
p + geom_line(data=Aff_final, aes(x = WEEK2, y = Affective*8000, color="Affective" ) ) +
  geom_line(data=Aff_final, aes(x = WEEK2, y = Cognitive*8000, color="Cognitive" ) ) +
  geom_point(data=Aff_final, aes(x = WEEK2, y = Affective*8000, color="Affective" ) ) +
  geom_point(data=Aff_final, aes(x = WEEK2, y = Cognitive*8000, color="Cognitive") )  +
  scale_color_manual(values = c('Affective' = 'darkblue', 'Cognitive' = 'red')) +
  labs(color = 'Risk Perception') +
  scale_y_continuous("No. of cases", sec.axis = sec_axis(~.*(1/8000), name = "Risk Perception" ) ) + ylab("Worry (%)") + 
  geom_vline(xintercept=as.Date(c("20200218","20200506","20200812","20201113"),"%Y%m%d"), linetype=2)+ 
  ggtitle("Weekly COVID-19 Confirmed Cases") + xlab("Date") +
  scale_x_date(breaks=confirmed_final$WEEK2, date_labels="%m/%d") +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 30, size=10))


survReport2 <- risk_base %>% group_by(phaseName) %>% 
  summarise(Affective = sum(Affective * WT2)/ sum(WT2),
            Cognitive = sum(Cognitive * WT2)/ sum(WT2)) %>% data.frame()

confirmed_final2 <- merge(WEEK_SURVEY, survReport2, by="phaseName", all.x=TRUE)
head(confirmed_final2)

Aff_final2 <- unique(confirmed_final2[,c('phaseName', 'Affective', 'Cognitive') ])
Aff_final2 <- merge(Aff_final2, survey, by='phaseName', all.x=TRUE)
Aff_final3 <- Aff_final2[c(2,7,15,19,22),]
p + geom_line(data=Aff_final3, aes(x = WEEK2, y = Affective*8000, color="Affective" ) ) +
  geom_line(data=Aff_final3, aes(x = WEEK2, y = Cognitive*8000, color="Cognitive" ) ) +
  geom_point(data=Aff_final3, aes(x = WEEK2, y = Affective*8000, color="Affective" ) ) +
  geom_point(data=Aff_final3, aes(x = WEEK2, y = Cognitive*8000, color="Cognitive") )  +
  scale_color_manual(values = c('Affective' = 'darkblue', 'Cognitive' = 'red')) +
  labs(color = 'Risk Perception') +
  scale_y_continuous("No. of cases", sec.axis = sec_axis(~.*(1/8000), name = "Risk Perception" ) ) + ylab("Worry (%)") + 
  geom_vline(xintercept=as.Date(c("20200218","20200506","20200812","20201113"),"%Y%m%d"), linetype=2)+ 
  ggtitle("Weekly COVID-19 Confirmed Cases") + xlab("Date") +
  scale_x_date(breaks=confirmed_final$WEEK2, date_labels="%m/%d") +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 30, size=10))

