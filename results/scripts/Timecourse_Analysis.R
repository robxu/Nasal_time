setwd("/Users/Robert/Documents/LINGUIST_245/Project_NasalTime/data/")
library(ggplot2)
library(lme4)
library(tidyverse)
library(bootstrap)

options(max.print = .Machine$integer.max)   # set unlimited view of rows

# Read data files
# The githut version should skip this and read downsized data file datasum1 and datasum3 instead
####### 
####### start skipping


# Sorting the huge eye tracking data file, downsize to workable file focusing on latency

# eyedata1 = read.csv("TimeCourse2-101-1.gazedata", sep = "\t")
# head(eyedata1)
# 
# eyedata3 = read.csv("TimeCourse2-103-1.gazedata", sep = "\t")
# head(eyedata3)
# 
# unique(eyedata3$CurrentObject)
# 
# # Get the critical part of the data file
# eyedata1_prime = subset(eyedata1, eyedata1$CurrentObject == "Prime")
# eyedata1_stimulus = subset(eyedata1, eyedata1$CurrentObject == "Stimulus")
# 
# eyedata1_critical = subset(eyedata1, eyedata1$CurrentObject == "Prime" | eyedata1$CurrentObject == "Stimulus")
# eyedata3_critical = subset(eyedata1, eyedata1$CurrentObject == "Prime" | eyedata1$CurrentObject == "Stimulus")
# 
# eyedata1_critical$PrimeWord = sapply(eyedata1_critical$Prime, 
#                                      function(x) {strsplit(as.character(x), "_")[[1]][1]})
# eyedata3_critical$PrimeWord = sapply(eyedata3_critical$Prime, 
#                                      function(x) {strsplit(as.character(x), "_")[[1]][1]})
# 
# eyedata1_critical$CorrectAOI = ifelse(eyedata1_critical$PrimeWord == as.character(eyedata1_critical$AOI1), 
#                                       1, 2) 
# eyedata3_critical$CorrectAOI = ifelse(eyedata3_critical$PrimeWord == as.character(eyedata1_critical$AOI1), 
#                                       1, 2) 
# 
# eyedata1_critical$RealAOI <- sapply(eyedata1_critical$XGazePosRightEye, 
#                                     function(x){if (x>0 && x<=0.375) return(1); if (x>0.625) return(2); return(NA)}
#                                    )
# eyedata3_critical$RealAOI <- sapply(eyedata3_critical$XGazePosRightEye, 
#                                     function(x){if (x>0 && x<=0.375) return(1); if (x>0.625) return(2); return(NA)}
#                                    )
# 
# infotab <- unique(eyedata1_critical %>% select(TrialId, Prime, AOI1, AOI2))
# firstfixation <- eyedata1_critical %>% 
#                   group_by(TrialId) %>% 
#                   summarise(FirstFixation = min(which(RealAOI == CorrectAOI)))
# infotab3 <- unique(eyedata3_critical %>% select(TrialId, Prime, AOI1, AOI2))
# firstfixation3 <- eyedata3_critical %>% 
#   group_by(TrialId) %>% 
#   summarise(FirstFixation = min(which(RealAOI == CorrectAOI)))
# 
# datasum1 = left_join(infotab, firstfixation, by = "TrialId")
# datasum1$FirstFixationTime <- datasum1$FirstFixation * 1000 / 300
# datasum1 = filter(datasum1, datasum1$FirstFixationTime<1000 & datasum1$FirstFixationTime>200)
# 
# datasum3 = left_join(infotab3, firstfixation3, by = "TrialId")
# datasum3$FirstFixationTime <- datasum3$FirstFixation * 1000 / 300
# datasum3 = filter(datasum3, datasum3$FirstFixationTime<1000 & datasum3$FirstFixationTime>200)
# 
# # datasum1$AOI1Nasal = sapply(datasum1$AOI1, function(s) {"n" %in% strsplit(as.character(s), "")[[1]]} )
# # datasum1$AOI1Voiced = sapply(datasum1$AOI1, function(s) {"d" %in% strsplit(as.character(s), "")[[1]]} )
# # 
# # datasum1$AOI2Nasal = sapply(datasum1$AOI2, function(s) {"n" %in% strsplit(as.character(s), "")[[1]]} )
# # datasum1$AOI2Voiced = sapply(datasum1$AOI2, function(s) {"d" %in% strsplit(as.character(s), "")[[1]]} )
# 
# datasum1$PrimeWord = sapply(datasum1$Prime, function(s) {strsplit(as.character(s), "_")[[1]][1]} )
# datasum1$PrimeNasal = sapply(datasum1$PrimeWord, function(s) {"n" %in% strsplit(as.character(s), "")[[1]]} )
# datasum1$PrimeVoiced = sapply(datasum1$PrimeWord, function(s) {"d" %in% strsplit(as.character(s), "")[[1]]} )
# datasum1$CompetitorWord = ifelse(as.character(datasum1$AOI1) == datasum1$PrimeWord, 
#                                  as.character(datasum1$AOI2), 
#                                  as.character(datasum1$AOI1))
# datasum1$CompetitorNasal = sapply(datasum1$CompetitorWord, function(s) {"n" %in% strsplit(as.character(s), "")[[1]]} )
# datasum1$CompetitorVoiced = sapply(datasum1$CompetitorWord, function(s) {"d" %in% strsplit(as.character(s), "")[[1]]} )
# 
# datasum1$PrimeTime = sapply(datasum1$Prime, function(s) {strsplit(as.character(s), "_")[[1]][2]} )
# 
# 
# datasum3$PrimeWord = sapply(datasum3$Prime, function(s) {strsplit(as.character(s), "_")[[1]][1]} )
# datasum3$PrimeNasal = sapply(datasum3$PrimeWord, function(s) {"n" %in% strsplit(as.character(s), "")[[1]]} )
# datasum3$PrimeVoiced = sapply(datasum3$PrimeWord, function(s) {"d" %in% strsplit(as.character(s), "")[[1]]} )
# datasum3$CompetitorWord = ifelse(as.character(datasum3$AOI1) == datasum3$PrimeWord, 
#                                  as.character(datasum3$AOI2), 
#                                  as.character(datasum3$AOI1))
# datasum3$CompetitorNasal = sapply(datasum3$CompetitorWord, function(s) {"n" %in% strsplit(as.character(s), "")[[1]]} )
# datasum3$CompetitorVoiced = sapply(datasum3$CompetitorWord, function(s) {"d" %in% strsplit(as.character(s), "")[[1]]} )
# 
# datasum3$PrimeTime = sapply(datasum3$Prime, function(s) {strsplit(as.character(s), "_")[[1]][2]} )
# 
# write.csv(datasum1,"datasum1.csv")
# write.csv(datasum3,"datasum3.csv")
# 
# datasum1$Subject = rep(1, nrow(datasum1))
# datasum3$Subject = rep(3, nrow(datasum3))
# 
# datasum1 = read.csv("datasum1.csv", sep = ",")
# datasum3 = read.csv("datasum3.csv", sep = ",")
# 
# datasum = bind_rows(datasum1, datasum3)
# datasum
# 

######
######Finish skipping


# Read datasum1 and datasum3 instead
# datasum1 datasum3 are downsized version of the data, only with right eye data
# The time course data were downsized to just the latency and useful annotated factors

# Read the downsized files
datasum1 = read.csv("datasum1.csv", sep = ",")
datasum3 = read.csv("datasum3.csv", sep = ",")
datasum = read.csv("datasum.csv", sep = ",")

datasum$CompetitorNas = sapply(datasum$CompetitorNasal, function(x){if (x == TRUE) return ("Nasal"); if (x == FALSE) return ("Oral")})

# Hypothesis A
Hyp_A = filter(datasum, PrimeNasal == TRUE & CompetitorNasal == FALSE & PrimeVoiced == CompetitorVoiced)

# t.test(FirstFixationTime ~ PrimeTime, Hyp_A, PrimeVoiced == FALSE)
# t.test(FirstFixationTime ~ PrimeTime, Hyp_A, PrimeVoiced == TRUE)

m = lmer(FirstFixationTime ~ PrimeVoiced * PrimeTime + (1|PrimeWord), data = Hyp_A, REML=F)
summary(m)

m0 = lm(FirstFixationTime ~ PrimeVoiced * PrimeTime, data = Hyp_A)
summary(m0)

m1 = lmer(FirstFixationTime ~ PrimeVoiced * PrimeTime + (1|PrimeWord) + (1|Subject), data = Hyp_A, REML=F)
summary(m1)

anova(m, m0)

sortingA = datasum %>%
  group_by(PrimeTime, PrimeVoiced) %>%
  summarise(MeanLatency = mean(FirstFixationTime), CI.Low = ci.low(FirstFixationTime), CI.High = ci.high(FirstFixationTime)) %>%
  mutate(YMin = MeanLatency - CI.Low, YMax = MeanLatency + CI.High)
sortingA

ggplot(filter(sortingA, PrimeTime != "NA" & PrimeVoiced == TRUE), aes(x=PrimeTime, y=MeanLatency)) +
  geom_bar(stat="identity",fill="grey",color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=.25) +
  xlab("Vowel nasalization timing (CVND)") +
  ylab("Mean latency (ms)")

ggplot(filter(sortingA, PrimeTime != "NA" & PrimeVoiced == FALSE), aes(x=PrimeTime, y=MeanLatency)) +
  geom_bar(stat="identity",fill="grey",color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=.25) +
  xlab("Vowel nasalization timing (CNVT)") +
  ylab("Mean latency (ms)")


# Hypothesis B
Hyp_B = filter(datasum, PrimeNasal == FALSE)

# t.test(FirstFixationTime ~ CompetitorNasal, Hyp_B, PrimeVoiced == FALSE)
# t.test(FirstFixationTime ~ CompetitorNasal, Hyp_B, PrimeVoiced == TRUE)

m = lmer(FirstFixationTime ~ PrimeVoiced * CompetitorNasal + (1|PrimeWord), data = Hyp_B, REML=F)
summary(m)


sortingB = datasum %>%
  group_by(CompetitorNasal, PrimeVoiced) %>%
  summarise(MeanLatency = mean(FirstFixationTime), CI.Low = ci.low(FirstFixationTime), CI.High = ci.high(FirstFixationTime)) %>%
  mutate(YMin = MeanLatency - CI.Low, YMax = MeanLatency + CI.High)
sortingB

sortingB$CompetitorNas = sapply(sortingB$CompetitorNasal, function(x){if (x == TRUE) return ("Nasal"); if (x == FALSE) return ("Oral")})


ggplot(filter(sortingB, PrimeVoiced == TRUE), aes(x=CompetitorNas, y=MeanLatency)) +
  geom_bar(stat="identity",fill="grey80",color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=.25) +
  xlab("CVD latency competing with CVT or CVND") +
  ylab("Mean latency (ms)")

ggplot(filter(sortingB, PrimeVoiced == FALSE), aes(x=CompetitorNas, y=MeanLatency)) +
  geom_bar(stat="identity",fill="grey80",color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=.25) +
  xlab("CVT latency competing with CVD or CVNT") +
  ylab("Mean latency (ms)")

nrow(datasum3)



# Some baselines
# Hyp01 = filter(datasum, PrimeNasal == TRUE & PrimeTime == "early")
# 
# t.test(FirstFixationTime ~ CompetitorNasal, Hyp01, PrimeVoiced = TRUE)
# t.test(FirstFixationTime ~ CompetitorNasal, Hyp01, PrimeVoiced = FALSE)
# 
# Hyp02 = filter(datasum, PrimeNasal == TRUE & PrimeTime == "late")
# 
# t.test(FirstFixationTime ~ CompetitorNasal, Hyp02, PrimeVoiced = TRUE)
# t.test(FirstFixationTime ~ CompetitorNasal, Hyp02, PrimeVoiced = FALSE)

