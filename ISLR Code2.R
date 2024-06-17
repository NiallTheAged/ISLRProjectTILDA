## Thanks again Siobhan, here is the code to run on Wave 1 and Wave 3
## First you'll need the following programs
install.packages("leaps")
install.packages("ISLR")
##("ggplot2")
##install.packages("lattice")
install.packages("caret")
##install.packages("tidyverse")
##install.packages("dplyr")
##install.packages("haven")
#knitr::stitch_rhtml('N:\\User Folders\\ncostello_H\\Datasets\\SS Work\\Niall Rcode.R')
setwd("N:/User Folders/ncostello_H/Datasets/SS Work")
library("leaps")
library("ISLR")
library("ggplot2")
library("lattice")
library("caret")
library("tidyverse")
library("dplyr")
library("haven")


## Read in wave 1 and 3
Tilda_Wave_1 <- read_sav("C:/Users/ncostello_H/Desktop/ncostello_H/User`s Folder/TILDA Datasets/Wave 1/Tilda Wave 1.sav")
Tilda_Wave_3 <- read_sav("C:/Users/ncostello_H/Desktop/ncostello_H/User`s Folder/TILDA Datasets/Wave 3/Tilda Wave 3.sav")
TildaMRI <- read_dta("C:/Users/ncostello_H/Desktop/ncostello_H/User`s Folder/TILDA Datasets/MRI/MRI_combined_data_v1-0.dta")
TildaDOT <- read_dta("C:/Users/ncostello_H/Desktop/ncostello_H/User`s Folder/TILDA Datasets/MRI/DOT_vars_transfer_300124.dta")


## You will have to find the column number of COGtrail1time and COGtrail2time in both wave 1 and 3 and change the names before merging
COLNUMCTT1T1 <- which(colnames(Tilda_Wave_1) == "COGtrail1time")
print(COLNUMCTT1T1)
COLNUMCTT2T1 <- which(colnames(Tilda_Wave_1) == "COGtrail2time")
print(COLNUMCTT2T1)
COLNUMCTT1T2 <- which(colnames(Tilda_Wave_3) == "COGtrail1time")
print(COLNUMCTT1T2)
COLNUMCTT2T2 <- which(colnames(Tilda_Wave_3) == "COGtrail2time")
print(COLNUMCTT2T2)

colnames(Tilda_Wave_1)[34] <- "CTT1T1"
colnames(Tilda_Wave_1)[35] <- "CTT2T1"
colnames(Tilda_Wave_3)[1651] <- "CTT1T2"
colnames(Tilda_Wave_3)[1652] <- "CTT2T2"

## drop nas from soc act
table(Tilda_Wave_1$SCQSocAct1)
table(Tilda_Wave_1$SCQSocAct2)
table(Tilda_Wave_1$SCQSocAct3)
table(Tilda_Wave_1$SCQSocAct4)
table(Tilda_Wave_1$SCQSocAct5)
table(Tilda_Wave_1$SCQSocAct6)
table(Tilda_Wave_1$SCQSocAct7)
table(Tilda_Wave_1$SCQSocAct8)
table(Tilda_Wave_1$SCQSocAct9)
table(Tilda_Wave_1$SCQSocAct10)
table(Tilda_Wave_1$SCQSocAct11)
table(Tilda_Wave_1$SCQSocAct12)
table(Tilda_Wave_1$SCQSocAct13)
table(Tilda_Wave_1$SCQSocAct14)

Tilda_Wave_1 <- Tilda_Wave_1 %>% 
  mutate(SCQSocAct1 = ifelse(SCQSocAct1 %in% c (-878, -867, -865, -812, -834, -845, -823), NA, SCQSocAct1)) %>% 
  mutate(SCQSocAct2 = ifelse(SCQSocAct2 %in% c (-878, -867, -865, -812, -834, -845, -823, 865), NA, SCQSocAct2)) %>% 
  mutate(SCQSocAct3 = ifelse(SCQSocAct3 %in% c (-878, -867, -865, -812, -834, -845, -823, -856), NA, SCQSocAct3)) %>% 
  mutate(SCQSocAct4 = ifelse(SCQSocAct4 %in% c (-878, -867, -865, -812, -834, -845, -823), NA, SCQSocAct4)) %>% 
  mutate(SCQSocAct5 = ifelse(SCQSocAct5 %in% c (-878, -867, -865, -812, -834, -845, -823), NA, SCQSocAct5)) %>% 
  mutate(SCQSocAct6 = ifelse(SCQSocAct6 %in% c (-878, -867, -865, -812, -834, -845, -823, 865), NA, SCQSocAct6)) %>% 
  mutate(SCQSocAct7 = ifelse(SCQSocAct7 %in% c (-878, -867, -865, -812, -834, -845, -823, -856), NA, SCQSocAct7)) %>% 
  mutate(SCQSocAct8 = ifelse(SCQSocAct8 %in% c (-878, -867, -865, -812, -834, -845, -823), NA, SCQSocAct8)) %>% 
  mutate(SCQSocAct9 = ifelse(SCQSocAct9 %in% c (-878, -867, -865, -812, -834, -845, -823, -856), NA, SCQSocAct9)) %>% 
  mutate(SCQSocAct10 = ifelse(SCQSocAct10 %in% c (-878, -867, -865, -812, -834, -845, -823, -856), NA, SCQSocAct10)) %>% 
  mutate(SCQSocAct11 = ifelse(SCQSocAct11 %in% c (-878, -867, -865, -812, -834, -845, -823), NA, SCQSocAct11)) %>% 
  mutate(SCQSocAct12 = ifelse(SCQSocAct12 %in% c (-878, -867, -865, -812, -834, -845, -823), NA, SCQSocAct12)) %>% 
  mutate(SCQSocAct13 = ifelse(SCQSocAct13 %in% c (-878, -867, -865, -812, -834, -845, -823), NA, SCQSocAct13)) %>% 
  mutate(SCQSocAct14 = ifelse(SCQSocAct14 %in% c (-878, -867, -865, -812, -834, -845, -823), NA, SCQSocAct14))

Tilda_Wave_3 <- Tilda_Wave_3 %>% 
  mutate(SCQSocAct1 = ifelse(SCQSocAct1 %in% c (-878, -867, -865, -812, -834, -845, -823, -99), NA, SCQSocAct1)) %>% 
  mutate(SCQSocAct2 = ifelse(SCQSocAct2 %in% c (-878, -867, -865, -812, -834, -845, -823, -99), NA, SCQSocAct2)) %>% 
  mutate(SCQSocAct3 = ifelse(SCQSocAct3 %in% c (-878, -867, -865, -812, -834, -845, -823, -99), NA, SCQSocAct3)) %>% 
  mutate(SCQSocAct4 = ifelse(SCQSocAct4 %in% c (-878, -867, -865, -812, -834, -845, -823, -99), NA, SCQSocAct4)) %>% 
  mutate(SCQSocAct5 = ifelse(SCQSocAct5 %in% c (-878, -867, -865, -812, -834, -845, -823, -99), NA, SCQSocAct5)) %>% 
  mutate(SCQSocAct6 = ifelse(SCQSocAct6 %in% c (-878, -867, -865, -812, -834, -845, -823, -99), NA, SCQSocAct6)) %>% 
  mutate(SCQSocAct7 = ifelse(SCQSocAct7 %in% c (-878, -867, -865, -812, -834, -845, -823, -99), NA, SCQSocAct7)) %>% 
  mutate(SCQSocAct8 = ifelse(SCQSocAct8 %in% c (-878, -867, -865, -812, -834, -845, -823, -99), NA, SCQSocAct8)) %>% 
  mutate(SCQSocAct9 = ifelse(SCQSocAct9 %in% c (-878, -867, -865, -812, -834, -845, -823, -99), NA, SCQSocAct9)) %>% 
  mutate(SCQSocAct10 = ifelse(SCQSocAct10 %in% c (-878, -867, -865, -812, -834, -845, -823, -99), NA, SCQSocAct10)) %>% 
  mutate(SCQSocAct11 = ifelse(SCQSocAct11 %in% c (-878, -867, -865, -812, -834, -845, -823, -99, -856), NA, SCQSocAct11)) %>% 
  mutate(SCQSocAct12 = ifelse(SCQSocAct12 %in% c (-878, -867, -865, -812, -834, -845, -823, -99), NA, SCQSocAct12)) %>% 
  mutate(SCQSocAct13 = ifelse(SCQSocAct13 %in% c (-878, -867, -865, -812, -834, -845, -823, -99), NA, SCQSocAct13)) %>% 
  mutate(SCQSocAct14 = ifelse(SCQSocAct14 %in% c (-878, -867, -865, -812, -834, -845, -823, -99), NA, SCQSocAct14))





## Could you very quickly reverse code the wave 1 SocAct variables please so 1 = Never and 8 = Daily
## I FORGOT HOW TO LOOP IN TILDYVERSE!!! :=( this will have to do for the moment as I'm writing this in the airport
Tilda_Wave_1$SCQSocAct1 <- 9 - Tilda_Wave_1$SCQSocAct1
Tilda_Wave_1$SCQSocAct2 <- 9 - Tilda_Wave_1$SCQSocAct2
Tilda_Wave_1$SCQSocAct3 <- 9 - Tilda_Wave_1$SCQSocAct3
Tilda_Wave_1$SCQSocAct4 <- 9 - Tilda_Wave_1$SCQSocAct4
Tilda_Wave_1$SCQSocAct5 <- 9 - Tilda_Wave_1$SCQSocAct5
Tilda_Wave_1$SCQSocAct6 <- 9 - Tilda_Wave_1$SCQSocAct6
Tilda_Wave_1$SCQSocAct7 <- 9 - Tilda_Wave_1$SCQSocAct7
Tilda_Wave_1$SCQSocAct8 <- 9 - Tilda_Wave_1$SCQSocAct8
Tilda_Wave_1$SCQSocAct9 <- 9 - Tilda_Wave_1$SCQSocAct9
Tilda_Wave_1$SCQSocAct10 <- 9 - Tilda_Wave_1$SCQSocAct10
Tilda_Wave_1$SCQSocAct11 <- 9 - Tilda_Wave_1$SCQSocAct11
Tilda_Wave_1$SCQSocAct12 <- 9 - Tilda_Wave_1$SCQSocAct12
Tilda_Wave_1$SCQSocAct13 <- 9 - Tilda_Wave_1$SCQSocAct13
Tilda_Wave_1$SCQSocAct14 <- 9 - Tilda_Wave_1$SCQSocAct14

Tilda_Wave_3$SCQSocAct1 <- 9 - Tilda_Wave_3$SCQSocAct1
Tilda_Wave_3$SCQSocAct2 <- 9 - Tilda_Wave_3$SCQSocAct2
Tilda_Wave_3$SCQSocAct3 <- 9 - Tilda_Wave_3$SCQSocAct3
Tilda_Wave_3$SCQSocAct4 <- 9 - Tilda_Wave_3$SCQSocAct4
Tilda_Wave_3$SCQSocAct5 <- 9 - Tilda_Wave_3$SCQSocAct5
Tilda_Wave_3$SCQSocAct6 <- 9 - Tilda_Wave_3$SCQSocAct6
Tilda_Wave_3$SCQSocAct7 <- 9 - Tilda_Wave_3$SCQSocAct7
Tilda_Wave_3$SCQSocAct8 <- 9 - Tilda_Wave_3$SCQSocAct8
Tilda_Wave_3$SCQSocAct9 <- 9 - Tilda_Wave_3$SCQSocAct9
Tilda_Wave_3$SCQSocAct10 <- 9 - Tilda_Wave_3$SCQSocAct10
Tilda_Wave_3$SCQSocAct11 <- 9 - Tilda_Wave_3$SCQSocAct11
Tilda_Wave_3$SCQSocAct12 <- 9 - Tilda_Wave_3$SCQSocAct12
Tilda_Wave_3$SCQSocAct13 <- 9 - Tilda_Wave_3$SCQSocAct13
Tilda_Wave_3$SCQSocAct14 <- 9 - Tilda_Wave_3$SCQSocAct14

table(Tilda_Wave_3$SCQSocAct14)

## rename socact vs by wave

CNSAW1 <- which(colnames(Tilda_Wave_1) == "SCQSocAct1")
print(CNSAW1)
colnames(Tilda_Wave_1)[693] <- "W1SCQSocAct1"
colnames(Tilda_Wave_1)[694] <- "W1SCQSocAct2"
colnames(Tilda_Wave_1)[695] <- "W1SCQSocAct3"
colnames(Tilda_Wave_1)[696] <- "W1SCQSocAct4"
colnames(Tilda_Wave_1)[697] <- "W1SCQSocAct5"
colnames(Tilda_Wave_1)[698] <- "W1SCQSocAct6"
colnames(Tilda_Wave_1)[699] <- "W1SCQSocAct7"
colnames(Tilda_Wave_1)[700] <- "W1SCQSocAct8"
colnames(Tilda_Wave_1)[701] <- "W1SCQSocAct9"
colnames(Tilda_Wave_1)[702] <- "W1SCQSocAct10"
colnames(Tilda_Wave_1)[703] <- "W1SCQSocAct11"
colnames(Tilda_Wave_1)[704] <- "W1SCQSocAct12"
colnames(Tilda_Wave_1)[705] <- "W1SCQSocAct13"
colnames(Tilda_Wave_1)[706] <- "W1SCQSocAct14"
CNSAW3 <- which(colnames(Tilda_Wave_3) == "SCQSocAct1")
print(CNSAW3)
colnames(Tilda_Wave_3)[1611] <- "W3SCQSocAct1"
colnames(Tilda_Wave_3)[1612] <- "W3SCQSocAct2"
colnames(Tilda_Wave_3)[1613] <- "W3SCQSocAct3"
colnames(Tilda_Wave_3)[1614] <- "W3SCQSocAct4"
colnames(Tilda_Wave_3)[1615] <- "W3SCQSocAct5"
colnames(Tilda_Wave_3)[1616] <- "W3SCQSocAct6"
colnames(Tilda_Wave_3)[1617] <- "W3SCQSocAct7"
colnames(Tilda_Wave_3)[1618] <- "W3SCQSocAct8"
colnames(Tilda_Wave_3)[1619] <- "W3SCQSocAct9"
colnames(Tilda_Wave_3)[1620] <- "W3SCQSocAct10"
colnames(Tilda_Wave_3)[1621] <- "W3SCQSocAct11"
colnames(Tilda_Wave_3)[1622] <- "W3SCQSocAct12"
colnames(Tilda_Wave_3)[1623] <- "W3SCQSocAct13"
colnames(Tilda_Wave_3)[1624] <- "W3SCQSocAct14"
AgeW3 <- which(colnames(Tilda_Wave_3) == "age")
print(AgeW3)
colnames(Tilda_Wave_3)[3] <- "ageW3"
EduW3 <- which(colnames(Tilda_Wave_3) == "edu3")
print(EduW3)
colnames(Tilda_Wave_3)[7] <- "edu3W3"
sexW3 <- which(colnames(Tilda_Wave_3) == "sex")
print(sexW3)
colnames(Tilda_Wave_3)[5] <- "sexW3"



## Prepare data for merging 

TildaCogW1 <- Tilda_Wave_1 %>% 
  select("tilda_serial", "age", "sex", "CTT1T1", "CTT2T1", "edu3", "W1SCQSocAct1", "W1SCQSocAct2", "W1SCQSocAct3", "W1SCQSocAct4", "W1SCQSocAct5", "W1SCQSocAct6", "W1SCQSocAct7", "W1SCQSocAct8", "W1SCQSocAct9", "W1SCQSocAct10", 
         "W1SCQSocAct11", "W1SCQSocAct12", "W1SCQSocAct13", "W1SCQSocAct14")

TildaCogW3 <- Tilda_Wave_3 %>% 
  select("tilda_serial", "edu3W3", "ageW3", "sexW3", "CTT1T2", "CTT2T2", "W3SCQSocAct1", "W3SCQSocAct2", "W3SCQSocAct3", "W3SCQSocAct4", "W3SCQSocAct5", "W3SCQSocAct6", "W3SCQSocAct7", "W3SCQSocAct8", "W3SCQSocAct9", "W3SCQSocAct10", 
         "W3SCQSocAct11", "W3SCQSocAct12", "W3SCQSocAct13", "W3SCQSocAct14")

TildaMRIred <- TildaMRI %>% 
  select("tilda_serial", "brainpad", "lwm_frontal_scheltensscores", "mwm_occipital_scheltensscores", "lwm_parietal_scheltensscores", "lwm_temporal_scheltensscores")

colnames(TildaMRIred)[3] <- "FrontalWMH"
colnames(TildaMRIred)[4] <- "OccipitalWMH"
colnames(TildaMRIred)[5] <- "ParietalWMH"
colnames(TildaMRIred)[6] <- "TemporalWMH"

TildaMRIred <- TildaMRIred %>% 
  mutate(TemporalWMH_Int = as.integer(TemporalWMH))

TildaDOT <- TildaDOT %>% 
  mutate(DOT_Int = as.integer(DOT_Code_Combined))

TildaDOT$Digit1 <- as.integer(substr(TildaDOT$DOT_Code_Combined, 1,1))
TildaDOT$Digit2 <- as.integer(substr(TildaDOT$DOT_Code_Combined, 2,2))
TildaDOT$Digit3 <- as.integer(substr(TildaDOT$DOT_Code_Combined, 3,3))

TildaDOT <- TildaDOT %>% 
  mutate(OccComplex21 = Digit1 + Digit2 + Digit3)



## Merge Waves 1 and 3 

MergedCogW <- merge(TildaCogW1, TildaCogW3, by = c("tilda_serial"))



##  Calculate regression scores for Processing Speed Change

RegPS <- lm(CTT1T2 ~ CTT1T1, data = MergedCogW)
summary(RegPS)
PSRC <- coef(RegPS)[2]
MergedCogW <- MergedCogW %>% 
  mutate(ChangeSpeed = RC * CTT1T1)
summary_text1 <- capture.output((summary(RegPS)))
write.table(summary_text1, "PSReg.txt", col.names = FALSE, quote = FALSE)

ggplot(MergedCogW, aes(x = CTT1T1, y = CTT1T2)) +
  geom_point(color = "blue") + 
  labs(title = "Scatter of Speed", x = "speed T1", y = "Speed T2")

## Calculate regression scores for EF

RegEF <- lm(CTT2T2 ~ CTT2T1, data = MergedCogW)
summary(RegEF)
EFRC <- coef(RegEF)[2]
MergedCogW <- MergedCogW %>% 
  mutate(ChangeEF = RC * CTT2T1)
summary_text1 <- capture.output((summary(RegPS)))
write.table(summary_text1, "EFReg.txt", col.names = FALSE, quote = FALSE)

ggplot(MergedCogW, aes(x = CTT2T1, y = CTT2T2)) +
  geom_point(color = "blue") + 
  labs(title = "Scatter of EF", x = "EF T1", y = "EF T2")

summary_text <- capture.output(hist(MergedCogW$IPSChange))
write.table(summary_text, "FWMH_EngModelSum.txt", col.names = FALSE, quote = FALSE)

## Subtract t2 from t1 variable again

MergedCogW <- MergedCogW %>% 
  mutate(IPSChange = CTT1T2 - CTT1T1,
         ZSSpeed = scale(IPSChange))

hist(MergeFull$IPSChange)


MergedCogW <- MergedCogW %>% 
  mutate(EFChange = CTT2T2 - CTT2T1,
         ZSEF = scale(EFChange))

hist(MergeFull$EFChange)


## merge rest of sets
MergeMRI <- merge(MergedCogW, TildaMRIred, by = c("tilda_serial"))
MergeFull <- merge(MergeMRI, TildaDOT, by = c("tilda_serial"))

hist(MergeFull$ChangeSpeed)


## Next I want to run a ML feature selection algorithm on the social activities variables 
## from wave 1 to see which set are optimal for CTT1T1

ISLRPST1 <- MergedCogW %>% 
  select("ageW3", "sex", "edu3W3", "CTT1T1", "W1SCQSocAct1", "W1SCQSocAct2", "W1SCQSocAct3", 
         "W1SCQSocAct4", "W1SCQSocAct5", "W1SCQSocAct6", "W1SCQSocAct7", "W1SCQSocAct8", "W1SCQSocAct9",
         "W1SCQSocAct10", "W1SCQSocAct11", "W1SCQSocAct12", "W1SCQSocAct13", "W1SCQSocAct14")

## Don't think you can run a FIML so you'll have to drop all NAs from the model

ISLRPST1 <- na.omit(ISLRPST1)

##Fit the model to run the iterative regressions
fitPST1 = regsubsets(CTT1T1~.,  ISLRPST1, nvmax = 17)
summary(fitPST1)
summary(fitPST1)$rsq
plot(fitPST1,scale= "bic")

##Saving output PST1
summary_text1 <- capture.output((summary(fitPST1)))
write.table(summary_text1, "fitPST1.txt", col.names = FALSE, quote = FALSE)
summary_text1 <- capture.output((summary(fitPST1)$rsq))
write.table(summary_text1, "fitPSR2T1.txt", col.names = FALSE, quote = FALSE)

modelPST1 <- lm(CTT1T1 ~ ageW3 + sex + edu3W3 + W1SCQSocAct8 + W1SCQSocAct6 + W1SCQSocAct5 + W1SCQSocAct9 + W1SCQSocAct13, data = MergedCogW)
summary(modelPST1)
summary_text1 <- capture.output((summary(modelPST1)))
write.table(summary_text1, "Engagement&SpeedRegressionT1.txt", col.names = FALSE, quote = FALSE)

## from wave 3 to see which set are optimal for CTT1T2

ISLRPST2 <- MergedCogW %>% 
  select("ageW3", "sex", "edu3W3", "CTT1T2", "W3SCQSocAct1", "W3SCQSocAct2", "W3SCQSocAct3", 
         "W3SCQSocAct4", "W3SCQSocAct5", "W3SCQSocAct6", "W3SCQSocAct7", "W3SCQSocAct8", "W3SCQSocAct9",
         "W3SCQSocAct10", "W3SCQSocAct11", "W3SCQSocAct12", "W3SCQSocAct13", "W3SCQSocAct14")

## Don't think you can run a FIML so you'll have to drop all NAs from the model

ISLRPST2 <- na.omit(ISLRPST2)

##Fit the model to run the iterative regressions
fitPST2 = regsubsets(CTT1T2~.,  ISLRPST2, nvmax = 17)
summary(fitPST2)
summary(fitPST2)$rsq
plot(fitPST2,scale= "bic")

modelPST2 <- lm(CTT1T2 ~ ageW3 + sex + edu3W3 + W3SCQSocAct8 + W3SCQSocAct6 + W3SCQSocAct5 + W1SCQSocAct9 + W1SCQSocAct13, data = MergedCogW)
summary(modelPST2)

##Saving output PST2
summary_text1 <- capture.output((summary(fitPST2)))
write.table(summary_text1, "fitPST2.txt", col.names = FALSE, quote = FALSE)
summary_text1 <- capture.output((summary(fitPST2)$rsq))
write.table(summary_text1, "fitPSR2T2.txt", col.names = FALSE, quote = FALSE)

modelPST2 <- lm(CTT1T2 ~ ageW3 + sex + edu3W3 + W3SCQSocAct8 + W3SCQSocAct6 + W3SCQSocAct5 + W3SCQSocAct9 + W3SCQSocAct13, data = MergedCogW)
summary(modelPST2)
summary_text1 <- capture.output((summary(modelPST2)))
write.table(summary_text1, "Engagement&SpeedRegressionT2.txt", col.names = FALSE, quote = FALSE)

## Now check change in PS score with same model

ISLRPSCh <- MergedCogW %>% 
  select("ageW3", "sex", "edu3W3", "IPSChange", "W3SCQSocAct1", "W3SCQSocAct2", "W3SCQSocAct3", 
         "W3SCQSocAct4", "W3SCQSocAct5", "W3SCQSocAct6", "W3SCQSocAct7", "W3SCQSocAct8", "W3SCQSocAct9",
         "W3SCQSocAct10", "W3SCQSocAct11", "W3SCQSocAct12", "W3SCQSocAct13", "W3SCQSocAct14")

## Don't think you can run a FIML so you'll have to drop all NAs from the model

ISLRPSCh <- na.omit(ISLRPSCh)

##Fit the model to run the iterative regressions
ISLRPSCh = regsubsets(IPSChange~.,  ISLRPSCh, nvmax = 17)
summary(ISLRPSCh)
summary(ISLRPSCh)$rsq
plot(ISLRPSCh,scale= "bic")

modelPSch <- lm(IPSChange ~ ageW3 + sex + edu3W3 + W3SCQSocAct8 + W3SCQSocAct6 + W3SCQSocAct5 + W1SCQSocAct9 + W1SCQSocAct13, data = MergedCogW)
summary(modelPSch)

## Check the control change method in regression on EF

modelcontrolT1PS <- lm(CTT1T2 ~ CTT1T1 + ageW3 + sex + edu3W3 + W3SCQSocAct8 + W3SCQSocAct6 + W3SCQSocAct5 + W1SCQSocAct9 + W1SCQSocAct13, data = MergedCogW)
summary(modelcontrolT1PS)


## Then do the same with score of EF 

ISLREFT1 <- MergedCogW %>% 
  select("age", "sex", "edu3W3", "CTT2T1", "W1SCQSocAct1", "W1SCQSocAct2", "W1SCQSocAct3", 
         "W1SCQSocAct4", "W1SCQSocAct5", "W1SCQSocAct6", "W1SCQSocAct7", "W1SCQSocAct8", "W1SCQSocAct9",
         "W1SCQSocAct10", "W1SCQSocAct11", "W1SCQSocAct12", "W1SCQSocAct13", "W1SCQSocAct14")

## Don't think you can run a FIML so you'll have to drop all NAs from the model

ISLREFT1 <- na.omit(ISLREFT1)

##Fit the model to run the iterative regressions
fitEFT1 = regsubsets(CTT2T1~.,  ISLREFT1, nvmax = 17)
summary(fitEFT1)
summary(fitEFT1)$rsq
plot(fitEFT1,scale= "bic")


##Saving output EFT1
summary_text1 <- capture.output((summary(fitEFT1)))
write.table(summary_text1, "fitEFT1.txt", col.names = FALSE, quote = FALSE)
summary_text1 <- capture.output((summary(fitEFT1)$rsq))
write.table(summary_text1, "fitEFR2T1.txt", col.names = FALSE, quote = FALSE)

modelEFT1 <- lm(CTT2T1 ~ ageW3 + sex + edu3W3 + W1SCQSocAct8 + W1SCQSocAct6 + W1SCQSocAct5 + W1SCQSocAct9 + W1SCQSocAct13, data = MergedCogW)
summary(modelEFT1)
summary_text1 <- capture.output((summary(modelEFT1)))
write.table(summary_text1, "Engagement&EFRegressionT1.txt", col.names = FALSE, quote = FALSE)



## Now for EF at T2

ISLREFT2 <- MergedCogW %>% 
  select("ageW3", "sex", "edu3W3", "CTT2T2", "W3SCQSocAct1", "W3SCQSocAct2", "W3SCQSocAct3", 
         "W3SCQSocAct4", "W3SCQSocAct5", "W3SCQSocAct6", "W3SCQSocAct7", "W3SCQSocAct8", "W3SCQSocAct9",
         "W3SCQSocAct10", "W3SCQSocAct11", "W3SCQSocAct12", "W3SCQSocAct13", "W3SCQSocAct14")

## Don't think you can run a FIML so you'll have to drop all NAs from the model

ISLREFT2 <- na.omit(ISLREFT2)

##Fit the model to run the iterative regressions
fitEFT2 = regsubsets(CTT2T2~.,  ISLREFT2, nvmax = 17)
summary(fitEFT2)
summary(fitEFT2)$rsq
plot(fitEFT2,scale= "bic")

##Saving output EFT1
summary_text1 <- capture.output((summary(fitEFT2)))
write.table(summary_text1, "fitEFT2.txt", col.names = FALSE, quote = FALSE)
summary_text1 <- capture.output((summary(fitEFT2)$rsq))
write.table(summary_text1, "fit2EFT2.txt", col.names = FALSE, quote = FALSE)

modelPST2 <- lm(CTT2T2 ~ ageW3 + sex + edu3W3 + W1SCQSocAct8 + W1SCQSocAct6 + W1SCQSocAct5 + W1SCQSocAct9 + W1SCQSocAct13, data = MergedCogW)
summary(modelPST2)
summary_text1 <- capture.output((summary(modelPST2)))
write.table(summary_text1, "Engagement&EFRegressionT2.txt", col.names = FALSE, quote = FALSE)


## EF Change ISLR

ISLREFCh <- MergedCogW %>% 
  select("ageW3", "sex", "edu3W3", "EFChange", "W3SCQSocAct1", "W3SCQSocAct2", "W3SCQSocAct3", 
         "W3SCQSocAct4", "W3SCQSocAct5", "W3SCQSocAct6", "W3SCQSocAct7", "W3SCQSocAct8", "W3SCQSocAct9",
         "W3SCQSocAct10", "W3SCQSocAct11", "W3SCQSocAct12", "W3SCQSocAct13", "W3SCQSocAct14")

## Don't think you can run a FIML so you'll have to drop all NAs from the model

ISLREFCh <- na.omit(ISLREFCh)

##Fit the model to run the iterative regressions
ISLREFCh = regsubsets(EFChange~.,  ISLREFCh, nvmax = 17)
summary(ISLRPSCh)
summary(ISLRPSCh)$rsq
plot(ISLRPSCh,scale= "bic")

modelEFch <- lm(EFChange ~ ageW3 + sex + edu3W3 + W3SCQSocAct8 + W3SCQSocAct6 + W3SCQSocAct5 + W1SCQSocAct9 + W1SCQSocAct13, data = MergedCogW)
summary(modelEFch)

## Check the control change method in regression on EF

modelcontrolT1EF <- lm(CTT2T2 ~ CTT2T1 + Age80plusW3 + sex + edu3W3 + W3SCQSocAct8 + W3SCQSocAct6 + W3SCQSocAct5 + W1SCQSocAct9 + W1SCQSocAct13, data = MergedCogW)
summary(modelcontrolT1EF)

## Now PS model  with T1 Control

modelcontrolT1PS <- lm(CTT1T2 ~ CTT1T1 + ageW3 + sex + edu3W3 + W3SCQSocAct8 + W3SCQSocAct6 + W3SCQSocAct5 + W3SCQSocAct9 + W3SCQSocAct13, data = MergedCogW)
summary(modelcontrolT1PS)

summary_text1 <- capture.output((summary(modelcontrolT1PS)))
write.table(summary_text1, "Engagement&PSRegressionControlT1.txt", col.names = FALSE, quote = FALSE)
## EF regression with T1 Control

modelcontrolT1EF <- lm(CTT2T2 ~ CTT2T1 + ageW3 + sex + edu3W3 + W3SCQSocAct8 + W3SCQSocAct6 + W3SCQSocAct5 + W1SCQSocAct9 + W1SCQSocAct13, data = MergedCogW)
summary(modelcontrolT1EF)

summary_text1 <- capture.output((summary(modelcontrolT1EF)))
write.table(summary_text1, "Engagement&EFRegressionControlT1.txt", col.names = FALSE, quote = FALSE)


## Now PS model with age dummy variables 
## create dummy v of age 

MergedCogW <- MergedCogW %>%
  mutate(Age80plusW3 = ifelse(ageW3 >= 80, 1,0))

modelcontrol80PS <- lm(CTT1T2 ~ Age80plusW3 + sex + edu3W3 + W3SCQSocAct8 + W3SCQSocAct6 + W3SCQSocAct5 + W3SCQSocAct9 + W3SCQSocAct13, data = MergedCogW)
summary(modelcontrol80PS)

summary_text1 <- capture.output((summary(modelcontrol80PS)))
write.table(summary_text1, "Engagement&PSRegressionControl80+.txt", col.names = FALSE, quote = FALSE)
## EF for over 80

modelcontrol80EF <- lm(CTT2T2 ~ Age80plusW3 + sex + edu3W3 + W3SCQSocAct8 + W3SCQSocAct6 + W3SCQSocAct5 + W1SCQSocAct9 + W1SCQSocAct13, data = MergedCogW)
summary(modelcontrol80EF)

summary_text1 <- capture.output((summary(modelcontrol80EF)))
write.table(summary_text1, "Engagement&EFRegressionControl80+.txt", col.names = FALSE, quote = FALSE)


## do we see similar results in the MRI group

ISLRTildaPS500 <- MergeFull %>% 
  select("ageW3", "sex", "edu3W3", "ChangeSpeed", "W1SCQSocAct1", "W1SCQSocAct2", "W1SCQSocAct3", 
         "W1SCQSocAct4", "W1SCQSocAct5", "W1SCQSocAct6", "W1SCQSocAct7", "W1SCQSocAct8", "W1SCQSocAct9",
         "W1SCQSocAct10", "W1SCQSocAct11", "W1SCQSocAct12", "W1SCQSocAct13", "W1SCQSocAct14")

## Don't think you can run a FIML so you'll have to drop all NAs from the model

ISLRTildaPS500 <- na.omit(ISLRTildaPS500)

##Fit the model to run the iterative regressions
fitPS500 = regsubsets(ChangeSpeed~.,  ISLRTildaPS500, nvmax = 17)
summary(fitPS500)
summary(fitPS500)$rsq
plot(fitPS500,scale= "bic")

modelPS500 <- lm(ZSSpeed ~ ageW3 + sex + edu3W3 + W1SCQSocAct12, data = MergeFull)
summary(modelPS500)

## Then do the same with the standardized change score of EF 

ISLRTildaEF500 <- MergeFull %>% 
  select("ageW3", "sex", "edu3W3", "ZSEF", "W1SCQSocAct1", "W1SCQSocAct2", "W1SCQSocAct3", 
         "W1SCQSocAct4", "W1SCQSocAct5", "W1SCQSocAct6", "W1SCQSocAct7", "W1SCQSocAct8", "W1SCQSocAct9",
         "W1SCQSocAct10", "W1SCQSocAct11", "W1SCQSocAct12", "W1SCQSocAct13", "W1SCQSocAct14")

## Don't think you can run a FIML so you'll have to drop all NAs from the model

ISLRTildaEF500 <- na.omit(ISLRTildaEF500)

##Fit the model to run the iterative regressions
fitEF500 = regsubsets(ZSEF~.,  ISLRTildaEF500, nvmax = 17)
summary(fitEF500)
summary(fitEF500)$rsq
plot(fitEF500,scale= "bic")

modelEF500 <- lm(ZSEF ~ ageW3 + sex + edu3W3 + W1SCQSocAct1 + W1SCQSocAct2 + W1SCQSocAct7 + W1SCQSocAct9, data = MergeFull)
summary(modelEF500)

hist(MergedCogW$edu3)
hist(MergeFull$edu3)

## MRI Data ISLR with SocAct Scores

ISLRTildaFWMH <- MergeFull %>% 
  select("ageW3", "sex", "edu3W3", "FrontalWMH", "W1SCQSocAct1", "W1SCQSocAct2", "W1SCQSocAct3", 
         "W1SCQSocAct4", "W1SCQSocAct5", "W1SCQSocAct6", "W1SCQSocAct7", "W1SCQSocAct8", "W1SCQSocAct9",
         "W1SCQSocAct10", "W1SCQSocAct11", "W1SCQSocAct12", "W1SCQSocAct13", "W1SCQSocAct14")

ISLRTildaFWMH <- na.omit(ISLRTildaFWMH)

##Fit the model to run the iterative regressions
fitFWMH = regsubsets(FrontalWMH~.,  ISLRTildaFWMH, nvmax = 17)
summary(fitFWMH)
summary(fitFWMH)$rsq
plot(fitFWMH,scale= "bic")

## Speed on FWH
SpeedFWH <- lm(CTT1T2 ~ FrontalWMH + edu3 + sex + ageW3, data = MergeFull)
summary(SpeedFWH)

FWMH_EngModel <- lm(FrontalWMH ~ W1SCQSocAct10 + edu3 + sex + ageW3, data = MergeFull)
summary(FWMH_EngModel)

hist(MergeFull$FrontalWMH)


## That should be it! There should be 6 resutls from between the two operations; 
summary(fitPS)
summary(fitPS)$rsq
plot(fitPS,scale= "bic") 

summary(fitEF) 
summary(fitEF)$rsq
plot(fitEF,scale= "bic") 
## Can you kindly send me the results please. Very much in your debt! 





