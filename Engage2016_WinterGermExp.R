setwd("/Users/karensamis/Google Drive/NSERC Engage/Methods and Data/Germination Data/New Experiment Nov_16/Engage_WinterGerm")

EngWG <- read.csv("Engage2016_WinterGermExp.csv")
View(EngWG)
str(EngWG)

EngWG$Trtmt <- as.factor(EngWG$Trtmt)
EngWG$Rep <- as.factor(EngWG$Rep)
EngWG$ID <- as.factor(EngWG$ID)
EngWG$Plate <- as.factor(EngWG$Plate)
EngWG$Tray <- as.factor(EngWG$Tray)
EngWG$Row <- as.factor(EngWG$Row)
EngWG$Col <- as.factor(EngWG$Col)
EngWG$H <- as.factor(EngWG$H)
EngWG$Ex <- as.factor(EngWG$Ex)
EngWG$C <- as.factor(EngWG$C)
EngWG$Survival <- as.factor(EngWG$Survival)

EngWG$Date.Sowed <- as.Date(EngWG$Date.Sowed, "%d-%b-%y")
EngWG$DateEmerg <- as.Date(EngWG$DateEmerg, "%d-%b-%y")

write.table(EngWG, file = "Engage2016_WinterGermExp_set.csv", sep = ",", col.names = TRUE, row.names = FALSE)

#Subset data
EngWG1 <- subset(EngWG, Rep == "1")
EngWG2 <- subset(EngWG, Rep == "2")


write.table(EngWG1, file = "Engage2016_WinterGermExp1_set.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(EngWG2, file = "Engage2016_WinterGermExp2_set.csv", sep = ",", col.names = TRUE, row.names = FALSE)


# Set Fuction to Summarize data
# Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
#   data: a data frame.
#   measurevar: the name of a column that contains the variable to be summariezed
#   groupvars: a vector containing names of columns that contain grouping variables
#   na.rm: a boolean that indicates whether to ignore NA's
#   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  length2 <- function (x, na.rm=TRUE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}

#alternatively, to see all in same table; include Lvs, Wet/DryWt, PercentWC (for now)
names(EngSC)
library(dplyr)
EngSC$PercentWC <- ((EngSC$WetWt-EngSC$DryWt)/EngSC$WetWt)*100
sem.na <- function(x)
{
  n = sum(x,na.rm=T)/mean(x,na.rm=T)
  sqrt(var(x,na.rm=T)/n)
}
means <- EngSC %>% 
  group_by(Trtmt) %>% 
  summarise(mLvs = mean(Lvs, na.rm = TRUE), sdLvs = sd(Lvs, na.rm = TRUE), seLvs = sem.na(Lvs), 
            mWetWt = mean(WetWt, na.rm = TRUE), sdWetWt = sd(WetWt, na.rm = TRUE), seWetWt = sem.na(WetWt),
            mDryWt = mean(DryWt, na.rm = TRUE), sdDryWt = sd(DryWt, na.rm = TRUE), seDryWt = sem.na(DryWt),
            mPWC = mean(PercentWC, na.rm = TRUE), sdPWC = sd(PercentWC, na.rm = TRUE), sePWC = sem.na(PercentWC)) %>% 
  arrange(Trtmt)
View(means)


#**************************Rep 1 only************************************
##histograms and transformations
hist(EngWG1$B_WetWt) #skew right
EngWG1$LogB_WetWt <- log10(EngWG1$B_WetWt+1)
hist(EngWG1$LogB_WetWt) #good shape and centered
EngWG1$sqrtB_WetWt <- sqrt(EngWG1$B_WetWt+0.5)
hist(EngWG1$sqrtB_WetWt) #bit better shape than log **
EngWG1$rankB_WetWt <- rank(EngWG1$B_WetWt)

boxplot(sqrtB_WetWt~Trtmt, data=EngWG1)
boxplot(WetWt~Trtmt, data=EngWG1)
boxplot(A_WetWt~Trtmt, data=EngWG1)
