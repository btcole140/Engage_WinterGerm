setwd("/Users/karensamis/Google Drive/NSERC Engage/Methods and Data/Germination Data/New Experiment Nov_16/Engage_WinterGerm")
#lab computer
setwd("/Users/mac/Google Drive/NSERC Engage/Methods and Data/Germination Data/New Experiment Nov_16/Engage_WinterGerm")
#home computer

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

EngWG$D2Emerg <- as.numeric(EngWG$D2Emerg)

EngWG$Date.Sowed <- as.Date(EngWG$Date.Sowed, "%d-%b-%y")
EngWG$DateEmerg <- as.Date(EngWG$DateEmerg, "%d-%b-%y")

write.table(EngWG, file = "Engage2016_WinterGermExp_set.csv", sep = ",", col.names = TRUE, row.names = FALSE)

#Subset data
EngWG1 <- subset(EngWG, Rep == "1")
EngWG1x <- subset(EngWG1, Survival == "1")
EngWG2 <- subset(EngWG, Rep == "2")

EngWG1 <- droplevels(EngWG1)
EngWG2 <- droplevels(EngWG2)

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
hist(EngWG1x$D2Emerg) #skew left
EngWG1x$LogD2Emerg <- log10(EngWG1x$D2Emerg+1)
hist(EngWG1x$LogD2Emerg) #centered, but many gaps
EngWG1x$sqrtD2Emerg <- sqrt(EngWG1x$D2Emerg+0.5)
hist(EngWG1x$sqrtD2Emerg) #bit better shape than log  but still gaps
EngWG1x$rankD2Emerg <- rank(EngWG1x$D2Emerg) #*

hist(EngWG1x$WetWt) #skew left
EngWG1x$LogWetWt <- log10(EngWG1x$WetWt+1)
hist(EngWG1x$LogWetWt) #more centered, but off shape
EngWG1x$sqrtWetWt<- sqrt(EngWG1x$WetWt+0.5)
hist(EngWG1x$sqrtWetWt) #bit better shape than log, but still not great
EngWG1x$rankWetWt <- rank(EngWG1x$WetWt) #*

hist(EngWG1x$A_WetWt) #skew left
EngWG1x$LogA_WetWt <- log10(EngWG1x$A_WetWt+1)
hist(EngWG1x$LogA_WetWt) #good shape and centered
EngWG1x$sqrtA_WetWt <- sqrt(EngWG1x$A_WetWt+0.5)
hist(EngWG1x$sqrtA_WetWt) #bit better shape than log but skewed left again *
EngWG1x$rankA_WetWt <- rank(EngWG1x$A_WetWt)

hist(EngWG1x$B_WetWt) #skew left
EngWG1x$LogB_WetWt <- log10(EngWG1x$B_WetWt+1)
hist(EngWG1x$LogB_WetWt) #good shape and centered
EngWG1x$sqrtB_WetWt <- sqrt(EngWG1x$B_WetWt+0.5)
hist(EngWG1x$sqrtB_WetWt) #bit better shape than log *
EngWG1x$rankB_WetWt <- rank(EngWG1x$B_WetWt)

boxplot(rankD2Emerg~Trtmt, data=EngWG1x) #likely diff between 100% and rest
boxplot(rankWetWt~Trtmt, data=EngWG1x) #likely diff between 100% and rest
boxplot(sqrtA_WetWt~Trtmt, data=EngWG1x) #likely diff between 100% and rest
boxplot(sqrtB_WetWt~Trtmt, data=EngWG1x) #may not see differences


#*******************************
#D2Emerg
SumEngD2E<- summarySE(EngWG1x, measurevar="rankD2Emerg", groupvars=c("Sp", "Trtmt")) 
GGEngD2E <- ggplot(data=SumEngD2E, aes(x=Trtmt, y=rankD2Emerg, group=Sp, shape=Sp)) +
  geom_errorbar(aes(ymin=rankD2Emerg-se, ymax=rankD2Emerg+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab("Ranked Days to Emerge") +
  scale_shape(name="Species") + ggtitle("WGerm1 Days 2 Emerg\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.75))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

  #interesting spike in lettuce days to emerge at the 100% trtmt level... these plants took
    #quite a bit longer. All other trtmts don't seem to vary much in basil and lettuce.


#*******************************
#WetWt
SumEngWWt<- summarySE(EngWG1x, measurevar="rankWetWt", groupvars=c("Sp", "Trtmt")) 
GGEngWWt <- ggplot(data=SumEngWWt, aes(x=Trtmt, y=rankWetWt, group=Sp, shape=Sp)) +
  geom_errorbar(aes(ymin=rankWetWt-se, ymax=rankWetWt+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab("Ranked Harvest Weight (mg)") +
  scale_shape(name="Species") + ggtitle("WGerm1 Harvest Weight\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.75))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

  #Harvest weight in lettuce and basil does not appear to vary between trtmts.
  

#*******************************
#A_WetWt
SumEngAWt<- summarySE(EngWG1x, measurevar="sqrtA_WetWt", groupvars=c("Sp", "Trtmt")) 
GGEngAWt <- ggplot(data=SumEngAWt, aes(x=Trtmt, y=sqrtA_WetWt, group=Sp, shape=Sp)) +
  geom_errorbar(aes(ymin=sqrtA_WetWt-se, ymax=sqrtA_WetWt+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab(expression(sqrt(Above~Ground~Harvest~Weight(mg)))) +
  scale_shape(name="Species") + ggtitle("WGerm1 AboveG Harvest Weight\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.75))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

  #possible difference in above gound harvest weight of lettuce in trtmt 50% MBE compared to
    #other trtmts.

#*******************************
#B_WetWt
SumEngBWt<- summarySE(EngWG1x, measurevar="sqrtB_WetWt", groupvars=c("Sp", "Trtmt")) 
GGEngBWt <- ggplot(data=SumEngBWt, aes(x=Trtmt, y=sqrtB_WetWt, group=Sp, shape=Sp)) +
  geom_errorbar(aes(ymin=sqrtB_WetWt-se, ymax=sqrtB_WetWt+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab(expression(sqrt(Below~Ground~Harvest~Weight(mg)))) +
  scale_shape(name="Species") + ggtitle("WGerm1 BelowG Harvest Weight\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.75))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

  #unlikely to see any variation between trtmts with either species... all standard errors
    #overlap