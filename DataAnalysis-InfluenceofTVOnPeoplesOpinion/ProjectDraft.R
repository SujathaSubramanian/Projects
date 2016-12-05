load(url("http://bit.ly/dasi_gss_data"))

gss_clean <- gss[!is.na(gss$tvhours) & !is.na(gss$natarms),]


unique(gss_clean[,'year'])


gss_clean  <- gss_clean[gss_clean$year >  2005,]



n <- nrow(gss_clean)
tvhoursMean <- mean(gss_clean$tvhours)

boxplot(tvhours ~natarms ,data = gss_clean,xlab = "Opinion on Military Spend",ylab="TV Hours")

x <- cbind(n=by(gss_clean$tvhours,gss_clean$natarms, length))
x <- cbind(x,mean=by(gss_clean$tvhours,gss_clean$natarms, mean))
x <- cbind(x,sd=by(gss_clean$tvhours,gss_clean$natarms, sd))
m <- nrow(x)

SST <- sum((gss_clean$tvhours - tvhoursMean)^ 2)

NatArmTooMuch <- gss_clean[gss_clean$natarms=="Too Much",]
NatArmTooMuchMean <- mean(NatArmTooMuch$tvhours)
SSW1 <- sum((NatArmTooMuch$tvhours - NatArmTooMuchMean)^ 2)
NatArmTooMuchCount <- nrow(NatArmTooMuch)

NatArmTooLittle <- gss_clean[gss_clean$natarms=="Too Little",]
NatArmTooLittleMean <- mean(NatArmTooLittle$tvhours)
SSW2 <- sum((NatArmTooLittle$tvhours - NatArmTooLittleMean)^ 2)
NatArmTooLittleCount <- nrow(NatArmTooLittle)

NatArmAboutRight <-  gss_clean[gss_clean$natarms=="About Right",]
NatArmAboutRightMean <- mean(NatArmAboutRight$tvhours)
SSW3 <- sum((NatArmAboutRight$tvhours - NatArmAboutRightMean)^ 2)
NatArmAboutRightCount <- nrow(NatArmAboutRight)

SSW <- SSW1 + SSW2 + SSW3

SSB <- NatArmTooMuchCount * ((NatArmTooMuchMean - tvhoursMean )^ 2) +
  NatArmTooLittleCount * ((NatArmTooLittleMean -tvhoursMean )^ 2)+
  NatArmAboutRightCount * ((NatArmAboutRightMean -tvhoursMean )^ 2)

FStatistic <- (SSB / (m-1)) /(SSW / ( n-m))


pf(FStatistic,m-1,n-m,lower.tail=FALSE)
gss_clean.lm = lm(tvhours~natarms, data=gss_clean) 
anv <- anova(gss_clean.lm)
MSE <- anv[2,'Mean Sq']
DF <- anv[2,'Df']

anv
Alpha <- 0.05
NewAlpha <- Alpha /((m * (m - 1)) /2)

# Reject the null hypothesis. Atleast one pair of population mean are different.
#Comparing two pairs - i.e There is no difference in TV watching pattern between groups that say
#spending "Too Much" or "About Right" in arms.
#H0 - Mean(Too Much) - Mean(About Right)  = 0
#HA - Mean(Too Much) - Mean(About Right)  != 0

TStatisticError1 <- sqrt((MSE/NatArmTooMuchCount) + (MSE/NatArmAboutRightCount))
TStatisticTMVsAR = abs(NatArmTooMuchMean - NatArmAboutRightMean)/TStatisticError1
PValue1 <- 2*pt(TStatisticTMVsAR,DF,lower.tail=FALSE)

if (PValue1 > NewAlpha)
  print "Fail to reject null hypothesis"


#Comparing two pairs - i.e There is no difference in TV watching pattern between groups that say
#spending "Too Much" or "Too Little" in arms.
#H0 - Mean(Too Much) - Mean(Too Little)  = 0
#HA - Mean(Too Much) - Mean(Too Little)  != 0
TStatisticError2 <- sqrt((MSE/NatArmTooMuchCount) + (MSE/NatArmTooLittleCount))
TStatisticTMVsTL = abs(NatArmTooMuchMean - NatArmTooLittleMean)/TStatisticError2


PValue2 <- 2*pt(TStatisticTMVsTL,DF,lower.tail=FALSE)
PValue2 > NewAlpha



#Comparing two pairs - i.e There is no difference in TV watching pattern between groups that say
#spending "Too Much" or "Too Little" in arms.
#H0 - Mean(About Right) - Mean(Too Little)  = 0
#HA - Mean(About Right) - Mean(Too Little)  != 0
TStatisticError3 <- sqrt((MSE/NatArmAboutRightCount) + (MSE/NatArmTooLittleCount))
TStatisticARVsTL = abs(NatArmAboutRightMean - NatArmTooLittleMean)/TStatisticError3


PValue3 <- 2*pt(TStatisticARVsTL,DF,lower.tail=FALSE)
PValue3 > NewAlpha


source("http://bit.ly/dasi_inference")

inference(y=gss_clean$tvhours,x=gss_clean$natarms,est="mean",type="ht",alternative="greater",method="theoretical")

gss_subset <- gss[!is.na(gss$tvhours) & !is.na(gss$natarms),c('year','tvhours','natarms')]

