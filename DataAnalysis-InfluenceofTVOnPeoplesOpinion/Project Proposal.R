load(url("http://bit.ly/dasi_gss_data"))

gss_clean <- gss[!is.na(gss$tvhours) & !is.na(gss$natarms),]
boxplot(tvhours ~natarms ,data = gss_clean,xlab = "Opinion on Military Spend",ylab="TV Hours")

by(gss_clean$tvhours,gss_clean$natarms, mean)


#
#Fitting each of the opinion in the normal distribution 
#with various degrees of freedom
#
by(gss_clean$tvhours,gss_clean$natarms, mean)

NatArmTooMuch=gss_clean[gss_clean$natarms=="Too Much",]

qqnorm(NatArmTooMuch$tvhours);
qqline(NatArmTooMuch$tvhours)


NatArmTooLittle=gss_clean[gss_clean$natarms=="Too Little",]

qqnorm(NatArmTooLittle$tvhours);
qqline(NatArmTooLittle$tvhours)

NatArmAboutRight=gss_clean[gss_clean$natarms=="About Right",]

qqnorm(NatArmAboutRight$tvhours);
qqline(NatArmAboutRight$tvhours)

hist(NatArmTooMuch$tvhours)
hist(NatArmTooLittle$tvhours)
hist(NatArmAboutRight$tvhours)


qqnorm(gss_clean$tvhours~gss_clean$natarms)

gss_clean.lm = lm(tvhours~natarms, data=gss_clean) 
anova(gss_clean.lm)
summary(gss_clean.lm)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(gss_clean.lm, las = 1)      # Residuals, Fitted, ...

par(opar)



gss_clean.stdres = rstandard(gss_clean.lm)
qqnorm(gss_clean.stdres,ylab="Standardized Hours",xlab="Normal Scores", main="") 
qqline(gss_clean.stdres) 

