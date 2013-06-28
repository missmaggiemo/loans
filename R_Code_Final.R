#R Code Final

#Updated 6/27/2013

#download data
download.file("https://spark-public.s3.amazonaws.com/dataanalysis/loansData.csv", destfile="./loansData.csv", method="curl")

#save date
dateDownloaded <- date()


loansData <- read.csv("./loansData.csv")

#save workspace
save(list=ls(all=T), file="loansData.rda")

#download codebook
download.file("https://spark-public.s3.amazonaws.com/dataanalysis/loansCodebook.pdf", destfile="./loansCodebook.pdf", method="curl")

#Exploratory Analysis

names(loansData)


head(loansData,4)


summary(loansData)

#Any missing values?

which(matrix(is.na(loansData), nrow=2500, ncol=14), arr.ind=T)

#Rows 367 and 1595 have missing values. I'm not sure what it might affect if I
#take them out, so I'll leave them in and accomodate if I need to.

#"The purpose of your analysis is to identify and quantify associations between
#the interest rate of the loan and the other variables in the data set. In
#particular, you should consider whether any of these variables have an
#important association with interest rate after taking into account the
#applicant's FICO score. For example, if two people have the same FICO score,
#can the other variables explain a difference in interest rate between them?"

summary(loansData$Interest)

levels(loansData$Interest)

#It looks like the loansData$Interest column has been leveled non-intuitively. Let me fix that!

Interest <- loansData$Interest
levels(Interest) -> levelsI
levelsN <- c(levelsI[230:275], levelsI[1:229])
InterestN <- factor(Interest, levelsN)
loansDataCorr <- loansData
loansDataCorr$Interest.Rate <- InterestN

par(mfrow=c(1,2))
barplot(table(loansDataCorr$Interest), col="blue", main="loansDataCorr$Interest")
barplot(table(loansData$Interest), col="blue", main="loansData$Interest")

#Ah! Much better!


par(mfrow=c(1,1))

plot(as.numeric(loansDataCorr$FICO), as.numeric(loansDataCorr$Interest), xlab="FICO Range, low to high", ylab="Interest Rate, low to high", main="Interest Rate vs. FICO Range")

#Interest Rate and FICO score seem to be linearly correlated, with some variance.

par(mfrow=c(1,2))

plot(as.numeric(loansDataCorr$FICO), as.numeric(loansDataCorr$Interest), xlab="FICO Range, low to high", ylab="Interest Rate, low to high", main="Interest Rate vs. FICO Range, Colored by Loan Length", pch=19, col=as.numeric(loansDataCorr$Loan.L))
legend(25,275,col=unique(as.numeric(loansDataCorr$Loan.L)), legend=unique(loansDataCorr$Loan.L), pch=19)

boxplot(as.numeric(loansDataCorr$Interest) ~ loansDataCorr$Loan.L, xlab="Loan Length, 36 or 60 months", ylab="Interest Rate, low to high")

#Longer loans seem to have higher interest rates.

t.test(as.numeric(loansDataCorr$Interest) ~ as.factor(loansDataCorr$Loan.L), alternative="less", conf.level=0.99)

#p-value < 2.2e-16, very significant


par(mfrow=c(1,1))

boxplot(as.numeric(loansDataCorr$Interest) ~ loansDataCorr$Inquiries, loansDataCorr, xlab="Number of Inquiries in Past 6 Months", ylab="Interest Rate, low to high", main="Interets Rate vs. Inquiries")

plot(as.numeric(loansDataCorr$Amount.R), as.numeric(loansDataCorr$Interest), xlab="Amount Requested, low to high", ylab="Interest Rate, low to high", main="Interest Rate vs. Amount Requested")

#Make Amount.Requested into a more manageable factor variable. Use cut2() from Hmisc package.

library(Hmisc)

AmountRFact <- cut2(loansDataCorr$Amount.R, g=4)

boxplot(as.numeric(loansDataCorr$Interest) ~ AmountRFact, xlab="Amount Requested, in 4 Groupings", ylab="Interest Rate, low to high", main="Interest Rate vs. Amount Requested, Grouped")

TukeyHSD(aov(as.numeric(loansDataCorr$Interest) ~ AmountRFact))

#The group with the most money requested has a significantly different mean
#interest rate compared to all three other loan amount groups.

boxplot(as.numeric(loansDataCorr$Interest) ~ loansDataCorr$Loan.P, xlab="Loan Purpose", ylab="Interest Rate, low to high", main="Interest Rate vs. Loan Purpose")

par(mfrow=c(1,3))

hist(loansDataCorr$Monthly, breaks=50, col="blue", xlab="Monthly Income", main="Monthly Income")
hist(log(loansDataCorr$Monthly), breaks=50, col="blue", xlab="log(Monthly Income)", main="Monthly Income, log")
plot(log(loansDataCorr$Monthly), as.numeric(loansDataCorr$Interest), xlab="log(Monthly Income)", ylab="Interest Rate, low to high", main = "Interest Rate vs. log(Monthly Income")

#I think I can leave out monthly income.

Debt <- loansDataCorr$Debt
levels(Debt) -> levelsD
levelsD
levelsDN <- levelsDN <- c(levelsD[1:56], levelsD[740:772], levelsD[1223:1265], levelsD[1335:1669], levelsD[57:739], levelsD[773:1222], levelsD[1266:1334])
DebtN <- factor(Debt, levelsDN)
loansDataCC <- loansDataCorr
loansDataCC$Debt.To.Income.Ratio <- DebtN

par(mfrow=c(1,2))
barplot(table(loansDataCorr$Debt), col="blue", main="loansDataCorr$Debt")
barplot(table(loansDataCC$Debt), col="blue", main="loansDataCC$Debt")

par(mfrow=c(1,2))

plot(as.numeric(loansDataCC$Debt), as.numeric(loansDataCC$Interest), xlab="Debt-to-Income Ratio, low to high", ylab="Interest Rate, low to high", main="Interest Rate vs. Debt")
plot(as.numeric(loansDataCC$Debt), as.numeric(loansDataCC$FICO), xlab="Debt-to-Income Ratio, low to high", ylab="FICO Score Range, low to high", main="FICO vs. Debt")


#What does FICO score alone look like?

hist(as.numeric(loansDataCC$FICO), breaks=100, col="blue", main="Histogram of FICO Score Ranges", xlab="FICO Score Ranges")

#It looks skewed. Let's take the log() of FICO scores.

hist(log(as.numeric(loansDataCC$FICO)), breaks=50, col="blue", main="Histogram of log(FICO Score Ranges)", xlab="log(FICO Score Ranges)")

#Better.

plot(log(as.numeric(loansDataCC$FICO)), as.numeric(loansDataCC$Interest), xlab="log(FICO Score Ranges), low to high", ylab="Interest Range, low to high", main="Interest Rate vs. log(FICO Score Ranges)")

#Interest Rate vs. FICO Score Ranges seems to follow a fairly linear pattern. I'll use a linear regression model.

#In order to not overload my model, I'm not including loan purpose.

#Linear model regression: FICO, loan length, amount requested, debt, inquiries,
#revolving credit balance, open credit, monthly income, and combined factors.

lm <- lm(as.numeric(loansDataCC$Interest) ~ log(as.numeric(loansDataCC$FICO)) + as.factor(loansDataCC$Loan.L) + as.factor(AmountRFact) + as.factor(AmountRFact)*log(as.numeric(loansDataCC$FICO)) + as.numeric(loansDataCC$Debt) + as.numeric(loansDataCC$Inquiries) + as.numeric(loansDataCC$Open.C)*as.numeric(loansDataCC$Debt))

summary(lm)

confint(lm)

anova(lm)

par(mfrow=c(2,2))

plot(log(as.numeric(loansDataCC$FICO[which(is.na(loansDataCC$Inquiries)==F)])), as.numeric(loansDataCC$Interest[which(is.na(loansDataCC$Inquiries)==F)]), xlab="log(FICO Range), low to high", ylab="Interest Rate, low to high", main="Interest Rate vs. FICO Score Range")

plot(as.numeric(loansDataCorr$FICO), as.numeric(loansDataCorr$Interest), xlab="FICO Range, low to high", ylab="Interest Rate, low to high", main="Interest Rate vs. FICO Range, Colored by Loan Length", pch=19, col=as.numeric(loansDataCorr$Loan.L))
legend(33,300,col=unique(as.numeric(loansDataCorr$Loan.L)), legend=unique(loansDataCorr$Loan.L), pch=19, cex=0.5)

plot(as.numeric(loansDataCC$FICO[which(is.na(loansDataCC$Inquiries)==F)]), as.numeric(loansDataCC$Interest[which(is.na(loansDataCC$Inquiries)==F)]), xlab="FICO Range, low to high", ylab="Interest Rate, low to high", main="Interest vs. FICO with Linear Model Fitted Points")
points(as.numeric(loansDataCC$FICO[which(is.na(loansDataCC$Inquiries)==F)]), lm$fitted, pch=19, col="blue")

plot(lm$fitted, lm$residuals, xlab="LM Fitted", ylab="LM Residuals", main="Linear Model Residuals")
abline(c(0,0), lwd=3, col="blue")




