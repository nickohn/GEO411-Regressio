#Linear Regression

#SAT Regression
sat <- read.csv("SAT.csv")

#scatter plot + regression line
plot(sat$SAT, sat$GPA)

satModel <- lm(GPA ~ SAT, data = sat)
summary(satModel)

plot(sat$SAT, sat$GPA)
abline(reg = satModel)

#Diagnostic Plots (Residuals, QQ, Homoskedacity, Outlier Influence)
plot(satModel)


#Mosquito Regression
mosquitos <- read.csv("mosquitos.csv")
summary(mosquitos)

#Histograms
hist(mosquitos$AegPupae)
hist(mosquitos$DevelopmentSites)

#Scatter plot
plot(mosquitos$DevelopmentSites, mosquitos$AegPupae)

#Estimated regression model
mosqModLinear <- lm(AegPupae ~ DevelopmentSites, data = mosquitos)
summary(mosqModLinear)

#Scatter plot + estimation
plot(mosquitos$DevelopmentSites, mosquitos$AegPupae)
abline(reg = mosqModLinear)

#Diagnostic Plots (Residuals, QQ, Homoskedacity, Outlier Influence)
plot(mosqModLinear)


#Outlier removal (Mosquito dataset)
mosqModLinear30 <- lm(AegPupae ~ DevelopmentSites, data = mosquitos[-30,])
summary(mosqModLinear30)

#Plot both with and without outlier
plot(mosquitos$DevelopmentSites, mosquitos$AegPupae)
points(mosquitos$DevelopmentSites[30], mosquitos$AegPupae[30], col = "red")
abline(reg = mosqModLinear)
abline(reg = mosqModLinear30, col = "red")

#Diagnostic Plots (Residuals, QQ, Homoskedacity, Outlier Influence)
plot(mosqModLinear30)


#Single Log Model (Log of AegPupae)
hist(log(mosquitos$AegPupae))

#Estimate log model parameters
mosqModLog <- lm(log(AegPupae) ~ DevelopmentSites, data = mosquitos)
summary(mosqModLog)

#Scatter plot + log estimation
plot(mosquitos$DevelopmentSites, log(mosquitos$AegPupae))
abline(reg = mosqModLog)

#Diagnostic Plots (Residuals, QQ, Homoskedacity, Outlier Influence)
plot(mosqModLog)


#Double log model (both AegPupae and DevelopmentSites are logged)
hist(log(mosquitos$DevelopmentSites))

#Estimate log-log model parameters
mosqModLogLog <- lm(log(AegPupae) ~ log(DevelopmentSites), data = mosquitos)
summary(mosqModLogLog)

#Scatter plot + log-log estimation
plot(log(mosquitos$DevelopmentSites), log(mosquitos$AegPupae))
abline(reg = mosqModLogLog)

#Diagnostic Plots (Residuals, QQ, Homoskedacity, Outlier Influence)
plot(mosqModLogLog)