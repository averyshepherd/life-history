setwd("/Users/averyshepherd/Documents/School Stuff/Spring 2019")
BeforeData <- read.csv("ANT348K_Data.csv")
Data <- BeforeData[-c(27),]

repro <- log(Data$First_Reproduction[Data$Sex == "F"])
mass <- log(Data$Mass..kg.[Data$Sex == "F"])

plot(mass, repro,
     main = "Relationship between Mass and Age First Reproduction in Females",
     ylab = "Age First Reproduction (yrs)",
     xlab = "Adult Mass (kg)",
     pch = 19,
     col = Data$Type)
legend(-3.8, 2.6, 
       legend=c("Primate", "Non-Primate"),
       col=c("Red", "Green"),
       pch = 19)
abline(0.35600, 0.21598,
       col = "blue")

glm <- lm(log(First_Reproduction) ~ log(Mass..kg.) + Type, data = Data)

model <- lm(log(First_Reproduction) ~ log(Mass..kg.), data = Data)
summary(model)
hist(glm$residuals, main = "Model Residuals", xlab = "Residual", 
     col = "light grey", right = F)
plot(glm$fitted.values, glm$residuals, xlab = "Fitted Values", 
     ylab = "Residuals", main = "Residual Plot", pch = 20)
abline(h = 0, col = "red")

Data$Mass_c <- log(Data$Mass..kg.) - mean(log(Data$Mass..kg.))
Data$Reproduce <- log(Data$First_Reproduction)
Data$Reproduce

mean(log(Data$Mass..kg., na.rm = TRUE))
Data$Mass_c

glm_int <- lm(Reproduce ~ Mass_c * Type, data = Data)
summary(glm_int)
summary(glm)

