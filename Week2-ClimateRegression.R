climate = read.csv('climate_change.csv')
str(climate)

climate_train = subset(climate, Year <= 2006)
climate_test = subset(climate, Year > 2006)

str(climate_train)

model1 = lm(Temp ~ MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols, data = climate_train)
summary(model1)

model1$residuals
SSE = sum(model1$residuals^2)
SSE


cor(climate_train$N2O, climate_train$MEI)
cor(climate_train$N2O, climate_train$CO2)
cor(climate_train$N2O, climate_train$CH4)
cor(climate_train$N2O, climate_train$CFC.11)
cor(climate_train$N2O, climate_train$CFC.12)
cor(climate_train$N2O, climate_train$Aerosols)
cor(climate_train$N2O, climate_train$TSI)

cor(climate_train$CFC.11, climate_train$MEI)
cor(climate_train$CFC.11, climate_train$CO2)
cor(climate_train$CFC.11, climate_train$CH4)
cor(climate_train$CFC.11, climate_train$N2O)
cor(climate_train$CFC.11, climate_train$CFC.12)
cor(climate_train$CFC.11, climate_train$Aerosols)
cor(climate_train$CFC.11, climate_train$TSI)


model2 = lm(Temp ~ MEI+TSI+Aerosols+N2O, data = climate_train)
summary(model2)

model3 = step(model1)
summary(model3)

climate_prediction = predict(model3, newdata = climate_test)

# Compute out-of-sample R^2
SSE = sum((climate_prediction - climate_test$Temp)^2)
SST = sum((mean(climate_train$Temp) - climate_test$Temp)^2)
R2 = 1 - SSE/SST
R2
