#importowanie zestawu danych
install.packages("readxl")
library(readxl)
logiTabela <- read_excel("C:/Users/Rafał/Desktop/logiTabela.xlsx")

#podgląd danych
View(logiTabela)
head(logiTabela)

#podsumowanie danych z tabeli
summary(logiTabela)

#sprawdzamy czy podział na high and low nie jest w 100% zależny od zmiennej
xtabs(~ logiTabela$LowLifeSat + logiTabela$AvgWeeklyWorkHours, data=logiTabela)
xtabs(~ logiTabela$LowLifeSat + logiTabela$TertiaryEduAtt, data=logiTabela)


#MODELE REGRESJI LOGISTYCZNEJ



#MODEL AvgWeeklyWorkHours vs LowLifeSat
#dopasowanie modelu regresji logistycznej
model <- glm(LowLifeSat~AvgWeeklyWorkHours, data=logiTabela, family="binomial")
#uzyskujemy szczegółowy raport wyników z regresji logistycznej
summary(model)
#definiowanie nowego data frame zawierającego zmienną predykcyjną
newdata <- data.frame(AvgWeeklyWorkHours=seq(min(logiTabela$AvgWeeklyWorkHours), max(logiTabela$AvgWeeklyWorkHours),len=500))
#używamy modelu do przewidywania wartości
newdata$LowLifeSat = predict(model, newdata, type="response")
#tworzymy ostateczny wykres regresji logistycznej zawierający krzywą
plot(LowLifeSat ~ AvgWeeklyWorkHours, data=logiTabela, col="steelblue")
lines(LowLifeSat~AvgWeeklyWorkHours, newdata, lwd=2)




#MODEL BadHealth vs LowLifeSat
#dopasowanie modelu regresji logistycznej
model2 <- glm(LowLifeSat~BadHealth, data=logiTabela, family="binomial")
#uzyskujemy szczegółowy raport wyników z regresji logistycznej
summary(model2)
#definiowanie nowego data frame zawierającego zmienną predykcyjną
newdata2 <- data.frame(BadHealth=seq(min(logiTabela$BadHealth), max(logiTabela$BadHealth),len=500))
#używamy modelu do przewidywania wartości
newdata2$LowLifeSat = predict(model2, newdata2, type="response")
#tworzymy ostateczny wykres regresji logistycznej zawierający krzywą
plot(LowLifeSat ~ BadHealth, data=logiTabela, col="steelblue")
lines(LowLifeSat~BadHealth, newdata2, lwd=2)


