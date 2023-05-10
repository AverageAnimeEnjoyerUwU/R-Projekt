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

# GLM - Generalized Linear Models
# bedziemy sprawdzac jaka zmienna daje najwieksze prawdopodobienstwo ze wystapi niska satysfakcja zyciowa

#tworzymy model przewidujący wpływ przeciętnej tygodniowej liczby godzin spędzonych w pracy na niską satysfakcję życiową
#uzyskujemy szczegółowe dane o regresji logistycznej
logisticWorkHours <- glm(LowLifeSat~AvgWeeklyWorkHours, data=logiTabela, family="binomial")
summary(logisticWorkHours)
#tworzymy model przewidujący wpływ ilości osób posiadających wykształcenie wyższe w danym kraju na niską satysfakcję życiową
#uzyskujemy szczegółowe dane o regresji logistycznej
logisticTertiaryEdu <- glm(LowLifeSat~TertiaryEduAtt, data=logiTabela, family="binomial")
summary(logisticTertiaryEdu)
# tworzymy model sprawdzający wszystkie zmienne wpływające na niską satysfakcję życiową, uzyskujemy szczegółowe dane o regresji logistycznej
logisticAll <- glm(LowLifeSat~AvgWeeklyWorkHours+TertiaryEduAtt, data=logiTabela, family="binomial")
summary(logisticAll)

#wykres przedstawiający wpływ przeciętnej ilości tygodniowych godzin w pracy na satysfakcję życiową
plot(logiTabela$AvgWeeklyWorkHours, logiTabela$LowLifeSat, main="Weekly hours vs Life sat.", xlab="Average number of usual weekly hours of work", ylab="(prob. of) Low Life Sat.", ylim=c(0,1), las=1, yaxt="n")
axis(side=2, at=c(0,1), labels=c("High Life Sat.", "Low Life Sat."))
#pionowa czerwona linia wspomagająca, która pokazuje od jakiej liczby przeciętnych tygodniowych godzin spędzonych w pracy w próbie badawczej pojawiają się
#pierwsze przypadki niskiej satysfakcji życiowej
abline(v=38, col="red")
#ilość przypadków wystąpienia niskiej satysfakcji życiowej wśród krajów w których przeciętna tygodniowa liczba godzin spędzonych w pracy jest mniejsza bądź
#równa 38 i ilość przypadków występienia niskiej satysfakcji życiowej wśród krajów w których przeciętna tygodniowa liczba godzin spędzonych w pracy jest większa
#niż 38
logiTabela$LowLifeSat[logiTabela$AvgWeeklyWorkHours<="38"]
logiTabela$LowLifeSat[logiTabela$AvgWeeklyWorkHours>"38"]
#prawdopodobieństwo wystąpienia niskiej satysfakcji życiowej poniżej 38 godzin i równe 38 godzin
mean(logiTabela$LowLifeSat[logiTabela$AvgWeeklyWorkHours<="38"])
#prawdopodobieństwo wystąpienia niskiej satysfakcji życiowej powyżej 38 godzin
mean(logiTabela$LowLifeSat[logiTabela$AvgWeeklyWorkHours>"38"])

#wykres przedstawiający wpływ ilości osób posiadających wykształcenie wyższe w danym kraju na ich satysfakcję życiową
plot(logiTabela$TertiaryEduAtt, logiTabela$LowLifeSat, main="TertiaryEduAtt vs Life sat.", xlab="Tertiary educational attainment", ylab="(prob. of) Low Life Sat.", ylim=c(0,1), las=1, yaxt="n")
axis(side=2, at=c(0,1), labels=c("High Life Sat.", "Low Life Sat."))
#pionowe czerwone linie wspomagające
abline(v=30, col="red")
abline(v=36, col="red")
abline(v=45, col="red")
#ilość przypadków wystąpienia niskiej satysfakcji życiowej wśród krajów w których ilość osób posiadających wykształcenie wyższe stanowi mniej niż 30% obywateli
logiTabela$LowLifeSat[logiTabela$TertiaryEduAtt<="30"]
#ilość przypadków wystąpienia niskiej satysfakcji życiowej wśród krajów w których ilość osób posiadających wykształcenie wyższe stanowi więcej niż 30% i mniej
#niż 36%
logiTabela$LowLifeSat[logiTabela$TertiaryEduAtt>"30" & logiTabela$TertiaryEduAtt<="36"]
#prawdopodobieństwo wystąpienia niskiej satysfakcji życiowej wśród krajów w których ilość osób posiadających wykształcenie wyższe stanowi więcej niż 30% i mniej
#niż 36%
mean(logiTabela$LowLifeSat[logiTabela$TertiaryEduAtt>"30" & logiTabela$TertiaryEduAtt<="36"])
#ilość przypadków wystąpienia niskiej satysfakcji życiowej wśród krajów w których ilość osób posiadających wykształcenie wyższe stanowi więcej niż 36% i mniej
#niż 45%
logiTabela$LowLifeSat[logiTabela$TertiaryEduAtt>"36" & logiTabela$TertiaryEduAtt<="45"]
#prawdopodobieństwo wystąpienia niskiej satysfakcji życiowej wśród krajów w których ilość osób posiadających wykształcenie wyższe stanowi więcej niż 36% i mniej
#niż 45%
mean(logiTabela$LowLifeSat[logiTabela$TertiaryEduAtt>"36" & logiTabela$TertiaryEduAtt<="45"])
#ilość przypadków wystąpienia niskiej satysfakcji życiowej wśród krajów w których ilość osób posiadających wykształcenie wyższe stanowi więcej niż 45%
logiTabela$LowLifeSat[logiTabela$TertiaryEduAtt>"45"]
#prawdopodobieństwo wystąpienia niskiej satysfakcji życiowej wśród krajów w których ilość osób posiadających wykształcenie wyższe stanowi więcej niż 45%
mean(logiTabela$LowLifeSat[logiTabela$TertiaryEduAtt>"45"])

#MODELE REGRESJI LOGISTYCZNEJ

#MODEL AvgWeeklyWorkHours vs LowLifeSat
#dopasowanie modelu regresji logistycznej
model <- glm(LowLifeSat~AvgWeeklyWorkHours, data=logiTabela, family="binomial")
#definiowanie nowego data frame zawierającego zmienną predykcyjną
newdata <- data.frame(AvgWeeklyWorkHours=seq(min(logiTabela$AvgWeeklyWorkHours), max(logiTabela$AvgWeeklyWorkHours),len=500))
#używamy modelu do przewidywania wartości
newdata$LowLifeSat = predict(model, newdata, type="response")
#tworzymy ostateczny wykres regresji logistycznej zawierający krzywą
plot(LowLifeSat ~ AvgWeeklyWorkHours, data=logiTabela, col="steelblue")
lines(LowLifeSat~AvgWeeklyWorkHours, newdata, lwd=2)

#MODEL TertiaryEduAtt vs LowLifeSat
#dopasowanie modelu regresji logistycznej
model2 <- glm(LowLifeSat~TertiaryEduAtt, data=logiTabela, family="binomial")
#definiowanie nowego data frame zawierającego zmienną predykcyjną
newdata2 <- data.frame(TertiaryEduAtt=seq(min(logiTabela$TertiaryEduAtt), max(logiTabela$TertiaryEduAtt),len=500))
#używamy modelu do przewidywania wartości
newdata2$LowLifeSat = predict(model2, newdata2, type="response")
#tworzymy ostateczny wykres regresji logistycznej zawierający krzywą
plot(LowLifeSat ~ TertiaryEduAtt, data=logiTabela, col="steelblue")
lines(LowLifeSat~TertiaryEduAtt, newdata2, lwd=2)