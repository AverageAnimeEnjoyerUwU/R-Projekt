# Wczytanie zbioru danych zawierającego informacje o wartości Eksportu i Importu w krajach UE w roku 2018
# Badamy zależność występującą między wielkością importu, a wielkością eksportu oraz między wielkością inwestycji, a wielkością eksportu
library(readxl)
liniowa <- read_excel("C:/Users/Rafał/Desktop/liniowaTest.xlsx")

# Podgląd danych
View(liniowa) 
head(liniowa)


# EKSPORT VS IMPORT

# Tworzymy wykres plot rozkłający dane na wykresie typu XY
plot(Eksport~Import, data=liniowa)

# Tworzymy regresję liniową
regresja <- lm(Eksport~Import, data=liniowa)

# Najważniejsze informacje o regresji
summary(regresja)


#Dodajemy linię regresji do naszego wykresu
abline(regresja, col="blue")





# EKSPORT VS INWESTYCJE

# Tworzymy wykres plot rozkłający dane na wykresie typu XY
plot(Eksport~Inwestycje, data=liniowa)

# Tworzymy regresję liniową
regresja2 <- lm(Eksport~Inwestycje, data=liniowa)

# Najważniejsze informacje o regresji
summary(regresja2)


#Dodajemy linię regresji do naszego wykresu
abline(regresja2, col="blue")
