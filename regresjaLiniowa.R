# Wczytanie zbioru danych zawierającego informacje o wartości Eksportu i Importu w krajach UE w roku 2018
# Badamy zależność występującą między wielkością importu, a eksportu
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
# Standard Error of the estimates i T value dostarczają informacji o tym jak wartości P zostały obliczone
# Gdy są równe 0 to nie mają dużej użyteczności w modelu
# Na końcu znajdują się wartości p dla oszacowanych parametrów
# Wartość p dla importu jest mniejsza niż 0,05 co oznacza, że zmienna jest statystycznie istotna
# Wartość p wynosi mniej niż 0.0000000000000002. Oznacza to, że zmienna pozwala na wiarygodne przypuszczanie wielkości eksportu
# Wartość R^2 wynosi aż 0.9927. Oznacza to, że zmienna Import wyjaśnia ponad 99% zmienności

#Dodajemy linię regresji do naszego wykresu
abline(regresja, col="blue")





# EKSPORT VS INWESTYCJE

# Tworzymy wykres plot rozkłający dane na wykresie typu XY
plot(Eksport~Inwestycje, data=liniowa)

# Tworzymy regresję liniową
regresja2 <- lm(Eksport~Inwestycje, data=liniowa)

# Najważniejsze informacje o regresji
summary(regresja2)
# Standard Error of the estimates i T value dostarczają informacji o tym jak wartości P zostały obliczone
# Gdy są równe 0 to nie mają dużej użyteczności w modelu
# Na końcu znajdują się wartości p dla oszacowanych parametrów
# Wartość p dla importu jest mniejsza niż 0,05 co oznacza, że zmienna jest statystycznie istotna
# Wartość p wynosi mniej niż 0.0000000000000002. Oznacza to, że zmienna pozwala na wiarygodne przypuszczanie wielkości eksportu
# Wartość R^2 wynosi aż 0.9927. Oznacza to, że zmienna Import wyjaśnia ponad 99% zmienności

#Dodajemy linię regresji do naszego wykresu
abline(regresja2, col="blue")
