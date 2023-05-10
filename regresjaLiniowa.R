# Wczytanie zbioru danych zawierającego informacje o wartości Eksportu i Importu w krajach UE w roku 2018
# Badamy zależność występującą między wielkością importu, a eksportu
library(readxl)
liniowa <- read_excel("C:/Users/Rafał/Desktop/liniowa.xlsx")

# Podgląd danych
View(liniowa) 
head(liniowa)

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

#Tworzymy wykres regresji liniowej
abline(regresja, col="blue")

