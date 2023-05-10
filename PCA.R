# Wczytanie zbioru danych przedstawiających sytuację gospodarczą wśród krajów UE
library(readxl)
analizy_dane <- read_excel("C:/Users/Rafał/Desktop/analizy-dane.xlsx")
View(analizy_dane)
# Wybranie zmiennych liczbowych (od 2 do 7)
dane=analizy_dane[,2:7]
head(dane)
# podsumowanie informacji o danych
summary(dane)
#analiza głóWnych składowych bez standaryzacji
res=princomp(dane)
# podstawowe informacje
print(res)
# najważniejsze informacje
summary(res)
# Cumulative Proportion dla PCx (gdzie x - numer głównej składowej) wskazuje ile skumulowanej zmienności dla zmiennych wyjaśnia dana składowa
# z informacji można wyczytać, że:
# pierwsza zmienna wyjaśnia 0.985 zmienności
# druga zmienna wyjaśnia 0.9975 zmienności
# trzecia zmienna wyjaśnia 0.99919 zmienności
# czwarta, piąta i szósta zmienna wyjaśniają 100% zmienności 
# (zwyle przyjmuje się, że wyjaśnienie 80% jest wystarczające)

plot(res)

biplot(res)
#wektory nie są prostopadłe, co oznacza, że zmienne są skorelowane
#WIĘCEJ INTERPRETACJI 
#WIĘCEJ INTERPRETACJI
#WIĘCEJ INTERPRETACJI
#WIĘCEJ INTERPRETACJI

# współrzędne obiektów w ramach składowych głównych
res$scores

# macierz ładunków dla głóWnych składowych
loadings(res)

# PCA ze standaryzacją danych
library(clusterSim)
z=data.Normalization(dane, "n1")
res=princomp(z)
plot(res)
summary(res)
biplot(res)
res$loadings
res$scores