# iii) (Johannes)

library(dplyr)
library(vcd)  # Für Assoziationsmaße wie Phi-Koeffizient

# Funktion: Deskriptive bivariate Statistik für den Zusammenhang zwischen zwei kategoriale Variablen
deskriptive_bivariate_kategorial <- function(data, var1, var2) {
  
  # Kontingenztabelle
  cat("\nKontingenztabelle:\n")
  tbl <- table(data[[var1]], data[[var2]])
  print(tbl)
  
  # Relative Häufigkeiten (gesamt)
  cat("\nRelative Häufigkeiten (gesamt):\n")
  print(prop.table(tbl))
  
  # Zeilenweise relative Häufigkeiten 
  # --> Verteilung von var2 innerhalb der Kategorien von var1
  cat("\nRelative Häufigkeiten (zeilenweise):\n")
  print(prop.table(tbl, margin = 1))
  
  # Spaltenweise relative Häufigkeiten
  # --> Verteilung von var1 innerhalb der Kategorien von var2
  cat("\nRelative Häufigkeiten (spaltenweise):\n")
  print(prop.table(tbl, margin = 2))
  
  # Korrigiertes Kontingenzmaß von Pearson
  n <- sum(tbl)     # Stichprobengröße
  r <- nrow(tbl)    # Anzahl Zeilen der Kontingenztabelle
  k <- ncol(tbl)    # Anzahl Spalten der Kontingenztabelle
  
  # Chi-Quadrat Test nach Pearson (wird nur benötigt, um den Chi-Quadrat-Wert zu erhalten)
  pearson <- chisq.test(tbl)
  
  # Korrigiertes Kontingenzmaß von Pearson
  KKP <- sqrt(pearson$statistic/(pearson$statistic + n)) * sqrt(min(r,k)/(min(r,k)-1))
  
  cat("\nKorrigiertes Kontingenzmaß von Pearson:\n")
  print(KKP)
}

# Beispielanwendung mit Titanic-Datensatz

titanic <- read.csv("titanic_clean.csv")
deskriptive_bivariate_kategorial(data = titanic, "Survived", "Sex")
