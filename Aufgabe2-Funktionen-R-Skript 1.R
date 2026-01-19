source("Aufgabe2-Funktionen-R-Skript 2.R")

# Aufgabe 2a

# i)
# Funktion, die verschiedene geeignete deskriptive Statistiken für metrische Variablen
# berechnet und ausgibt

deskriptive_metrisch <- function(data) {
  min <- min(data) # Minimum berechnen mit base R Funktion
  max <- max(data) # Maximum berechnen mit base R Funktion
  mean <- mean(data) # arithmethisches Mittel berechnen mit base R Funktion
  med <- median(data) # Median berechnen mit base R Funktion
  var <- var(data) # Varianz berechnen mit base R Funktion
  sd <- sd(data) # Standardabweichung berechnen mit base R Funktion
  iqr <- Quantil_Funktion(data , p = 0.75) - Quantil_Funktion(data, p = 0.25) # Interquartilsabstand berechnen mit Hilfsfunktion Quantil_Funktion
  var_koef <- Variationskoeffizient_Funktion(data) # Variationskoeffizient berechnen mit Hilfsfunktion Variationskoeffizient_Funktion
  
  return(list("Minimum" = min, "Maximum" = max , "Mittelwert" = mean, "Median" = med,
              "Varianz" = var, "Standardabweichung" = sd, "Interquartilsabstand" = iqr,
              "Variationskoeffizient" = var_koef))
}

# Test:
df <- c(1 , 5, 3, 100 , 64 , 21)
deskriptive_metrisch(df)
