# Aufgabe 2b

# Hilfsfunktionen:

# Funktion zum Berechnen des Variationskoeffizienten:
Variationskoeffizient_Funktion <- function(data) {
  if(mean(data) == 0) {
    return(NA)
  } # Rückgabe von NA falls der Mittelwert 0 ist
  var_koef <- (sd(data))/(mean(data)) # Variationskoeffizient berechnen
  return(var_koef)
}

# Funktion zum Berchnen von Quantilen:
Quantil_Funktion <- function(data , p) {
  if(is.numeric(p) == FALSE | p < 0 | p > 1) {
    stop(paste("p muss eine numerische Zahl im geschlossenen Intervall zwischen 0 und 1 sein."))
  } # Überprüfen, ob p eine Zahl zwischen 0 und 1 ist
  n <- length(data) # Länge von n berechnen
  np <- n*p
  sort_list <- sort(data) # geordnete Liste erstellen
  if(np %% 1 != 0) {
    quan <- sort_list[[floor(np) + 1]]
    # Quantil berechnen, wenn n*p keine natürliche Zahl ist
  } else {
    quan <- 1/2 * (sort_list[[np]] + sort_list[[np + 1]])
    # Quantil berchnen, wenn n*p eine natürliche Zahl ist
  }
  return(quan)
}
