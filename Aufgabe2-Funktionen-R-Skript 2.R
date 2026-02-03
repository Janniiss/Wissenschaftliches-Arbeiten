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

# 2.v
# Hilfsfunktion zur Überprüfung der Anzahl der Variablen
#
# Prüft, ob genau 3 oder 4 Variablen für die Visualisierung übergeben wurden.
#
# Eingabe:
# - data: Datensatz 
# - ... : Variablennamen der kategorialen Variablen
#
# Ausgabe:
# - Vektor mit den übergebenen Variablennamen
#
# Fehler:
# - Abbruch, wenn nicht genau 3 oder 4 Variablen angegeben werden
#
check <- function(data, ...) {
  var<-c(...)
  if (length(var) < 3 || length(var) > 4) {
    stop("Es sind nicht 3 oder 4 Variablen gegeben.")
    #Wenn es weniger als 3 oder mehr als 4 Variablen sind gibt es eine Fehlermeldung an
  }
  return(var)
}


# vi.) (Jannis)

check_vars <- function(data, ...) {
  vars <- c(...)
  
  # Prüfen ob Variablen existieren
  if (!all(vars %in% names(data))) {
    stop("Mindestens eine Variable existiert nicht im Datensatz.")
  }
return(vars)
}
  # Hilfsfunktion: kategorial erkennen
  is_cat <- function(x) {
    is.factor(x) || is.character(x)
  }

check_cat <- function(data, v) {
    if (!is_cat(data[[v]])) {
      stop(paste(
        "Fehler: Für eine einzelne metrische Variable ist keine Visualisierung vorgesehen:",
        v
      ))
    }
  }
