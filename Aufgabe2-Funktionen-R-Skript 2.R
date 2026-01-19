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