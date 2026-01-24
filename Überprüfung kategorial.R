
# Prüfen, ob eine der Variablen numerisch ist
# Falls ja: Abbruch, da die Funktion nur für kategoriale Variablen gedacht ist
if (is.numeric(data[[var1]]) || is.numeric(data[[var2]])) {
  stop("Beide Variablen müssen kategorial sein.")
}