library(ggplot2)

# 2.v) Gestapeltes Balkendiagramm für 3 oder 4 kategoriale Variablen
# Eingabe:
# - data: Datensatz
# - ... : 3 oder 4 Variablennamen als Strings (z.B. "Sex", "Pclass", "Survived")
# Ausgabe:
# - ggplot-Objekt (Plot) welcher die Variablen 1 und 2 vergleicht und die Variablen 3 und 4 als Facets darstellt

visualisierung <- function(data, ...) {
  var <- check(data, ...)  # var ist ein Vektor mit 3 oder 4 Variablennamen
  
  # Basisplot: 1. Variable auf x,die  2. Variable wird als Farbe dargestellt
  p <- ggplot(data, aes_string(x = var[1], fill = var[2])) +
    geom_bar(position = "fill") +  # Gestapeltes Balkendiagramm mit relativen Häufigkeiten
    labs(
      title = "Gestapeltes Balkendiagramm (rel. Häufigkeiten)",
      x = var[1],
      y = "Relative Häufigkeit",
      fill = var[2]
    ) +
    theme_minimal() # schlichtere Dartstellung
  
  # 3 Variablen: 3. Variable als Facets (Panels) dargestellt
  if (length(var) == 3) {
    p <- p + facet_wrap(as.formula(paste("~", var[3])))
  }
  
  # 4 Variablen: 3. und 4. Variable als Facet-Grid (Zeilen x Spalten) dargestellt
  if (length(var) == 4) {
    p <- p + facet_grid(as.formula(paste(var[3], "~", var[4])))
  }
  
  return(p)
}


