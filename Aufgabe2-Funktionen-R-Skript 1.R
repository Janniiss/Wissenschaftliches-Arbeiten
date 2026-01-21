source("Aufgabe2-Funktionen-R-Skript 2.R")

library(ggplot2)


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


# ii. (Paul)

# berechnet deskriptive Statistiken für kategoriale Merkmale
deskriptive_kategoriell= function(data){

  #modalwert berechnen indem der Name der häufigsten Merkmalsausprägung
  #ausgegeben wird
  
  modalwert <- names(sort(table(data), decreasing = TRUE))[1]

  
  #relative Häufigkeiten berechnen indem die Anzahl der Merkmalsausprägungen
  #durch die Gesamtanzahl der Beobachtungen geteilt wird
  
  relative_häufigkeit <- table(data)/length(data)
  
  #Merkmalsausprägungen berechnen durch extrahieren der namen aus dem table
  
  Merkmalsausprägung <- names(table(data))
  
  return(list("Modalwert"=modalwert, "Relative häufigkeit"= relative_häufigkeit,
              "Merkmalsausprägungen"=Merkmalsausprägung))
  
}
#Testen dieser Funktion:
data = c("A", "B", "D", "E", "C", "A")
test = deskriptive_kategoriell(data)
if(!all(test$Modalwert == "A")){
  print("Fehler im Test")
}
if(!all(test$`Relative häufigkeit` == c(2/6, 1/6, 1/6, 1/6, 1/6))){
  print("Fehler im Test")
}
if(!all(test$Merkmalsausprägungen == c("A", "B", "C", "D", "E"))){
  print("Fehler im Test")
}


# iv.) (Jannis)

library(dplyr)
library(psych) #Punktbasierte Korrelation 

# data = Datensatz, dichotom_var = Name der dichotomen Variable, metric_var = Name der metrischen Variable
deskriptive_bivariate <- function(data, dichotom_var, metric_var){
  
  # Checks
  if (!(dichotom_var %in% names(data)) || !(metric_var %in% names(data))) {
    stop("Eine oder beide Variablen existieren nicht im Datensatz.")
  }
  
  if (length(unique(data[[dichotom_var]])) != 2) {
    stop("Die erste Variable muss dichotom sein.")
  }
  
  if (!is.numeric(data[[metric_var]])) {
    stop("Die zweite Variable muss metrisch sein.")
  }
  
  #Deskriptive Statistiken nach Gruppen
  # Übergibt den Datensatz mithilfe des Pipe-Operators (%>%) und gruppiert ihn nach der dichotomen Variable
  descriptives <- data %>%
    
  # Gruppierung nach den Ausprägungen der dichotomen Variable
  group_by(.data[[dichotom_var]]) %>%
    # Berechnung der deskriptiven Kennwerte je Gruppe
    summarise(
      n = n(),                             # Stichprobengröße pro Gruppe
      mean = mean(.data[[metric_var]]),    # Mittelwert der metrischen Variable
      SD = sd(.data[[metric_var]])         # Standardabweichung der metrischen Variable
    )
# Ausgabe der deskriptiven Statistiken
cat("\nDeskriptive Statistiken nach Gruppen:\n")
print(descriptives)

# Punktbasierte Korrelation
# Berechnung der punktbasierten Korrelation zwischen der dichotomen und der metrischen Variable
# Die dichotome Variable wird zuerst in einen Faktor und anschließend in numerische Werte (0/1) umgewandelt
point_biserial <- biserial(
  data[[metric_var]],
  as.numeric(as.factor(data[[dichotom_var]]))
)

# Ausgabe der Korrelation
cat("\nPunktbasierte Korrelation:\n")
print(point_biserial)

# Ergebnisse werden als Liste zurückgegeben
invisible(list("Deskriptive Statistiken nach Gruppen" = descriptives, "Punktbasierte Korrelation" = point_biserial))
}

# Test mit Titanic Datensatz
titanic <- read.csv("titanic_clean.csv")
deskriptive_bivariate(data = titanic, "Survived", "Age")


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