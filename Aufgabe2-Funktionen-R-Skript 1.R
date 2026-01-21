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
