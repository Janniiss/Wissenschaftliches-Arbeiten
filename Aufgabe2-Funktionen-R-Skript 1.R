source("Aufgabe2-Funktionen-R-Skript 2.R")
library(ggplot2)


# Aufgabe 2a

# i) (Katharina)
# Funktion, die verschiedene geeignete deskriptive Statistiken für metrische Variablen
# berechnet und ausgibt

deskriptive_metrisch <- function(data) {
  if(!is.numeric(data)) {
    stop(paste("Die Eingabe muss numerisch sein."))
  } # Checken, ob Variable metrisch ist 
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
df <- c("b" , "f" , "f")

# ii. (Paul)

# berechnet deskriptive Statistiken für kategoriale Merkmale
deskriptive_kategoriell <- function(data){
  
  # checken ob die Eingabe kategoriell ist
if (!is.character(data) && !is.factor(data)) {
  stop(paste("Die Eingabe muss kategorisch sein."))
   }
  
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
data <- c("A", "B", "D", "E", "C", "A")
test <- deskriptive_kategoriell(data)
if(!all(test$Modalwert == "A")){
  print("Fehler im Test")
}
if(!all(test$`Relative häufigkeit` == c(2/6, 1/6, 1/6, 1/6, 1/6))){
  print("Fehler im Test")
}
if(!all(test$Merkmalsausprägungen == c("A", "B", "C", "D", "E"))){
  print("Fehler im Test")
}


# iii) (Johannes)

library(dplyr)
library(vcd)  # Für Assoziationsmaße wie Phi-Koeffizient

# Funktion: Deskriptive bivariate Statistik für den Zusammenhang zwischen zwei kategoriale Variablen
deskriptive_bivariate_kategorial <- function(data, var1, var2) {
  
  # Prüfen, ob eine der Variablen numerisch ist
  # Falls ja: Abbruch, da die Funktion nur für kategoriale Variablen gedacht ist
  if (is.numeric(data[[var1]]) || is.numeric(data[[var2]])) {
    stop("Beide Variablen müssen kategorial sein.")
  }
  
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


# iv.) (Jannis)

library(dplyr)
library(psych) #Punktbasierte Korrelation 

# data = Datensatz, dichotom_var = Name der dichotomen Variable, metric_var = Name der metrischen Variable
deskriptive_bivariate_metrisch_dichotom <- function(data, var1, var2) {
  
  if (!(var1 %in% names(data)) || !(var2 %in% names(data))) {
    stop("Eine oder beide Variablen existieren nicht im Datensatz.")
  }
  
  # Typen erkennen
  if (length(unique(na.omit(data[[var1]]))) == 2 && is.numeric(data[[var2]])) {
    dichotom_var <- var1
    metric_var <- var2
  } else if (length(unique(na.omit(data[[var2]]))) == 2 && is.numeric(data[[var1]])) {
    dichotom_var <- var2
    metric_var <- var1
  } else {
    stop("Es muss eine dichotome und eine metrische Variable übergeben werden.")
  }
  
  # Deskriptive Statistiken
  descriptives <- data %>%
    group_by(.data[[dichotom_var]]) %>%
    summarise(
      n = n(),
      mean = mean(.data[[metric_var]], na.rm = TRUE),
      SD = sd(.data[[metric_var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\nDeskriptive Statistiken nach Gruppen:\n")
  print(descriptives)
  
  # Punktbiseriale Korrelation
  point_biserial <- biserial(
    data[[metric_var]],
    as.numeric(as.factor(data[[dichotom_var]]))
  )
  
  cat("\nPunktbasierte Korrelation:\n")
  print(point_biserial)
  
  invisible(list(
    Deskriptive_Statistiken = descriptives,
    Punktbiseriale_Korrelation = point_biserial
  ))
}


# Test mit Titanic Datensatz
titanic <- read.csv("titanic_clean.csv")
deskriptive_bivariate_metrisch_dichotom(data = titanic, "Age", "Survived")


# 2.v) (Henning)
# Gestapeltes Balkendiagramm für 3 oder 4 kategoriale Variablen
# Eingabe:
# - data: Datensatz
# - ... : 3 oder 4 Variablennamen als Strings (z.B. "Sex", "Pclass", "Survived")
# Ausgabe:
# - ggplot-Objekt (Plot) welcher die Variablen 1 und 2 vergleicht und die Variablen 3 und 4 als Facets darstellt

visualisierung <- function(data, ..., ignore_na = TRUE) {
  var <- check(data, ...)  # var ist ein Vektor mit 3 oder 4 Variablennamen
  
  
  # NA´s entfernen
  if (ignore_na) {
    data <- data[stats::complete.cases(data[, var]), ]
  }
  
  
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


# vi.) (Jannis)

visualisierung_1Var_2Var <- function(data, ...) {
  vars <- c(...)
  
  check_vars(data,vars)
  # -----------------------
  # 1 Variable
  # -----------------------
  if (length(vars) == 1) {
    v <- vars[1]
    data2 <- data[!is.na(data[[v]]), ]
    
    if (!is_cat(data2[[v]])) {
      # metrisch
      p <- ggplot(data2, aes(x = .data[[v]])) +
        geom_histogram(bins = 30, fill = "steelblue", color = "white") +
        labs(
          title = paste("Histogramm von", v),
          x = v,
          y = "Anzahl"
        ) +
        theme_minimal()
      
      return(p)
      
    } else {
      # kategorial
      p <- ggplot(data2, aes(x = .data[[v]])) +
        geom_bar(
          aes(y = after_stat(count / sum(count))),
          fill = "steelblue"
        ) +
        scale_y_continuous(labels = scales::percent) +
        labs(
          title = paste("Relative Häufigkeit von", v),
          x = v,
          y = "Relative Häufigkeit"
        ) +
        theme_minimal()
      
      return(p)
    }
  }
  
  # -----------------------
  # 2 Variablen
  # -----------------------
  if (length(vars) == 2) {
    v1 <- vars[1]
    v2 <- vars[2]
    
    data2 <- data[complete.cases(data[, vars]), ]
    
    cat1 <- is_cat(data2[[v1]])
    cat2 <- is_cat(data2[[v2]])
    
    # kategorial + metrisch
    if (cat1 != cat2) {
      cat_var <- if (cat1) v1 else v2
      met_var <- if (cat1) v2 else v1
      
      p <- ggplot(data2, aes(x = .data[[cat_var]], y = .data[[met_var]])) +
        geom_boxplot(fill = "steelblue") +
        labs(
          title = paste("Boxplot:", met_var, "nach", cat_var),
          x = cat_var,
          y = met_var
        ) +
        theme_minimal()
      
      return(p)
    }
    
    # 2 kategorial
    if (cat1 && cat2) {
      p <- ggplot(data2, aes(x = .data[[v1]], fill = .data[[v2]])) +
        geom_bar(position = "stack") +
        labs(
          title = paste("Beziehung:", v1, "und", v2),
          x = v1,
          y = "Anzahl",
          fill = v2
        ) +
        theme_minimal()
      
      return(p)
    }
    
    stop("Kombination nicht unterstützt.")
  }
  
  stop("Bitte 1 oder 2 Variablen übergeben.")
}


visualisierung_1Var_2Var(titanic_clean,"Age")
visualisierung_1Var_2Var(titanic_clean, "Sex")
visualisierung_1Var_2Var(titanic_clean, "Sex", "Survived")
visualisierung_1Var_2Var(titanic_clean, "Embarked", "Survived")
