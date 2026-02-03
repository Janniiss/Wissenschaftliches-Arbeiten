# Aufgabe 4

load("titanic_clean.RData")
source("Aufgabe2-Funktionen-R-Skript 1.R")
source("Aufgabe2-Funktionen-R-Skript 2.R")

# (Katharina)

# Analyse Age:
deskriptive_metrisch(datensatz$Age) # deskriptive Kennwerte zum Alter ausrechnen
# $Minimum
# [1] 0.42
# 
# $Maximum
# [1] 80
# 
# $Mittelwert
# [1] 29.37168
# 
# $Median
# [1] 30
# 
# $Varianz
# [1] 175.6399
# 
# $Standardabweichung
# [1] 13.25292
# 
# $Interquartilsabstand
# [1] 14
# 
# $Variationskoeffizient
# [1] 0.4512142

names(sort(table(datensatz$Age), decreasing = TRUE))[1] # Modalwert des Alters ausrechnen
# [1] "30"

Quantil_Funktion(datensatz$Age , 0.25) # unteres Quartil berechnen
# [1] 21

Quantil_Funktion(datensatz$Age, 0.75) # oberes Quartil berechnen
# [1] 35

visualisierung_1Var_2Var(datensatz, "Age") # Alter-Verteilung visualisieren
ggsave("Age_Histogramm.png" , 
       plot = visualisierung_1Var_2Var(datensatz, "Age"),
       width = 8, height = 5 , dpi = 300) # Histogramm zum Alter speichern

# Analyse Ticketpreis:
deskriptive_metrisch(datensatz$Fare) # deskriptive Kennwerte zum Ticketpreis ausrechnen
# $Minimum
# [1] 0
# 
# $Maximum
# [1] 512.3292
# 
# $Mittelwert
# [1] 32.20421
# 
# $Median
# [1] 14.4542
# 
# $Varianz
# [1] 2469.437
# 
# $Standardabweichung
# [1] 49.69343
# 
# $Interquartilsabstand
# [1] 23.1042
# 
# $Variationskoeffizient
# [1] 1.543073

names(sort(table(datensatz$Fare), decreasing = TRUE))[1] # Modalwert des Ticketpreises ausrechnen
[1] "8.05"

Quantil_Funktion(datensatz$Fare , 0.25) # unteres Quartil berechnen
# [1] 7.8958

Quantil_Funktion(datensatz$Fare , 0.75) # oberes Quartil berechnen
# [1] 31

visualisierung_1Var_2Var(datensatz, "Fare") # Ticketpreis-Verteilung visualisieren
ggsave("Fare_Histogramm.png" , 
       plot = visualisierung_1Var_2Var(datensatz, "Fare"),
       width = 8, height = 5 , dpi = 300) # Histogramm zum Ticketpreis speichern

# Analyse Sex:
deskriptive_kategoriell(datensatz$Sex) # deskriptive Statistiken zum Geschlecht 
# $Modalwert
# [1] "male"
# 
# $`Relative häufigkeit`
# data
# male   female 
# 0.647587 0.352413 
# 
# $Merkmalsausprägungen
# [1] "male"   "female"

visualisierung_1Var_2Var(datensatz, "Sex") # Geschlechtsverteilung visualisieren
ggsave("Sex_Balkendiagramm.png" , 
       plot = visualisierung_1Var_2Var(datensatz, "Sex"),
       width = 8, height = 5 , dpi = 300) # Balkendiagramm zum Geschlecht speichern

# Analyse Anrede:
deskriptive_kategoriell(datensatz$Anrede)
# $Modalwert
# [1] "Mr"
# 
# $`Relative häufigkeit`
# data
# Master       Miss         Mr        Mrs 
# 0.04489338 0.20763187 0.60381594 0.14365881 
# 
# $Merkmalsausprägungen
# [1] "Master" "Miss"   "Mr"     "Mrs"   

visualisierung_1Var_2Var(datensatz, "Anrede") # Anredenverteilung visualisieren
ggsave("Anrede_Balkendiagramm.png" , 
       plot = visualisierung_1Var_2Var(datensatz, "Anrede"),
       width = 8, height = 5 , dpi = 300) # Balkendiagramm zur Anrede speichern

# Analyse Survived:
deskriptive_kategoriell(datensatz$Survived) # Survived/Not survived mit deskriptiven Statistiken analysieren
# $Modalwert
# [1] "No"
# 
# $`Relative häufigkeit`
# data
# No       Yes 
# 0.6161616 0.3838384 
# 
# $Merkmalsausprägungen
# [1] "No"  "Yes"

visualisierung_1Var_2Var(datensatz, "Survived") # Survived-Verteilung visualisieren
ggsave("Survived_Balkendiagramm.png" , 
       plot = visualisierung_1Var_2Var(datensatz, "Survived"),
       width = 8, height = 5 , dpi = 300) # Balkendiagramm zu Survived speichern

# Analyse Pclass:
deskriptive_kategoriell(datensatz$Pclass) # deskriptive Statistiken zu Pclass
# $Modalwert
# [1] "3"
# 
# $`Relative häufigkeit`
# data
# 1         2         3 
# 0.2424242 0.2065095 0.5510662 
# 
# $Merkmalsausprägungen
# [1] "1" "2" "3"

visualisierung_1Var_2Var(datensatz, "Pclass") # Klassenverteilung visualisieren
ggsave("Pclass_Balkendiagramm.png" , 
       plot = visualisierung_1Var_2Var(datensatz, "Pclass"),
       width = 8, height = 5 , dpi = 300) # Balkendiagramm zu Pclass speichern



#(iii)
# Johannes
deskriptive_bivariate_kategorial(datensatz,"Sex","Survived")
# Kontingenztabelle:
#   
#         No Yes
# male   468 109
# female  81 233
#
# Relative Häufigkeiten (gesamt):
#
#             No        Yes
# male   0.52525253 0.12233446
# female 0.09090909 0.26150393
#
# Relative Häufigkeiten (zeilenweise):
#  
#            No       Yes
# male   0.8110919 0.1889081
# female 0.2579618 0.7420382
#
# Relative Häufigkeiten (spaltenweise):
#  
#            No       Yes
# male   0.8524590 0.3187135
# female 0.1475410 0.6812865
#
# Korrigiertes Kontingenzmaß von Pearson:
#  X-squared 
# 0.6728632

visualisierung_1Var_2Var(datensatz,"Sex","Survived")
ggsave("Sex_Survived.png",
       plot = visualisierung_1Var_2Var(datensatz,"Sex","Survived"),
       width = 8, height = 5, dpi = 300)

deskriptive_bivariate_kategorial(datensatz,"Pclass","Survived")

# Kontingenztabelle:
#  
#   No Yes
# 1  80 136
# 2  97  87
# 3 372 119
#
# Relative Häufigkeiten (gesamt):
#  
#   No        Yes
# 1 0.08978676 0.15263749
# 2 0.10886644 0.09764310
# 3 0.41750842 0.13355780
#
# Relative Häufigkeiten (zeilenweise):
#   
#   No       Yes
# 1 0.3703704 0.6296296
# 2 0.5271739 0.4728261
# 3 0.7576375 0.2423625
#
# Relative Häufigkeiten (spaltenweise):
#  
#   No       Yes
# 1 0.1457195 0.3976608
# 2 0.1766849 0.2543860
# 3 0.6775956 0.3479532
#
# Korrigiertes Kontingenzmaß von Pearson:
#   X-squared 
# 0.45502 


visualisierung_1Var_2Var(datensatz,"Pclass","Survived")
ggsave("Survived_Pclass.png",
       plot = visualisierung_1Var_2Var(datensatz,"Pclass","Survived"),
       width = 8, height = 5, dpi = 300)



# Jannis
# ============================
# (iv) deskriptive_bivariate_metrisch_dichotom()
# ============================
deskriptive_bivariate_metrisch_dichotom(datensatz, "Survived", "Age")

# Deskriptive Statistiken nach Gruppen:
#   # A tibble: 2 × 4
#   Survived     n  mean    SD
# <fct>    <int> <dbl> <dbl>
#   1 No         549  30.2  12.7
# 2 Yes        342  28.1  14.1
# 
# 
# Punktbasierte Korrelation:
#   [,1]
# [1,] -0.09899649

visualisierung_1Var_2Var(datensatz, "Survived", "Age")
ggsave("Survived_Age_box.png",
       plot = visualisierung_1Var_2Var(datensatz, "Survived", "Age"),
       width = 8, height = 5, dpi = 300)

#----------------------------------------------------------------------
deskriptive_bivariate_metrisch_dichotom(datensatz, "Survived", "Fare")

# Deskriptive Statistiken nach Gruppen:
#   # A tibble: 2 × 4
#   Survived     n  mean    SD
# <fct>    <int> <dbl> <dbl>
#   1 No         549  22.1  31.4
# 2 Yes        342  48.4  66.6
# 
# 
# Punktbasierte Korrelation:
#   [,1]
# [1,] 0.327468                              

visualisierung_1Var_2Var(datensatz, "Survived", "Fare")
ggsave("Survived_Fare_box.png",
       plot = visualisierung_1Var_2Var(datensatz, "Survived", "Fare"),
       width = 8, height = 5, dpi = 300)


# ============================
# (vi) visualisierung()
# ============================
visualisierung(datensatz, "Survived", "Sex", "Pclass")
ggsave("Survived_Sex_Pclass.png",
       plot = visualisierung(datensatz, "Survived", "Sex", "Pclass"),
       width = 10, height = 6, dpi = 300)







