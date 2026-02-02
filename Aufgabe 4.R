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
