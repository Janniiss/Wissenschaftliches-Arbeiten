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


# Johannes
deskriptive_bivariate_kategorial(datensatz,"Sex","Survived")
# Kontingenztabelle:
#   
#   No Yes
# male   468 109
# female  81 233
#
# Relative Häufigkeiten (gesamt):
#
#  No        Yes
# male   0.52525253 0.12233446
# female 0.09090909 0.26150393
#
# Relative Häufigkeiten (zeilenweise):
#  
#  No       Yes
# male   0.8110919 0.1889081
# female 0.2579618 0.7420382
#
# Relative Häufigkeiten (spaltenweise):
#  
#  No       Yes
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

deskriptive_bivariate_kategorial(datensatz,"Survived","Deck")
# Kontingenztabelle:
#  
#  A  B  C  D  E  F  G  T
# No   8 12 24  8  8  5  2  1
# Yes  7 35 35 25 24  8  2  0
#
# Relative Häufigkeiten (gesamt):
#   
#   A           B           C           D           E           F           G           T
# No  0.039215686 0.058823529 0.117647059 0.039215686 0.039215686 0.024509804 0.009803922 0.004901961
# Yes 0.034313725 0.171568627 0.171568627 0.122549020 0.117647059 0.039215686 0.009803922 0.000000000
# 
# Relative Häufigkeiten (zeilenweise):
#
#  A          B          C          D          E          F          G          T
# No  0.11764706 0.17647059 0.35294118 0.11764706 0.11764706 0.07352941 0.02941176 0.01470588
# Yes 0.05147059 0.25735294 0.25735294 0.18382353 0.17647059 0.05882353 0.01470588 0.00000000
#
# Relative Häufigkeiten (spaltenweise):
#
#  A         B         C         D         E         F         G         T
# No  0.5333333 0.2553191 0.4067797 0.2424242 0.2500000 0.3846154 0.5000000 1.0000000
# Yes 0.4666667 0.7446809 0.5932203 0.7575758 0.7500000 0.6153846 0.5000000 0.0000000
# 
# Korrigiertes Kontingenzmaß von Pearson:
#  X-squared 
# 0.3100514 
#
# Warning message:
#  In chisq.test(tbl) : Chi-squared approximation may be incorrect



visualisierung_1Var_2Var(titanic,"Survived","Deck")
ggsave("Survived_Deck.png",
       plot = visualisierung_1Var_2Var(datensatz,"Survived","Deck"),
       width = 8, height = 5, dpi = 300)












