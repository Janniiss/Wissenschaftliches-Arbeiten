
#lesen des Titanic-Datensatzes bei Paul:
# datensatz <- read.csv("E:/Uni/Semester 3/Wissarbeiten/titanic.csv")

#lesen des Titanic-Datensatzes bei Henning:
datensatz <- read.csv("titanic.csv")

# a) (Paul)

# Anrede herausfiltern
Anrede <- sub('.*, ([A-Za-z]+)\\..*', '\\1', datensatz$Name)

# b) (Paul)

# Codieren der Variablen "Survived","Sex" und "Embarked" als factor
datensatz$Survived <- factor(datensatz$Survived, levels = c(0, 1), labels = c("No", "Yes"))
datensatz$Sex <- factor(datensatz$Sex, levels = c("male", "female"))
datensatz$Embarked <- factor(datensatz$Embarked, levels = c("C", "Q", "S"),
labels = c("Cherbourg", "Queenstown", "Southampton"))

# c) (Paul)
# überführen der Variable "Pclass" in einen ordered-factor
datensatz$Pclass <- factor(datensatz$Pclass, levels = c(1, 2, 3),ordered = TRUE)

# d) (Henning)

# Imputieren der fehlenden Werte in der Variable „Age“ mithilfe der erzeugten
# Variable „Anrede“

# Datensatz um Anrede erweitern
datensatz$Anrede <- Anrede

# Median-Alter pro Anrede berechnen 
median_alter <- tapply(datensatz$Age, datensatz$Anrede, median, na.rm = TRUE)

# Fehlende Werte imputieren
for (i in which(is.na(datensatz$Age))) {
  datensatz$Age[i] <- median_alter[datensatz$Anrede[i]]
}

# e) (Henning)

# Neue Variablen „Deck“ und „Seite“ mithilfe der Variable "Cabin" erstellen


# Einträge mit unbekannter Kabinennummer werden auf NA gesetzt
# zuerst, damit es bei Deck und Seite zu keinen Fehlern kommt
datensatz$Cabin[datensatz$Cabin == ""] <- NA

# Extrahiert mittsels substr das erste Zeichen der Kabinenbezeichnung (das Deck)
# und wandelt es in eine neue Variable „Deck“ um
datensatz$Deck <- substr(datensatz$Cabin, 1, 1)

# Kabinennummer extrahieren, für einfacheren zugriff auf dei Kabinennummer
Kabinennummer <- as.numeric(gsub("[A-Za-z]", "", datensatz$Cabin))

# Seite bestimmen: Kabinen mit einer ungeraden Nummer liegen
# auf Steuerbord, die anderen auf Backbord
# Wenn die Kabinennummer Modulo 2 gleich 1 ist, ist sie ungerade und die Seite
# wird auf "Steuerbord" gesetzt, sonst auf "Backbord"
datensatz$Seite <- ifelse(
  is.na(Kabinennummer),
  NA,
  ifelse(Kabinennummer %% 2 == 1, "Steuerbord", "Backbord")
)


# f) (Henning)

# Entfernen der Variablen „PassengerID“,„Name“,„Ticket“ und „Cabin“ 
# aus dem Datensatz

# Die Variablen werden auf NULL gesetzt, um sie zu entfernen
datensatz$PassengerId <- NULL
datensatz$Name <- NULL
datensatz$Ticket <- NULL
datensatz$Cabin <- NULL


# g) (Henning)


setwd("/Users/henninghans/Documents/GitHub/Wissenschaftliches-Arbeiten")

# Speichern des bereinigten Datensatzes als CSV-Datei
write.csv(datensatz, "titanic_clean.csv", row.names = FALSE)
# Der bereinigte Datensatz wird als "titanic_clean.csv" gespeichert



