
#lesen des Titanic-Datensatzes bei Paul:
# datensatz <- read.csv("E:/Uni/Semester 3/Wissarbeiten/titanic.csv")

#lesen des Titanic-Datensatzes bei Henning:
datensatz <- read.csv("titanic.csv")

# a) (Paul)

# Anrede herausfiltern
# Hier wird dien Anrede herausgefiltert,
#indem der Text zwischen dem Komma und dem Punkt extrahiert wird

Anrede <- sub('.*, ([A-Za-z ]+)\\..*', '\\1', datensatz$Name)

# Nach einem Vorschlag von Janiss werden die Anreden nun alle zu den Variablen
# Mr, Mrs, Miss und Master zusammengefasst:
# Also werden mademoiselle und ms  zu Miss
Anrede[Anrede %in% c("Mlle", "Ms")] <- "Miss"
# Mme,Lady, the countess und Dona werden zu Mrs 
Anrede[Anrede %in% c( "Mme", "Lady", "the Countess", "Dona")] <- "Mrs"
# Capt, Col, Major, Sir, Rev, Don, "Jonkheer" und Dr werden zu Mr
Anrede[Anrede %in% c("Capt", "Col", "Major", "Dr", "Sir", "Rev", "Don", "Jonkheer" )] <- "Mr"
unique(Anrede)
# b) (Paul)

# Codieren der Variablen "Survived","Sex" und "Embarked" als factor

# Bei Survived werden 0 und 1 in No und Yes umgewandelt
datensatz$Survived <- factor(datensatz$Survived, levels = c(0, 1), labels = c("No", "Yes"))

# Bei Sex werden die Einträge nicht geändert, aber in einen Factor übertragen
datensatz$Sex <- factor(datensatz$Sex, levels = c("male", "female"))

# Bei Embarked werden die Abkürzungen nun ausgeschrieben
datensatz$Embarked <- factor(datensatz$Embarked, levels = c("C", "Q", "S"),
labels = c("Cherbourg", "Queenstown", "Southampton"))

# c) (Paul)

# überführen der Variable "Pclass" in einen ordered-factor
# Also gilt dann 1>2>3
datensatz$Pclass <- factor(datensatz$Pclass, levels = c(1, 2, 3),ordered = TRUE)

# d) (Henning)

# Imputieren der fehlenden Werte in der Variable „Age“ mithilfe der erzeugten
# Variable „Anrede“

# Datensatz um die Variable "Anrede" erweitern
datensatz$Anrede <- Anrede

# Median-Alter pro Anrede mittels tapply berechnen 
median_alter <- tapply(datensatz$Age, datensatz$Anrede, median, na.rm = TRUE)

# Fehlende Werte in die Variable "Age" imputieren
for (i in which(is.na(datensatz$Age))) {
  datensatz$Age[i] <- median_alter[datensatz$Anrede[i]]
}

# e) (Henning)

# Neue Variablen „Deck“ und „Seite“ mithilfe der Variable "Cabin" erstellen


# Einträge mit unbekannter Kabinennummer werden auf NA gesetzt.
# Die passiert zuerst, damit es bei Deck und Seite zu keinen Fehlern kommt
datensatz$Cabin[datensatz$Cabin == ""] <- NA

# Extrahiert mittsels substr das erste Zeichen der Kabinenbezeichnung (das Deck)
# und wandelt es in eine neue Variable „Deck“ um
datensatz$Deck <- substr(datensatz$Cabin, 1, 1)

# Kabinennummer mittels gsub extrahieren, welches alle Buchstaben aus Cabin, 
# für einfacheren zugriff auf die Kabinennummer, entfernt
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

save(datensatz , file = "titanic_clean.RData")

