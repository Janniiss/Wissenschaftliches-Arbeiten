
#lesen des Titanic-Datensatzes
datensatz <- read.csv("E:/Uni/Semester 3/Wissarbeiten/titanic.csv")

# a)

# Anrede herausfiltern
Anrede <- sub('.*, ([A-Za-z]+)\\..*', '\\1', datensatz$Name)

# b)

# Codieren der Variablen "Survived","Sex" und "Embarked" als factor
datensatz$Survived <- factor(datensatz$Survived, levels = c(0, 1), labels = c("No", "Yes"))
datensatz$Sex <- factor(datensatz$Sex, levels = c("male", "female"))
datensatz$Embarked <- factor(datensatz$Embarked, levels = c("C", "Q", "S"),
labels = c("Cherbourg", "Queenstown", "Southampton"))

# c)
# überführen der Variable "Pclass" in einen ordered-factor
datensatz$Pclass <- factor(datensatz$Pclass, levels = c(1, 2, 3),ordered = TRUE)

# d)

# e)

# f)