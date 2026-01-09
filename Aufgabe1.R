
#lesen des Titanic-Datensatzes
datensatz <- read.csv("E:/Uni/Semester 3/Wissarbeiten/titanic.csv")

# a)

#ohne packages(vielleicht leichter mit)
titel <- sub('.*, ([A-Za-z]+)\\..*', '\\1', datensatz$Name)

# b)


# c)