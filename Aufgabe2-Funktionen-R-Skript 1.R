
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