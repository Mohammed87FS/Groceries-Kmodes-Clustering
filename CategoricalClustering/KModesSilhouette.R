# Lade die benötigten Pakete
library(klaR)         # für die k-modes Funktion
library(cluster)      # für die Silhouettenanalyse und Distanzberechnung
library(factoextra)   # für die Visualisierung von Dendrogrammen

# Lade deine Daten
my_data <- read.csv('Complete.csv', stringsAsFactors = TRUE) # Lade die Daten aus der CSV-Datei

# Wähle die relevanten Spalten für k-modes aus (ausschließlich 'Größe')
km_data <- my_data[, c('Marke', 'Produktart', 'Verpackungsart', 'Material')] # Erstelle einen neuen Datenrahmen ohne die Spalte 'Größe'

# Konvertiere alle ausgewählten Spalten in Faktoren, falls sie noch keine sind
for(col in colnames(km_data)) {
  km_data[[col]] <- as.factor(km_data[[col]]) # Schleife durch alle Spalten und konvertiere sie in Faktoren für k-modes
}

# Initialisiere einen numerischen Vektor für die Silhouettenbreite
sil_width <- numeric(9) # Ein numerischer Vektor mit 9 Elementen (für k von 2 bis 33)

# Schleife über mögliche k-Werte für die Silhouettenanalyse
for(k in 2:33) {
  km <- kmodes(km_data, modes = k, iter.max = 33, weighted = FALSE) # Führe k-modes aus
  diss <- daisy(km_data) # Berechne die Distanzmatrix für die Silhouettenanalyse
  ss <- silhouette(km$cluster, diss) # Berechne Silhouetten-Scores
  sil_width[k - 1] <- mean(ss[, "sil_width"]) # Berechne die durchschnittliche Silhouettenbreite für jedes k
}

# Zeichne ein Diagramm für die Silhouettenanalyse
plot(2:33, sil_width, type='b', xlab="Anzahl der Cluster", ylab="Durchschnittliche Silhouettenbreite", main="Silhouettenanalyse für optimales K")

# Setze die optimale Anzahl von Clustern fest
optimal_k <- 33 # Festlegen der optimalen Clusteranzahl basierend auf der Analyse

# Wende k-modes Clustering mit der optimalen Anzahl von Clustern an
kmodes_model <- kmodes(km_data, modes = optimal_k, iter.max = 33, weighted = FALSE) # Erstelle das k-modes-Modell

# Füge die resultierenden Cluster-IDs zu deinem ursprünglichen Datenrahmen hinzu
my_data$group_ID <- kmodes_model$cluster # Weise jedem Artikel eine Cluster-ID zu

# Sortiere die Daten basierend auf der Cluster-ID
my_data_ordered <- my_data[order(my_data$group_ID), ] # Sortiere den Datenrahmen nach 'group_ID'

# Setze die Zeilennamen zurück, um eine durchgehende Sequenz zu erhalten
row.names(my_data_ordered) <- NULL # Entferne die alten Zeilennamen für eine saubere Darstellung

# Zeige die ersten Zeilen des geordneten Datenrahmens an
head(my_data_ordered) # Zeige die ersten Zeilen an, um die Sortierung zu überprüfen

# Schreibe den geordneten Datenrahmen in eine neue CSV-Datei
write.csv(my_data_ordered, 'Ordered_Complete.csv', row.names = FALSE) # Speichere den sortierten Datenrahmen ohne Zeilennamen in einer CSV-Datei

