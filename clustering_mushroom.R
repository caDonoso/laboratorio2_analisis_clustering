library("ggplot2")

#LECTURA DE DATOS
columnas <- c("class","cap-shape","cap-surface","cap-color","bruises","odor","gill-attachment","gill-spacing","gill-size","gill-color"
              ,"stalk-shape","stalk-root","stalk-surface-above-ring","stalk-surface-below-ring","stalk-color-above-ring"
              ,"stalk-color-below-ring","veil-type","veil-color","ring-number","ring-type","spore-print-color","population","habitat")
datos <- read.csv("D:/Universidad/Analisis de datos/LAB 2/agaricus-lepiota.data", header=FALSE, 
                  sep=",", col.names = columnas)

datosWithoutUnknown <- datos[datos$stalk.root != "?",]

#Porcentaje de datos inicial:
#   - edible: 51.8% (4208)
#   - poisonous: 48.2% (3916)

#LIMPIEZA DE DATOS
#Se elimina el atributo veil-type ya que es una elemento de tipo constante por lo que no aporta al estudio.
filtered.datos <- subset(datos, select = -c(class, veil.type))

#Se eliminan las tuplas o registros que en el atributo "stalk-root" no tienen ningun valor registrado.
#Se filtran 2480 datos, es decir, nos quedamos con el 69,5% de los datos.
#El porcentaje de datos por cada variable de clase, despues de filtrado:
#   - edible: 61,8% (3488)
#   - poisonous: 38.2% (2156)

filtered.datos <- filtered.datos[filtered.datos$stalk.root != "?",]

#Se procede a realizar la codificacion One-Hot para tener variables discretas representadas por valores binarios.
filtered.datos.OneHot <- model.matrix(~.-1, data=filtered.datos)

View(filtered.datos.OneHot)

#Ahora se procede a hacer uso del algoritmo de clustering k-means
#El segundo parametro de la función kmeans indica la cantidad de clusters, para efectos de este estudio
# se coloca 2 ya que la variable clasificadora solo divide en grupos de setas comestibles y venenosas.
set.seed(20)
clustering.kmeans = kmeans(filtered.datos.OneHot, 2, nstart = 50, iter.max = 15) 
View(clustering.kmeans$cluster)
clustering.kmeans.compare <- table(datosWithoutUnknown$class, clustering.kmeans$cluster)
View(clustering.kmeans.compare)
purity.kmean <- sum(apply(clustering.kmeans.compare, 2, max)) / nrow(filtered.datos)
purity.kmean
