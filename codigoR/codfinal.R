library(readxl)
#------------------------------

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#------------------------------------------------------

DataSet1 <- read_excel("C:/BD_Scripts/cereal/cerealdb.xlsx",col_names = TRUE)

summary(DataSet1)

# Tabla de frecuencia para Var Cuantitativa mfr

datos_1 = c(DataSet1$calories)


ni = table(datos_1) # Frecuencia absoluta
fi = table(datos_1)/length(datos_1) # Frecuencia relativa
Ni = cumsum(ni) # Frecuencia absoluta acumulada
Fi = cumsum(fi) # Frecuencia relativa acumulada
Tabla_Frec = cbind(ni,fi,Ni,Fi) # Se crea una tabla con todas las frecuencias
Tabla_Frec # Se visualiza la tabla

# Gráficas
#--------------------------------------------------------------------

# histograma para var mfr
library(MASS)            
mfr = DataSet1$mfr      
mfr.freq = table(mfr)   

barplot(mfr.freq, col = "red") 


# gráfico de pastel para var type
colors =c("violet", "cyan")
type = DataSet1$type 
type.freq = table(type)   

pie(type.freq, col=colors)


# histograma para var calories
hist(DataSet1$calories,
     main="Distribución de variable calories",
     xlab="Calorías por porción ",
     col="darkmagenta",
     freq=TRUE
)


# histograma para var protein
hist(DataSet1$protein,
     main="Distribución de variable protein ",
     xlab="Gramos de proteína ",
     col="navyblue",
     freq=TRUE
)

# histograma para var fat
hist(DataSet1$fat,
     main="Distribución de variable fat",
     xlab="Gramos de grasa",
     col="lightseagreen",
     freq=TRUE
)

# histograma para var sodium
hist(DataSet1$sodium,
     main="Distribución de variable sodium",
     xlab="Miligramos de sodio",
     col="mediumvioletred",
     freq=TRUE
)


# histograma para var fiber
fiber <- c(DataSet1$fiber)
fiber <- as.numeric(fiber)
hist(fiber,
     main="Distribución de variable fiber ",
     xlab="Gramos de fibra dietaria",
     col="pink",
     freq=TRUE
)


# grafico de barras para var carbo
carbo <- c(DataSet1$carbo)
carbo <- as.numeric(carbo)
hist(carbo,
     main="Distribución de variable carbo ",
     xlab="Gramos de carbohidratos complejos",
     col="green",
     freq=TRUE
)

#histograma para var sugars
sugar<- c(DataSet1$sugars)
sugar <- as.numeric(sugars)
hist(sugar,
     main="Distribuci?n de variable Sugars",
     xlab="Gramos de azucar",
     col="coral",
     freq=TRUE
)

#histograma para var potass
potass<- c(DataSet1$potass)
potass <- as.numeric(potass)
hist(potass,
     main="Distribuci?n de variable Potass",
     xlab="miligramos de potasio",
     col="darkolivegreen1",
     freq=TRUE
)

#histograma para var vitamins
vitamins<- c(DataSet1$vitamins)
vitamins<- as.numeric(vitamins)
hist(vitamins,
     main="Distribuci?n de variable Vitamins",
     xlab="porcentaje de vitaminas ",
     col="lightpink",
     freq=TRUE
)

#diagrama de torta para var shelf
colors = palette("pastel2")
shelf = DataSet1$shelf 
shelf.freq = table(shelf)   

pie(shelf.freq, col=colors)

#histograma para var weight
weight<- c(DataSet1$weight)
weight<- as.numeric(weight)
hist(weight,
     main="Distribuci?n de variable weight",
     xlab="Peso en onzas por porcion. ",
     col="mediumorchid2",
     freq=TRUE
)

#histograma para var cups
cups<- c(DataSet1$cups)
cups<- as.numeric(cups)
hist(cups,
     main="Distribuci?n de variable cups",
     xlab="Numero de tazas en una porcion.",
     col="royalblue2",
     freq=TRUE
)


#histograma para var rating
rating<- c(DataSet1$rating)
rating<- as.numeric(rating)
hist(rating,
     main="Distribucion de variable ratings",
     xlab="Calificacion de los cereales",
     col="seagreen2",
     freq=TRUE
)

#----------------------------------------------------------------------------

#características 

# análisis para var calories

media_calories = mean(DataSet1$calories)

mediana_calories = median(DataSet1$calories)

de_calories = sd(DataSet1$calories)

moda_calories <- getmode(DataSet1$calories)

cuantiles_calories = quantile(DataSet1$calories,seq(0.25,0.75,by=0.25))

max_calories = max(DataSet1$calories)

min_calories = min(DataSet1$calories)

range_calories = range(DataSet1$calories)

print(media_calories)
print(mediana_calories)
print(de_calories)
print(moda_calories)
print(cuantiles_calories)
print(max_calories)
print(min_calories)
print(range_calories)  


# análisis para var protein

media_protein = mean(DataSet1$protein)

mediana_protein = median(DataSet1$protein)

de_profein = sd(DataSet1$protein)

moda_protein <- getmode(DataSet1$protein)

cuantiles_protein = quantile(DataSet1$protein,seq(0.25,0.75,by=0.25))

max_protein = max(DataSet1$protein)

min_protein = min(DataSet1$protein)

range_protein = range(DataSet1$protein)

print(media_protein)
print(mediana_protein)
print(de_profein)
print(moda_protein)
print(cuantiles_protein)
print(max_protein)
print(min_protein)
print(range_protein)  



# análisis para var fat

media_fat = mean(DataSet1$fat)

mediana_fat = median(DataSet1$fat)

de_fat = sd(DataSet1$fat)

moda_fat <- getmode(DataSet1$fat)

cuantiles_fat = quantile(DataSet1$fat,seq(0.25,0.75,by=0.25))

max_fat = max(DataSet1$fat)

min_fat = min(DataSet1$fat)

range_fat = range(DataSet1$fat)


print(media_fat)
print(mediana_fat)
print(de_fat)
print(moda_fat)
print(cuantiles_fat)
print(max_fat)
print(min_fat)
print(range_fat) 




# análisis para var sodium

media_sodium = mean(DataSet1$sodium)

mediana_sodium = median(DataSet1$sodium)

de_sodium = sd(DataSet1$sodium)

moda_sodium <- getmode(DataSet1$sodium)

cuantiles_sodium = quantile(DataSet1$sodium,seq(0.25,0.75,by=0.25))

max_sodium = max(DataSet1$sodium)

min_sodium = min(DataSet1$sodium)

range_sodium = range(DataSet1$sodium)



print(media_sodium)
print(mediana_sodium)
print(de_sodium)
print(moda_sodium)
print(cuantiles_sodium)
print(max_sodium)
print(min_sodium)
print(range_sodium) 



# análisis para var fiber

media_fiber = mean(as.numeric(DataSet1$fiber),na.rm = TRUE)

mediana_fiber = median(as.numeric(DataSet1$fiber),na.rm = TRUE)

de_fiber = sd(as.numeric(DataSet1$fiber),na.rm = TRUE)

moda_fiber <- getmode(as.numeric(DataSet1$fiber),na.rm = TRUE)

cuantiles_fiber = quantile(as.numeric(DataSet1$fiber),seq(0.25,0.75,by=0.25),na.rm = TRUE)

max_fiber = max(as.numeric(DataSet1$fiber),na.rm = TRUE)

min_fiber = min(as.numeric(DataSet1$fiber),na.rm = TRUE)

range_fiber = range(as.numeric(DataSet1$fiber),na.rm = TRUE)

print(media_fiber)
print(mediana_fiber)
print(de_fiber)
print(moda_fiber)
print(cuantiles_fiber)
print(max_fiber)
print(min_fiber)
print(range_fiber)

# análisis para var carbo

media_carbo = mean(as.numeric(DataSet1$carbo),na.rm = TRUE)

mediana_carbo = median(as.numeric(DataSet1$carbo),na.rm = TRUE)

de_carbo = sd(as.numeric(DataSet1$carbo),na.rm = TRUE)

moda_carbo <- getmode(as.numeric(DataSet1$carbo),na.rm = TRUE)

cuantiles_carbo = quantile(as.numeric(DataSet1$carbo),seq(0.25,0.75,by=0.25),na.rm = TRUE)

max_carbo = max(as.numeric(DataSet1$carbo),na.rm = TRUE)

min_carbo = min(as.numeric(DataSet1$carbo),na.rm = TRUE)

range_carbo = range(as.numeric(DataSet1$carbo),na.rm = TRUE)

print(media_carbo)
print(mediana_carbo)
print(de_carbo)
print(moda_carbo)
print(cuantiles_carbo)
print(max_carbo)
print(min_carbo)
print(range_carbo)

# análisis para var sugars

media_sugars = mean(DataSet1$sugars)

mediana_sugars = median(DataSet1$sugars)

de_sugars = sd(DataSet1$sugars)

moda_sugars <- getmode(DataSet1$sugars)

cuantiles_sugars = quantile(DataSet1$sugars,seq(0.25,0.75,by=0.25))

max_sugars = max(DataSet1$sugars)

min_sugars = min(DataSet1$sugars)

range_sugars = range(DataSet1$sugars)

print(media_sugars)
print(mediana_sugars)
print(de_sugars)
print(moda_sugars)
print(cuantiles_sugars)
print(max_sugars)
print(min_sugars)
print(range_sugars)

# análisis para var potass

media_potass = mean(DataSet1$potass)

mediana_potass = median(DataSet1$potass)

de_potass = sd(DataSet1$potass)

moda_potass <- getmode(DataSet1$potass)

cuantiles_potass = quantile(DataSet1$potass,seq(0.25,0.75,by=0.25))

max_potass = max(DataSet1$potass)

min_potass = min(DataSet1$potass)

range_potass = range(DataSet1$potass)

print(media_potass)
print(mediana_potass)
print(de_potass)
print(moda_potass)
print(cuantiles_potass)
print(max_potass)
print(min_potass)
print(range_potass)

# análisis para var vitamins

media_vitamins = mean(DataSet1$vitamins)

mediana_vitamins = median(DataSet1$vitamins)

de_vitamins = sd(DataSet1$vitamins)

moda_vitamins <- getmode(DataSet1$vitamins)

cuantiles_vitamins = quantile(DataSet1$vitamins,seq(0.25,0.75,by=0.25))

max_vitamins = max(DataSet1$vitamins)

min_vitamins = min(DataSet1$vitamins)

range_vitamins = range(DataSet1$vitamins)

print(media_vitamins)
print(mediana_vitamins)
print(de_vitamins)
print(moda_vitamins)
print(cuantiles_vitamins)
print(max_vitamins)
print(min_vitamins)
print(range_vitamins)

# análisis para var shelf

media_shelf = mean(DataSet1$shelf)

mediana_shelf = median(DataSet1$shelf)

de_shelf = sd(DataSet1$shelf)

moda_shelf <- getmode(DataSet1$shelf)

cuantiles_shelf = quantile(DataSet1$shelf,seq(0.25,0.75,by=0.25))

max_shelf = max(DataSet1$shelf)

min_shelf = min(DataSet1$shelf)

range_shelf = range(DataSet1$shelf)

print(media_shelf)
print(mediana_shelf)
print(de_shelf)
print(moda_shelf)
print(cuantiles_shelf)
print(max_shelf)
print(min_shelf)
print(range_shelf)


# análisis para var weight

media_weight = mean(as.numeric(DataSet1$weight),na.rm = TRUE)

mediana_weight = median(as.numeric(DataSet1$weight),na.rm = TRUE)

de_weight = sd(as.numeric(DataSet1$weight),na.rm = TRUE)

moda_weight <- getmode(as.numeric(DataSet1$weight),na.rm = TRUE)

cuantiles_weight = quantile(as.numeric(DataSet1$weight),seq(0.25,0.75,by=0.25),na.rm = TRUE)

max_weight = max(as.numeric(DataSet1$weight),na.rm = TRUE)

min_weight = min(as.numeric(DataSet1$weight),na.rm = TRUE)

range_weight = range(as.numeric(DataSet1$weight),na.rm = TRUE)

print(media_weight)
print(mediana_weight)
print(de_weight)
print(moda_weight)
print(cuantiles_weight)
print(max_weight)
print(min_weight)
print(range_weight)

# análisis para var cups

media_cups = mean(as.numeric(DataSet1$cups),na.rm = TRUE)

mediana_cups = median(as.numeric(DataSet1$cups),na.rm = TRUE)

de_cups = sd(as.numeric(DataSet1$cups),na.rm = TRUE)

moda_cups <- getmode(as.numeric(DataSet1$cups),na.rm = TRUE)

cuantiles_cups = quantile(as.numeric(DataSet1$cups),seq(0.25,0.75,by=0.25),na.rm = TRUE)

max_cups = max(as.numeric(DataSet1$cups),na.rm = TRUE)

min_cups = min(as.numeric(DataSet1$cups),na.rm = TRUE)

range_cups = range(as.numeric(DataSet1$cups),na.rm = TRUE)

print(media_cups)
print(mediana_cups)
print(de_cups)
print(moda_cups)
print(cuantiles_cups)
print(max_cups)
print(min_cups)
print(range_cups)

# análisis para var rating

media_rating = mean(DataSet1$rating)

mediana_rating = median(DataSet1$rating)

de_rating = sd(DataSet1$rating)

moda_rating <- getmode(DataSet1$rating)

cuantiles_rating = quantile(DataSet1$rating,seq(0.25,0.75,by=0.25))

max_rating = max(DataSet1$rating)

min_rating = min(DataSet1$rating)

range_rating = range(DataSet1$rating)

print(media_rating)
print(mediana_rating)
print(de_rating)
print(moda_rating)
print(cuantiles_rating)
print(max_rating)
print(min_rating)
print(range_rating)

# Datos para PH

mean(DataSet1$sugars[DataSet1$type =="C"])
mean(DataSet1$sugars[DataSet1$type =="H"])

K <- c(DataSet1$sugars[DataSet1$mfr =="K"])
print(length(K))

G <- c(DataSet1$sugars[DataSet1$mfr =="G"])
print(length(G))


media_K = mean(K)
de_K = sd(K)


media_G = mean(G)
de_G = sd(G)



