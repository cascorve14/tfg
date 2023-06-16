library(readxl)           # Cargamos readxl para leer archivos de Excel
library(dplyr)            # Cargamos dplyr para manipulaci�n de datos
library(ggplot2)          # Cargamos ggplot2 para visualizacion de datos
library(GGally)           # Cargamos GGally para visualizacion de datos
library(Hmisc)            # Cargamos Hmisc para operaciones estadisticas
library(corrplot)         # Cargamos corrplot para visualizacion de matrices de correlacion
library(PerformanceAnalytics)  # Cargamos PerformanceAnalytics para analisis de rendimiento
library(sf)               # Cargamos sf para manipulacion de datos espaciales
library(jsonlite)         # Cargamos jsonlite para trabajar con archivos JSON
library(caret)            # Cargamos caret para modelado y evaluacion de datos

datos_TV <- read_excel("/cloud/project/Base_Datos_Definitiva.xlsx")  # Leemos los datos del archivo Excel y los almacenamos en la variable datos_TV

head(datos_TV)           # Visualizamos las primeras filas de datos_TV
summary(datos_TV)        # Mostramos un resumen de datos_TV
str(datos_TV)            # Mostramos la estructura de datos_TV

media_coste <- aggregate(datos_TV$COSTE~ datos_TV$cadena, FUN = mean)  # Calculamos la media del costo agrupado por cadena de TV

medias_generos <- c(mean(datos_TV$H), mean(datos_TV$M))  # Calculamos la media de las variables H y M

medias_edades <- c(mean(datos_TV$E1), mean(datos_TV$E2), mean(datos_TV$E3), mean(datos_TV$E4), mean(datos_TV$E5), mean(datos_TV$E6), mean(datos_TV$E7))  # Calculamos la media de las variables E1 a E7

media_audiencias <- aggregate(datos_TV$ML ~ datos_TV$cadena, FUN = mean)  # Calculamos la media de la audiencia ML agrupada por cadena de TV

media_CPM <- aggregate(datos_TV$CPM ~ datos_TV$cadena, FUN = mean)  # Calculamos la media del CPM agrupado por cadena de TV

media_TI <- aggregate(datos_TV$TI ~ datos_TV$cadena, FUN = mean)  # Calculamos la media del TI agrupado por cadena de TV

media_TC <- aggregate(datos_TV$TC ~ datos_TV$cadena, FUN = mean)  # Calculamos la media del TC agrupado por cadena de TV

colores_cadena <- c("orange", "red", "green", "blue")  # Definimos los colores para las cadenas de TV

# Crear el grafico de barras
barplot(media_coste$`datos_TV$COSTE`, names.arg = media_coste$`datos_TV$cadena`, 
        xlab = "Cadena", ylab = "miles de €", main = "Coste Medio por Cadena", 
        col = colores_cadena)

# Crear el grafico de barras
barplot(medias_generos, names.arg = c("Hombre", "Mujer"), 
        xlab = "Género", ylab = "Porcentaje de Audiencia", main = "Audiencia Media por Género", 
        col = c("blue","pink"))

# Crear el grafico de barras
barplot(medias_edades, names.arg = c("4-12","13-24","25-44","45-54","55-64","65-742","+75"), 
        xlab = "Edades", ylab = "Porcentaje de Audiencia", main = "Audiencia Media por Edad", 
        col = "dark green")

# Crear el grafico de barras
barplot(media_audiencias$`datos_TV$ML`, names.arg = media_audiencias$`datos_TV$cadena`,
        xlab = "Cadena", ylab = "Miles de Personas", main = "Audiencia por Cadena", 
        col = colores_cadena)

# Crear el grafico de barras
barplot(media_CPM$`datos_TV$CPM`, names.arg = media_CPM$`datos_TV$cadena`,
        xlab = "Cadena", ylab = "Euros", main = "CPM por Cadena", 
        col = colores_cadena)

# Crear el grafico de barras combinado
barplot(media_TI$`datos_TV$TI`, names.arg = media_TI$`datos_TV$cadena`,
        xlab = "Cadena", ylab = "Porcentaje Audiencia", main = "TI por Cadena", 
        col = colores_cadena)

# A�adir un segundo grafico de barras en el mismo grafico
par(new = TRUE)
barplot(media_TC$`datos_TV$TC`, names.arg = media_TI$`datos_TV$cadena`,
        xlab = "", ylab = "", col = colores_cadena, ylim = c(0,max(media_TC$`datos_TV$TC`)))

# Cargar los datos del archivo JSON "comunidades_espana.json" y guardarlos en la variable "comunidades"
comunidades <- fromJSON("comunidades_espana.json")
print(comunidades)

# Crear un objeto de data frame con los nombres de las comunidades y los valores medios correspondientes
datos_prov <- data.frame(
  comunidad = c("Andaluc�a", "Islas Canarias", "Castilla-La Mancha", "Catalu�a", "Pa�s Vasco", "Galicia", "Comunidad de Madrid",
                "Comunidad Valenciana", "Arag�n","Regi�n Murcia","Castilla y Le�n"),
  valor = c(mean(datos_TV$AND),mean(datos_TV$CAN),mean(datos_TV$CLM),mean(datos_TV$CAT),mean(datos_TV$EUS),
            mean(datos_TV$GAL),mean(datos_TV$MAD),mean(datos_TV$VAL),mean(datos_TV$ARA),mean(datos_TV$MUR),
            mean(datos_TV$CYL))
)

# Imprimir el objeto "datos_prov"
print(datos_prov)

# Crear un histograma de la variable COSTE con colores de las barras "lightblue"
hist(datos_TV$COSTE, col = "lightblue", main = "Histograma de Costes", xlab = "€", ylab = "Frecuencia")

# Crear una matriz de correlacion redondeada de las variables numericas en "datos_TV"
matriz_1 <- round(cor(datos_num),1) 

# Imprimir la matriz de correlaci�n
print(matriz_1)

# Crear un gr�fico de correlaci�n usando la funci�n "corrplot"
corrplot(matriz_1, method = "number", type="upper")

# Mostrar el resumen estad�stico de la variable ML en "datos_TV"
summary(datos_TV$ML)

# Ajustar un modelo de regresi�n lineal con la variable de respuesta "ML" y las variables independientes especificadas
modelo_tv <- lm(datos_TV$ML~ datos_TV$cadena + datos_TV$`hora inicio`+ 
                  datos_TV$`hora fin`+datos_TV$COSTE+datos_TV$CPM+datos_TV$TI
                +datos_TV$TC)

# Mostrar los resultados del modelo de regresi�n
summary(modelo_tv)

# Obtener los coeficientes de las variables independientes del modelo
modelo_tv$coefficients

# Realizar predicciones utilizando el modelo ajustado
predicciones <- predict(modelo_tv)

# Calcular los residuos del modelo
residuales <- (datos_TV$ML-predicciones)

# Calcular el error cuadr�tico medio (RMSE) de los residuos
rmse <- sqrt(mean(residuales^2))
print(rmse)

# Crear un grafico de dispersi�n entre las observaciones reales y las predicciones del modelo
plot(datos_TV$ML, predicciones, ylab = "predicciones", xlab = "observaciones", main = "Modelo 1 TV")

# Seleccionar las columnas relevantes para el segundo modelo de regresion
datos_modelo_1_TV <- datos_TV[,c("ML","cadena","hora inicio","hora fin" ,"COSTE" , "CPM", "TI","TC")]
datos_modelo_1_TV <- datos_modelo_1_TV[,sapply(datos_modelo_1_TV, is.numeric)]

# Calcular la matriz de correlacion redondeada de las variables del segundo modelo
matriz_modelo_1_TV <- round(cor(datos_modelo_1_TV),1)

# Imprimir la matriz de correlacion del segundo modelo
print(matriz_modelo_1_TV)

# Mostrar un gráfico de correlacion para el segundo modelo
corrplot(matriz_modelo_1_TV, method = "number", type="upper")

# Ajustar un nuevo modelo de regresion lineal con las variables seleccionadas
modelo_tv_final <- lm(datos_TV$ML~ datos_TV$COSTE + datos_TV$CPM +datos_TV$TI)

# Mostrar los resultados del segundo modelo de regresion
summary(modelo_tv_final)

# Obtener los coeficientes de las variables independientes del segundo modelo
modelo_tv_final$coefficients

# Realizar predicciones utilizando el segundo modelo ajustado
predicciones <- predict(modelo_tv_final)

# Calcular los residuos del segundo modelo
residuales <- (datos_TV$ML-predicciones)

# Calcular el error cuadratico medio (RMSE) de los residuos del segundo modelo
rmse <- sqrt(mean(residuales^2))
print(rmse)

# Crear un grafico de dispersian entre las observaciones reales y las predicciones del segundo modelo
plot(datos_TV$ML, predicciones, ylab = "predicciones", xlab = "observaciones", main = "Modelo 2 TV")

# Seleccionar las columnas relevantes para el tercer modelo de regresión
datos_modelo_2_TV <- datos_TV[,c("TC","IA","IB","IC" ,"ID" , "IE")]
datos_modelo_2_TV <- datos_modelo_2_TV[,sapply(datos_modelo_2_TV, is.numeric)]

# Calcular la matriz de correlacion redondeada de las variables del tercer modelo
matriz_modelo_2_TV <- round(cor(datos_modelo_2_TV),1)

# Imprimir la matriz de correlacion del tercer modelo
print(matriz_modelo_2_TV)

# Mostrar un gráfico de correlacion para el tercer modelo
corrplot(matriz_modelo_2_TV, method = "number", type="upper")

# Ajustar un nuevo modelo de regresion lineal con las variables seleccionadas para el tercer modelo
modelo_tv2 <- lm(datos_TV$TC~  datos_TV$IA + datos_TV$IB  + 
                   datos_TV$IC +datos_TV$ID +datos_TV$IE)

# Mostrar los resultados del tercer modelo de regresión
summary(modelo_tv2)

# Obtener los coeficientes de las variables independientes del tercer modelo
modelo_tv2$coefficients

# Realizar predicciones utilizando el tercer modelo ajustado
predicciones <- predict(modelo_tv2)

# Calcular los residuos del tercer modelo
residuales <- (datos_TV$TC-predicciones)

# Calcular el error cuadratico medio (RMSE) de los residuos del tercer modelo
rmse <- sqrt(mean(residuales^2))
print(rmse)

# Crear un gráfico de dispersion entre las observaciones reales y las predicciones del tercer modelo
plot(datos_TV$TC, predicciones, ylab = "predicciones", xlab = "observaciones", main = "Modelo 3 TV")




# Cargar los datos desde un archivo Excel
datos_ADS <- read_excel("/cloud/project/Base_Datos_Publicidad.xlsx")

# Visualizar el contenido principal del archivo
head(datos_ADS)
summary(datos_ADS)
str(datos_ADS)

# Realizar cálculos y transformaciones en los datos
datos_ADS <- datos_ADS %>% 
  mutate(CTR = round(((CLICKS / IMPRESIONES) * 100), 2),
         CPC = ifelse(CLICKS != 0, round(INVERSION / CLICKS, 2), INVERSION),
         CostePorConv_Total = ifelse(CONVERSION_TOTAL != 0, round(INVERSION / CONVERSION_TOTAL, 2), INVERSION),
         CostePorConv_Final = ifelse(CONVERSION_FINAL != 0, round(INVERSION / CONVERSION_FINAL, 2), INVERSION),
         CPM = round((INVERSION / IMPRESIONES) * 1000, 2))

# Crear gráficos de barras
colores_redes <- c("blue", "purple", "black", "light blue")
conteo_por_red_social <- table(datos_ADS$RSS)

# Gráfico de barras: Número de observaciones por red social
ggplot(data.frame(RRSS = names(conteo_por_red_social), conteo = as.numeric(conteo_por_red_social)), aes(x = RRSS, y = conteo, fill = RRSS)) +
  geom_bar(stat = "identity") +
  xlab("Red Social") +
  ylab("Número de observaciones") +
  ggtitle("Número de observaciones por red social") +
  scale_fill_manual(values = colores_redes)

conteo_por_edades <- table(datos_ADS$EDAD)

# Gráfico de barras: Número de observaciones por edad
ggplot(data.frame(edades = names(conteo_por_edades), conteo = as.numeric(conteo_por_edades)), aes(x = edades, y = conteo, fill = edades)) +
  geom_bar(stat = "identity") +
  xlab("Franja de Edad") +
  ylab("Número de observaciones") +
  ggtitle("Número de observaciones por edad")

conteo_por_genero <- table(datos_ADS$GENERO)

# Gráfico de barras: Número de observaciones por género
ggplot(data.frame(Género = names(conteo_por_genero), conteo = as.numeric(conteo_por_genero)), aes(x = Género, y = conteo, fill = Género)) +
  geom_bar(stat = "identity") +
  xlab("Género") +
  ylab("Número de observaciones") +
  ggtitle("Número de observaciones por Género") +
  scale_fill_manual(values = c("light pink", "dark blue"))

# Calcular medias de variables por red social
media_impresiones <- aggregate(datos_ADS$IMPRESIONES ~ datos_ADS$RSS, FUN = mean)

# Gráfico de barras: Impresiones medias por red social
barplot(media_impresiones$`datos_ADS$IMPRESIONES`, names.arg = media_impresiones$`datos_ADS$RSS`,
        xlab = "Red Social", ylab = "Impresiones", main = "Impresiones medias por RRSS",
        col = colores_redes)

# Calcula la media de la variable CLICKS agrupada por la variable RSS en el dataframe datos_ADS y lo almacena en media_clicks
media_clicks <- aggregate(datos_ADS$CLICKS ~ datos_ADS$RSS, FUN = mean)

# Crea un gráfico de barras utilizando los valores de datos_ADS$CLICKS en el eje y, los valores de datos_ADS$RSS como etiquetas en el eje x,
# y agrega etiquetas y títulos adecuados. El color de las barras se toma de la variable colores_redes.
barplot(media_clicks$`datos_ADS$CLICKS`, names.arg = media_clicks$`datos_ADS$RSS`, 
        xlab = "Red Social", ylab = "clicks", main = "Clciks medios por RRSS", 
        col = colores_redes)

# Calcula la media de la variable INVERSION agrupada por la variable RSS en el dataframe datos_ADS y lo almacena en media_inversion
media_inversion <- aggregate(datos_ADS$INVERSION ~ datos_ADS$RSS, FUN = mean)

# Crea un gráfico de barras utilizando los valores de datos_ADS$INVERSION en el eje y, los valores de datos_ADS$RSS como etiquetas en el eje x,
# y agrega etiquetas y títulos adecuados.
barplot(media_inversion$`datos_ADS$INVERSION`, names.arg = media_inversion$`datos_ADS$RSS`, 
        xlab = "Red Social", ylab = "€", main = "Inversión media por RRSS", 
        col = colores_redes)

# Calcula la media de la variable CONVERSION_TOTAL agrupada por la variable RSS en el dataframe datos_ADS y lo almacena en media_conversion
media_conversion <- aggregate(datos_ADS$CONVERSION_TOTAL ~ datos_ADS$RSS, FUN = mean)

# Crea un gráfico de barras utilizando los valores de datos_ADS$CONVERSION_TOTAL en el eje y, los valores de datos_ADS$RSS como etiquetas en el eje x,
# y agrega etiquetas y títulos adecuados.
barplot(media_conversion$`datos_ADS$CONVERSION_TOTAL`, names.arg = media_conversion$`datos_ADS$RSS`, 
        xlab = "Red Social", ylab = "Cantidad", main = "Conversión media por RRSS", 
        col = colores_redes)

# Calcula la media de la variable CONVERSION_FINAL agrupada por la variable RSS en el dataframe datos_ADS y lo almacena en media_conversion_f
media_conversion_f <- aggregate(datos_ADS$CONVERSION_FINAL ~ datos_ADS$RSS, FUN = mean)

# Crea un gráfico de barras utilizando los valores de datos_ADS$CONVERSION_FINAL en el eje y, los valores de datos_ADS$RSS como etiquetas en el eje x,
# y agrega etiquetas y títulos adecuados.
barplot(media_conversion_f$`datos_ADS$CONVERSION_FINAL`, names.arg = media_conversion_f$`datos_ADS$RSS`, 
        xlab = "Red Social", ylab = "Cantidad", main = "Conversión final media por RRSS", 
        col = colores_redes)

# Almacena en la variable modelo_1_ADS la regresión lineal que hemos definido utilizando las variables especificadas.
modelo_1_ADS <- lm(datos_ADS$INVERSION ~ datos_ADS$IMPRESIONES + datos_ADS$RSS + datos_ADS$CLICKS + datos_ADS$CONVERSION_TOTAL 
                   + datos_ADS$CONVERSION_FINAL)
# Muestra los resultados de la regresión lineal
summary(modelo_1_ADS)
# Muestra los coeficientes del modelo
modelo_1_ADS$coefficients

# Selecciona las columnas especificadas del dataframe datos_ADS y almacena el resultado en datos_modelo_1
datos_modelo_1 <- datos_ADS[,c("INVERSION", "IMPRESIONES", "RSS", "CLICKS", "CONVERSION_TOTAL", "CONVERSION_FINAL")]
# Filtra las columnas numéricas de datos_modelo_1 y almacena el resultado en datos_modelo_1
datos_modelo_1 <- datos_modelo_1[,sapply(datos_modelo_1, is.numeric)]

# Calcula la matriz de correlación redondeada de datos_modelo_1 y almacena el resultado en matriz_modelo_1
matriz_modelo_1 <- round(cor(datos_modelo_1), 1)

# Imprime la matriz de correlación matriz_modelo_1
print(matriz_modelo_1)

# Crea un gráfico de correlación utilizando la matriz de correlación matriz_modelo_1 y agrega etiquetas y títulos adecuados
corrplot(matriz_modelo_1, method = "number", type = "upper")

# Muestra los resultados de la regresión lineal modelo_1_ADS
summary(modelo_1_ADS)
# Muestra los coeficientes del modelo modelo_1_ADS
modelo_1_ADS$coefficients

# Realiza predicciones utilizando el modelo modelo_1_ADS
predicciones <- predict(modelo_1_ADS)

# Calcula los residuales restando las predicciones de los valores reales
residuales <- (datos_ADS$INVERSION - predicciones)

# Calcula el error cuadrático medio (RMSE) de los residuales
rmse <- sqrt(mean(residuales^2))
print(rmse)

# Crea un gráfico de dispersión con los valores reales en el eje y y las predicciones en el eje x,
# y agrega etiquetas y títulos adecuados.
plot(datos_ADS$INVERSION, predicciones, ylab = "predicciones", xlab = "observaciones", main = "Modelo 1 ADS")

# Almacena en la variable modelo_2_ADS la regresión lineal que hemos definido utilizando las variables especificadas.
modelo_2_ADS <- lm(datos_ADS$INVERSION ~ datos_ADS$IMPRESIONES + datos_ADS$CPC 
                   + datos_ADS$CTR + datos_ADS$CostePorConv_Total + datos_ADS$CostePorConv_Total)
# Muestra los resultados de la regresión lineal
summary(modelo_2_ADS)
# Muestra los coeficientes del modelo
modelo_2_ADS$coefficients

# Selecciona las columnas especificadas del dataframe datos_ADS y almacena el resultado en datos_modelo_2
datos_modelo_2 <- datos_ADS[,c("INVERSION", "IMPRESIONES", "CPC", "CTR", "CostePorConv_Total", "CostePorConv_Total")]
# Filtra las columnas numéricas de datos_modelo_2 y almacena el resultado en datos_modelo_2
datos_modelo_2 <- datos_modelo_2[,sapply(datos_modelo_2, is.numeric)]

# Calcula la matriz de correlación redondeada de datos_modelo_2 y almacena el resultado en matriz_modelo_2
matriz_modelo_2 <- round(cor(datos_modelo_2), 1)

# Imprime la matriz de correlación matriz_modelo_2
print(matriz_modelo_2)

# Crea un gráfico de correlación utilizando la matriz de correlación matriz_modelo_2 y agrega etiquetas y títulos adecuados
corrplot(matriz_modelo_2, method = "number", type = "upper")

# Realiza predicciones utilizando el modelo modelo_2_ADS
predicciones <- predict(modelo_2_ADS)

# Calcula los residuales restando las predicciones de los valores reales
residuales <- (datos_ADS$INVERSION - predicciones)

# Calcula el error cuadrático medio (RMSE) de los residuales
rmse <- sqrt(mean(residuales^2))
print(rmse)

# Crea un gráfico de dispersión con los valores reales en el eje y y las predicciones en el eje x,
# y agrega etiquetas y títulos adecuados.
plot(datos_ADS$INVERSION, predicciones, ylab = "predicciones", xlab = "observaciones", main = "Modelo 2 ADS")


# Almacena en la variable modelo_3_ADS la regresión lineal que hemos definido utilizando las variables especificadas.
modelo_3_ADS <- lm(datos_ADS$INVERSION ~ datos_ADS$IMPRESIONES + datos_ADS$CPC + datos_ADS$CostePorConv_Total + datos_ADS$CostePorConv_Final)
# Muestra los resultados de la regresión lineal
summary(modelo_3_ADS)
# Muestra los coeficientes del modelo
modelo_3_ADS$coefficients 

# Realiza predicciones utilizando el modelo modelo_3_ADS
predicciones <- predict(modelo_3_ADS)

# Calcula los residuales restando las predicciones de los valores reales
residuales <- (datos_ADS$INVERSION - predicciones)

# Calcula el error cuadrático medio (RMSE) de los residuales
rmse <- sqrt(mean(residuales^2))
print(rmse)

# Crea un gráfico de dispersión con los valores reales en el eje y y las predicciones en el eje x,
# y agrega etiquetas y títulos adecuados.
plot(datos_ADS$INVERSION, predicciones, ylab = "predicciones", xlab = "observaciones", main = "Modelo 3 ADS")

# Calcula la desviación estándar de la variable INVERSION y la almacena en desviacion_estandar
desviacion_estandar <- sd(datos_ADS$INVERSION)

# Define el umbral para los valores atípicos (3 desviaciones estándar)
umbral <- 3

# Identifica los valores atípicos basados en el umbral definido
valores_atipicos <- datos_ADS$INVERSION[abs(datos_ADS$INVERSION - mean(datos_ADS$INVERSION)) > umbral * datos_ADS$INVERSION]

# Muestra los valores atípicos
valores_atipicos

# Filtra los registros correspondientes a los valores atípicos y almacénalos en registros_atipicos
registros_atipicos <- datos_ADS %>% filter(INVERSION %in% valores_atipicos)

# Muestra los registros correspondientes a los valores atípicos
registros_atipicos

# Filtra los registros que no son atípicos y almacénalos en datos_sin_atipicos
datos_sin_atipicos <- datos_ADS %>% filter(!INVERSION %in% registros_atipicos)

# Muestra el contenido principal de datos_sin_atipicos
head(datos_sin_atipicos)

# Muestra un resumen de los datos en datos_sin_atipicos
summary(datos_sin_atipicos)

# Muestra la estructura de los datos en datos_sin_atipicos
str(datos_sin_atipicos)

# Almacena en la variable modelo_3 la regresión lineal que hemos definido utilizando las variables especificadas.
modelo_3 <- lm(datos_sin_atipicos$INVERSION ~ datos_sin_atipicos$IMPRESIONES + datos_sin_atipicos$CPC + datos_sin_atipicos$CTR +
                 datos_sin_atipicos$CostePorConv_Total + datos_sin_atipicos$CostePorConv_Final)
# Muestra los resultados de la regresión lineal
summary(modelo_3)
# Muestra los coeficientes del modelo
modelo_3$coefficients  

# Realiza predicciones utilizando el modelo modelo_3
predicciones <- predict(modelo_3)

# Calcula los residuales restando las predicciones de los valores reales
residuales <- (datos_sin_atipicos$INVERSION - predicciones)

# Calcula el error cuadrático medio (RMSE) de los residuales
rmse <- sqrt(mean(residuales^2))
print(rmse)