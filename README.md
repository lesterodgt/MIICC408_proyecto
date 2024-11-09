# MIICC408_proyecto

# Proyecto - parte 1

## Conjunto de datos

Se analizará el conjunto de datos obtenido de la encuesta nacional de condiciones de Vida ENCOVI, la cual brinda información sobre la pobreza a nivel nacional considerando características de la población y hogares. El conjunto de datos cuenta con información obtenido de diferentes años siendo estos: 2023, 2014, 2011, 2006, 2000, en este caso el análisis toma únicamente los datos 2023 para tener un enfoque de la realidad actual más próximo.

Enlace: https://datos.minfin.gob.gt/dataset?groups=contrataciones-adquisiciones

Conjunto: hogares

Año: 2023 - anual

## Requisitos
* R
* Ide a elección
* Conjunto de datos

## Inicio
En esta sección se agregaron las librerías necesarias para el total del análisis , incluyendo la lectura de los datos a analizar.
```r
install.packages("readxl")
library(readxl)
install.packages("arules")
library(arules)
install.packages("fim4r")
library(fim4r)
install.packages("ggplot2")
library(ggplot2)
install.packages("writexl")
library(writexl)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggalt")
library(ggalt)
dataHogaresOriginal <- read_excel('C:\\Users\\Lester\\Documents\\MAESTRIA\\INTO MINERIA DE DATOS\\proyecto_parte1\\ENCOVI2023_Hogares.xlsx')
```
## Reglas de asociación A priori

En esta sección se realiza el análisis de reglas a priori, para no tener problema con los datos originales se obtiene una copia de los datos.

```r
dataHogares2 <- dataHogaresOriginal
#Ejecución de algoritmo a priori
reglasHogares2 <- apriori(dataHogares2 , parameter = list(support = 0.3, confidence = 0.6, minlen = 2))
#Convertir reglas en formato frame para analisis
reglasHogaresFrame2 <- as(reglasHogares2, "data.frame")
#Guardar reglas en archivo
write_xlsx(reglasHogaresFrame2 , "C:\\Users\\Lester\\Documents\\MAESTRIA\\INTO MINERIA DE DATOS\\proyecto_parte1\\ReglasHogar_Apriori2.xlsx")
```
## Reglas de asociación FP-Growth

En esta sección se realiza el análisis de reglas FP-Growth, para no tener problema con los datos originales se obtiene una copia de los datos y se filtran ciertas columnas debido a que el algoritmo demanda más recursos.

```r
dataHogares2Filtrado3 <- dataHogares2[, c("DEPTO", "DOMINIO","POBREZA","TOTAL_PERS", "PPB02", "P01G01","P01A01", "P01A02", "P01A03", "P01A04", "P01A05A", "P01A05B", "P01A05C", "P01A05E", "P01A05F", "P01A06", "P01B01", "P01D01", "P01D02", "P01D16", "P01D17", "P01D22", "P01D34", "P01H02", "P01H07", "P01H08", "P01H09", "P01I01B", "P02B01")]

#Se cambian los valores null por 0 en el caso de no tener datos, según las columnas elegidas el valor no afecta.
dataHogares2Filtrado3[is.na(dataHogares2Filtrado3)] <- 0

#Se convierten los datos a un formato compatible con el algoritmo
dataHogares2FiltradoFrame3<- as.data.frame(dataHogares2Filtrado3)

#Se ejecuta el algoritmo
regrlasDataHogares2FiltradoFrame3 <- fim4r(dataHogares2FiltradoFrame3, method = "fpgrowth", target="rules", supp = .3, conf = .6)

#Se pueden almacenar las reglas en un archivo de excel
reglasHogarFpgrowth2Frame3 <- as(regrlasDataHogares2FiltradoFrame3, "data.frame")
write_xlsx(reglasHogarFpgrowth2Frame3, "C:\\Users\\Lester\\Documents\\MAESTRIA\\INTO MINERIA DE DATOS\\proyecto_parte1\\ReglasHogar_fpgrowthsup3.xlsx")

#O bien se pueden insepccionar las reglas
inspect(regrlasDataHogares2FiltradoFrame3[0:100])
inspect(regrlasDataHogares2FiltradoFrame3[101:200])
inspect(regrlasDataHogares2FiltradoFrame3[201:300])
inspect(regrlasDataHogares2FiltradoFrame3[301:400])
inspect(regrlasDataHogares2FiltradoFrame3[401:500])
inspect(regrlasDataHogares2FiltradoFrame3[501:600])
inspect(regrlasDataHogares2FiltradoFrame3[601:700])
inspect(regrlasDataHogares2FiltradoFrame3[701:800])
```

## Análisis de clúster

Para esta sección de análisis de clúster se hacen modificaciones a ciertos valores de tal manera se facilite la visualización de gráficos.

```r
dataHogares2Filtrado3$P01A03[dataHogares2Filtrado3$P01A03 == 98] <- 6
dataHogares2Filtrado3$P01D16[dataHogares2Filtrado3$P01D16 == 98] <- 6
#El algoritmo no permite tener columnas con valores vaios, por tal razón esta instrucción podria ser util para eliminar las columnas que no cumplan la restricción. Se debe analizar los casos si la información es faltante segun la interpretación
dataHogares2Filtrado3_1 <- na.omit(dataHogares2Filtrado3)

#Para generar el analisis de cluster se debe establecer el numero de centroides, en este caso serán 4
clusterHogar <- kmeans(dataHogares2Filtrado3_1, centers=4)
```

Gráficos
La generación de gráficos ayudar a comprender de mejor manera el comportamiento de los clústers
```r
#1 Visualización entre el nivel de pobreza y el departamento
ggplot(dataHogares2Filtrado3_1, aes(x = POBREZA, y = DEPTO, color = as.factor(clusterHogar$cluster), shape = as.factor(clusterHogar$cluster)))+
  geom_point(alpha = 0.6, size = 3) +
  geom_point(data = as.data.frame(clusterHogar$centers), 
            aes(x=POBREZA, y = DEPTO), color = "black", fill="black", size=4, shape=21)+
  geom_encircle(aes(group = clusterHogar$cluster, fill = as.factor(clusterHogar$cluster)),
                alpha = 0.2, s_shape = 1, expand = 0.05)+
  scale_colour_manual(values = c("blue", "orange", "purple", "green" ))+
  scale_shape_manual(values=c(16,17,15,18))+
  labs(title = "1. POBREZA vs DEPARTAMENTO")+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5, face="bold"),
    legend.position = "right",
    legend.title = element_blank()
  )

#2 Visualización entre el numero de hogares en una vivienda y la relación entre la cantidad de dormitorios que posee la vivienda.
ggplot(dataHogares2Filtrado3_1, aes(x = PPB02, y = P01D02, color = as.factor(clusterHogar$cluster), shape = as.factor(clusterHogar$cluster)))+
  geom_point(alpha = 0.6, size = 3) +
  geom_point(data = as.data.frame(clusterHogar$centers), 
             aes(x=PPB02, y = P01D02), color = "black", fill="black", size=4, shape=21)+
  geom_encircle(aes(group = clusterHogar$cluster, fill = as.factor(clusterHogar$cluster)),
                alpha = 0.2, s_shape = 1, expand = 0.05)+
  scale_colour_manual(values = c("blue", "orange", "purple", "green" ))+
  scale_shape_manual(values=c(16,17,15,18))+
  labs(title = "2. #HOGARES EN UNA VIVIENDA vs DORMITORIO", x="#Hogares en una vivienda", y="# dormitorios")+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5, face="bold"),
    legend.position = "right",
    legend.title = element_blank()
  )


#3 Visualización de hogares por vivienda según departamento
ggplot(dataHogares2Filtrado3_1, aes(x = PPB02, y = DEPTO, color = as.factor(clusterHogar$cluster), shape = as.factor(clusterHogar$cluster)))+
  geom_point(alpha = 0.6, size = 3) +
  geom_point(data = as.data.frame(clusterHogar$centers), 
             aes(x=PPB02, y = DEPTO), color = "black", fill="black", size=4, shape=21)+
  geom_encircle(aes(group = clusterHogar$cluster, fill = as.factor(clusterHogar$cluster)),
                alpha = 0.2, s_shape = 1, expand = 0.05)+
  scale_colour_manual(values = c("blue", "orange", "purple", "green" ))+
  scale_shape_manual(values=c(16,17,15,18))+
  labs(title = "3. #HOGARES EN UNA VIVIENDA vs DEPARTAMENTO", x="#Hogares en una vivienda", y="DEPARTAMENTO")+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5, face="bold"),
    legend.position = "right",
    legend.title = element_blank()
  )


#4 Visualizacióndel nivel de pobreza y su relación por el tipo de area
ggplot(dataHogares2Filtrado3_1, aes(x = P01G01, y = DOMINIO, color = as.factor(clusterHogar$cluster), shape = as.factor(clusterHogar$cluster)))+
  geom_point(alpha = 0.6, size = 3) +
  geom_point(data = as.data.frame(clusterHogar$centers), 
             aes(x=P01G01, y = DOMINIO), color = "black", fill="black", size=4, shape=21)+
  geom_encircle(aes(group = clusterHogar$cluster, fill = as.factor(clusterHogar$cluster)),
                alpha = 0.2, s_shape = 1, expand = 0.05)+
  scale_colour_manual(values = c("blue", "orange", "purple", "green" ))+
  scale_shape_manual(values=c(16,17,15,18))+
  labs(title = "4. Nivel de pobreza vs Tipo Area", x="Nivel de pobreza", y="Tipo area")+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5, face="bold"),
    legend.position = "right",
    legend.title = element_blank()
  )




#5 Visualización del tratamiendo de agua y el tiempo
ggplot(dataHogares2Filtrado3_1, aes(x = P01D16, y = P01A03, color = as.factor(clusterHogar$cluster), shape = as.factor(clusterHogar$cluster)))+
  geom_point(alpha = 0.6, size = 3) +
  geom_point(data = as.data.frame(clusterHogar$centers), 
             aes(x=P01D16, y = P01A03), color = "black", fill="black", size=4, shape=21)+
  geom_encircle(aes(group = clusterHogar$cluster, fill = as.factor(clusterHogar$cluster)),
                alpha = 0.2, s_shape = 1, expand = 0.05)+
  scale_colour_manual(values = c("blue", "orange", "purple", "green" ))+
  scale_shape_manual(values=c(16,17,15,18))+
  labs(title = "5. Tratamiendo de agua vs Material predominante techo", x="Tratamiento de agua", y="Material predominante techo")+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5, face="bold"),
    legend.position = "right",
    legend.title = element_blank()
  )

```
