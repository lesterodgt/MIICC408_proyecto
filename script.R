#Lectura de datos
install.packages("readxl")
library(readxl)
install.packages("arules")
library(arules)



dataPersonasOriginal <- read_excel('C:\\Users\\Lester\\Documents\\MAESTRIA\\INTO MINERIA DE DATOS\\proyecto_parte1\\ENCOVI2023_Personas.xlsx')
dataHogaresOriginal <- read_excel('C:\\Users\\Lester\\Documents\\MAESTRIA\\INTO MINERIA DE DATOS\\proyecto_parte1\\ENCOVI2023_Hogares.xlsx')

dataPersonas <- dataPersonasOriginal
#Limpiando
unique(dataPersonas$AÑO)
dataPersonas$AÑO <- NULL #2023

unique(dataPersonas$P09I01D)
dataPersonas$P09I01D <- NULL #2023

unique(dataPersonas$PET)
dataPersonas$PET[is.na(dataPersonas$PET)] <- 0


unique(dataPersonas$PEA)
dataPersonas$PEA[is.na(dataPersonas$PEA)] <- 0

unique(dataPersonas$OCUPADOS)
dataPersonas$OCUPADOS[is.na(dataPersonas$OCUPADOS)] <- 0

unique(dataPersonas$DESOCUPADOS)
dataPersonas$DESOCUPADOS[is.na(dataPersonas$DESOCUPADOS)] <- 0

unique(dataPersonas$SUBVISIBLES)
dataPersonas$SUBVISIBLES[is.na(dataPersonas$SUBVISIBLES)] <- 0

unique(dataPersonas$INACTIVOS)
dataPersonas$INACTIVOS[is.na(dataPersonas$INACTIVOS)] <- 0

#Generando reglas
reglasPersonas <- apriori(dataPersonas, parameter = list(support=0.2, confidence = 0.5))

#revisión de reglas
reglasPersonasFrame <- as(reglasPersonas, "data.frame")

