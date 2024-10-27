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

reglasPersonasFrameSexo <-  reglasPersonasFrame[grepl("PPA02", reglasPersonasFrame[[1]]), ]

#seleccion de campos
dataPersonasFiltrado <- dataPersonas[, c("DEPTO", "AREA", "POBREZA", "PPA02", "PPA03", "PPA06", "PPA07A", "PPA07B", "PPA07C", "PPA07D", "PPA07E", "PPA07F", "P04A01A", "P04A02", "P04A03", "P04A04A", "P04A05", "P04A06", "P04A07", "P04A08", "P04A09", "P04A10", "P04A11", "P04A12", "P05B02", "P05B03A", "P05B04", "P05B05A", "P05B05B", "P05B06A", "P05B06B", "P05B06C", "P05B07", "P05D19", "P05E01", "P05E03", "P05E06A", "P05E18", "P05E19", "P05E20", "P05E22", "P05E23A", "P05E23B", "P05E23C", "P05E23D", "P05E23E", "P05E23F", "P05E23G", "P05E23H", "P05E23I", "P05E23J", "P05E23K", "P06B01", "P06B02A", "P08A02", "P08A03A", "P08A03B", "P08A03C", "P10B11C", "P10B11E", "P10B11I", "P10C22", "P10C27", "P12A04", "P12A05", "P12A06A", "P12A06B", "P12A06C", "YLAB_PUBLI", "Edad", "Grupos_edad", "PET", "PEA", "OCUPADOS", "DESOCUPADOS", "SUBVISIBLES", "INACTIVOS", "FORMAL_INFORMAL" )]
reglasPersonasFiltrado <- apriori(dataPersonasFiltrado, parameter = list(support=0.2, confidence = 0.5))

reglasPersonasFiltradoFrame <- as(reglasPersonasFiltrado, "data.frame")
reglasPersonasFiltradoFrameConfidence <-  reglasPersonasFiltradoFrame[reglasPersonasFiltradoFrame[[3]] < 0.9, ]
reglasPersonasFiltradoFrameConfidence <-  reglasPersonasFiltradoFrameConfidence[grepl("P04A09=[1,2]", reglasPersonasFiltradoFrameConfidence[[1]]), ]

reglasPersonasFiltradoFrameConfidence <- reglasPersonasFiltradoFrameConfidence[!grepl("P04A09=\\[1,2\\]", reglasPersonasFiltradoFrameConfidence[[1]]), ]

reglasPersonasFiltradoFrameConfidence <- reglasPersonasFiltradoFrameConfidence[!grepl("P04A10=\\[1,25)", reglasPersonasFiltradoFrameConfidence[[1]]), ]

reglasPersonasFiltradoFrameConfidence <- reglasPersonasFiltradoFrameConfidence[!grepl("\\{\\}", reglasPersonasFiltradoFrameConfidence[[1]]), ]

