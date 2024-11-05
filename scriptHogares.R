#Lectura de datos
install.packages("readxl")
library(readxl)
install.packages("arules")
library(arules)



dataHogaresOriginal <- read_excel('C:\\Users\\Lester\\Documents\\MAESTRIA\\INTO MINERIA DE DATOS\\proyecto_parte1\\ENCOVI2023_Hogares.xlsx')

dataHogares <- dataHogaresOriginal


dataHogaresFiltrado <- dataHogares[, c("DEPTO", "PPB02", "P01A01", "P01A02", "P01A03", "P01A04", "P01A05A", "P01A05B", "P01A05C", "P01A05D", "P01A05E", "P01A05F", "P01A06", "P01B01", "P01D07", "P01D08", "P01D16")]

dataHogaresFiltrado <- as.data.frame(dataHogaresFiltrado)
reglasHogares <- apriori(dataHogaresFiltrado, parameter = list(support=0.2, confidence = 0.5))

reglasHogaresFrame <- as(reglasHogares, "data.frame")








  