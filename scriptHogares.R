#Lectura de datos
install.packages("readxl")
library(readxl)
install.packages("arules")
library(arules)
install.packages("fim4r")
library(fim4r)
install.packages("ggplot2")
library(ggplot2)



dataHogaresOriginal <- read_excel('C:\\Users\\Lester\\Documents\\MAESTRIA\\INTO MINERIA DE DATOS\\proyecto_parte1\\ENCOVI2023_Hogares.xlsx')

dataHogares <- dataHogaresOriginal


dataHogaresFiltrado <- dataHogares[, c("DEPTO", "PPB02", "P01A01", "P01A02", "P01A03", "P01A04", "P01A05A", "P01A05B", "P01A05C", "P01A05D", "P01A05E", "P01A05F", "P01A06", "P01B01", "P01D07", "P01D08", "P01D16")]
dataHogaresFiltrado <- as.data.frame(dataHogaresFiltrado)

#apriori

reglasHogares <- apriori(dataHogaresFiltrado, parameter = list(support=0.2, confidence = 0.5))
reglasHogaresFrame <- as(reglasHogares, "data.frame")



install.packages("writexl")
library(writexl)
write_xlsx(reglasHogaresFrame, "C:\\Users\\Lester\\Documents\\MAESTRIA\\INTO MINERIA DE DATOS\\proyecto_parte1\\ReglasHogar_Apriori.xlsx")


reglasHogares2 <- apriori(dataHogaresFiltrado, parameter = list(support = 0.3, confidence = 0.6, minlen = 2))
reglasHogaresFrame2 <- as(reglasHogares2, "data.frame")
write_xlsx(reglasHogaresFrame2, "C:\\Users\\Lester\\Documents\\MAESTRIA\\INTO MINERIA DE DATOS\\proyecto_parte1\\ReglasHogar_Apriori2.xlsx")


##fpgrowth

reglasHogarFpgrowth <- fim4r(dataHogaresFiltrado, method = "fpgrowth", target="rules", supp = .2, conf = .5)

reglasFrameHogarFpgrowth <- as(reglasHogarFpgrowth, "data.frame")

reglasFrameHogarFpgrowthConfidence <-  reglasFrameHogarFpgrowth[reglasFrameHogarFpgrowth[[3]] < 0.9, ]

reglasFrameHogarFpgrowthConfidence2 <- reglasFrameHogarFpgrowthConfidence[grepl("[()]", reglasFrameHogarFpgrowthConfidence[[1]]), ]



#kmeans

dataHogaresFiltrado2 <- na.omit(dataHogaresFiltrado)


clusterHogar <- kmeans(dataHogaresFiltrado2, centers=5)

ggplot(dataHogaresFiltrado2, aes(x = P01D16, y = P01A04, color = as.factor(clusterHogar$cluster)))+
  geom_point(alpha = 0.6, size = 2) +
  geom_point(data = as.data.frame(clusterHogar$centers), aes(x=P01D16, y = P01A04), color = "black", size=4, shape=17)+
  labs(title = "Edad vs hogares en casa", color ="Cluster de emigración")+
  theme_minimal()