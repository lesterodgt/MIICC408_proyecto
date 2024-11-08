#Lectura de datos
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


dataHogaresOriginal <- read_excel('C:\\Users\\Lester\\Documents\\MAESTRIA\\INTO MINERIA DE DATOS\\proyecto_parte1\\ENCOVI2023_Hogares.xlsx')

dataHogares <- dataHogaresOriginal
dataHogares2 <- dataHogaresOriginal

dataHogaresFiltrado <- dataHogares[, c("DEPTO", "PPB02", "P01A01", "P01A02", "P01A03", "P01A04", "P01A05A", "P01A05B", "P01A05C", "P01A05D", "P01A05E", "P01A05F", "P01A06", "P01B01", "P01D07", "P01D08", "P01D16")]
dataHogaresFiltrado <- as.data.frame(dataHogaresFiltrado)


#apriori

reglasHogares <- apriori(dataHogaresFiltrado, parameter = list(support=0.2, confidence = 0.5))
reglasHogaresFrame <- as(reglasHogares, "data.frame")




write_xlsx(reglasHogaresFrame, "C:\\Users\\Lester\\Documents\\MAESTRIA\\INTO MINERIA DE DATOS\\proyecto_parte1\\ReglasHogar_Apriori.xlsx")


reglasHogares2 <- apriori(dataHogaresFiltrado, parameter = list(support = 0.3, confidence = 0.6, minlen = 2))
reglasHogaresFrame2 <- as(reglasHogares2, "data.frame")
write_xlsx(reglasHogaresFrame2, "C:\\Users\\Lester\\Documents\\MAESTRIA\\INTO MINERIA DE DATOS\\proyecto_parte1\\ReglasHogar_Apriori2.xlsx")


dataHogares2$P01C03F <- NULL
reglasHogaresSinFilto <- apriori(dataHogares2, parameter = list(support = 0.3, confidence = 0.6, minlen = 2))
reglasHogaresSinFiltroFrame2 <- as(reglasHogaresSinFilto, "data.frame")

inspect(reglasHogaresSinFilto[0:100])
inspect(reglasHogaresSinFilto[101:200])
inspect(reglasHogaresSinFilto[201:300])
inspect(reglasHogaresSinFilto[301:400])
inspect(reglasHogaresSinFilto[401:500])
inspect(reglasHogaresSinFilto[501:600])
inspect(reglasHogaresSinFilto[601:700])
inspect(reglasHogaresSinFilto[701:800])

##fpgrowth
## limitar datos
dataHogares2Filtrado <- dataHogares2[, c("DEPTO", "TOTAL_PERS", "PPB01", "PPB02", "P01A01", "P01A02", "P01A03", "P01A04", "P01A05A", "P01A05B", "P01A05C", "P01A05D", "P01A05E", "P01A05F", "P01A06", "P01B01", "P01D01", "P01D02", "P01D07", "P01D08", "P01D16", "P01D17", "P01D22", "P01D34", "P01H02", "P01H07", "P01H08", "P01H09", "P01I01B", "P02B01")]
dataHogares2Filtrado3 <- dataHogares2[, c("DEPTO", "TOTAL_PERS", "PPB02", "P01A01", "P01A02", "P01A03", "P01A04", "P01A05A", "P01A05B", "P01A05C", "P01A05E", "P01A05F", "P01A06", "P01B01", "P01D01", "P01D02", "P01D16", "P01D17", "P01D22", "P01D34", "P01H02", "P01H07", "P01H08", "P01H09", "P01I01B", "P02B01")]

dataHogares2$UPM<- NULL
dataHogares2$FACTOR<- NULL
dataHogares2$P02B03B<- NULL
dataHogares2$P02B03C<- NULL
dataHogares2$P02B04B<- NULL
dataHogares2$P02B02G<- NULL
dataHogares2$P02B03A<- NULL
dataHogares2$P02B04A<- NULL
dataHogares2$P02B04C<- NULL
dataHogares2$P02B05A<- NULL
dataHogares2$P02B05B<- NULL

str(dataHogares2Filtrado)
head(dataHogares2Filtrado)
dataHogares2Filtrado[is.na(dataHogares2Filtrado)] <- 0


dataHogares2FiltradoFrame <- as.data.frame(dataHogares2Filtrado)

data_items <- apply(dataHogares2Filtrado, 1, function(row) {
  paste(names(dataHogares2Filtrado), row, sep = "=")
})
head(data_items)

data_list <- split(data_items, seq(nrow(dataHogares2Filtrado)))

transactions <- as(data_list, "transactions")
head(transactions)


#############
dataHogares2Filtrado3[is.na(dataHogares2Filtrado3)] <- 0
dataHogares2FiltradoFrame3<- as.data.frame(dataHogares2Filtrado3)
regrlasDataHogares2FiltradoFrame3 <- fim4r(dataHogares2FiltradoFrame3, method = "fpgrowth", target="rules", supp = .3, conf = .6)
reglasHogarFpgrowth2Frame3 <- as(regrlasDataHogares2FiltradoFrame3, "data.frame")

write_xlsx(reglasHogarFpgrowth2Frame3, "C:\\Users\\Lester\\Documents\\MAESTRIA\\INTO MINERIA DE DATOS\\proyecto_parte1\\ReglasHogar_fpgrowthsup3.xlsx")

inspect(regrlasDataHogares2FiltradoFrame3[0:100])
inspect(regrlasDataHogares2FiltradoFrame3[101:200])
inspect(regrlasDataHogares2FiltradoFrame3[201:300])
inspect(regrlasDataHogares2FiltradoFrame3[301:400])
inspect(regrlasDataHogares2FiltradoFrame3[401:500])
inspect(regrlasDataHogares2FiltradoFrame3[501:600])
inspect(regrlasDataHogares2FiltradoFrame3[601:700])
inspect(regrlasDataHogares2FiltradoFrame3[701:800])
########################

dataHogares2Filtrado4 <- dataHogares2[, c("DEPTO", "TOTAL_PERS", "PPB02", "P01A01", "P01A02", "P01A03", "P01A04", "P01A06", "P01B01", "P01D01", "P01D02", "P01D16", "P01D17", "P01D22", "P01D34", "P01H02", "P01H07", "P01H08", "P01H09", "P02B01")]
dataHogares2Filtrado4[is.na(dataHogares2Filtrado4)] <- 0
dataHogares2Filtrado4Frame<- as.data.frame(dataHogares2Filtrado4)
reglasdataHogares2Filtrado4Frame <- fim4r(dataHogares2Filtrado4Frame, method = "fpgrowth", target="rules", supp = .3, conf = .6)
inspect(reglasdataHogares2Filtrado4Frame[0:100])
inspect(reglasdataHogares2Filtrado4Frame[101:200])
inspect(reglasdataHogares2Filtrado4Frame[201:300])
inspect(reglasdataHogares2Filtrado4Frame[301:400])
inspect(reglasdataHogares2Filtrado4Frame[401:500])
##########


dataHogares2Filtrado3Guate <- subset(dataHogares2Filtrado3, DEPTO == 1)
dataHogares2Filtrado3Guate$DEPTO<- NULL
dataHogares2FiltradoFrame3Guate<- as.data.frame(dataHogares2Filtrado3Guate)
reglasHogarFpgrowth2_2_3Guate <- fim4r(dataHogares2FiltradoFrame3Guate, method = "fpgrowth", target="rules", supp = .5, conf = .6)
reglasHogarFpgrowth2Frame3Guate <- as(reglasHogarFpgrowth2_2_3Guate, "data.frame")
write_xlsx(reglasHogarFpgrowth2Frame3Guate, "C:\\Users\\Lester\\Documents\\MAESTRIA\\INTO MINERIA DE DATOS\\proyecto_parte1\\ReglasHogar_fpgrowthGT.xlsx")

##########

reglasHogarFpgrowth2 <- fim4r(transactions, method = "fpgrowth", target="rules", supp = .5, conf = .6)
reglasHogarFpgrowth2Frame <- as(reglasHogarFpgrowth2, "data.frame")

#corregido
reglasHogarFpgrowth2_2 <- fim4r(dataHogares2FiltradoFrame, method = "fpgrowth", target="rules", supp = .5, conf = .6)
reglasHogarFpgrowth2Frame_2 <- as(reglasHogarFpgrowth2, "data.frame")




reglasHogarFpgrowth <- fim4r(dataHogares2, method = "fpgrowth", target="rules", supp = .2, conf = .5)
reglasHogarFpgrowth2Frame <- as(reglasHogarFpgrowth2, "data.frame")

inspect(reglasHogarFpgrowth2[0:100])

write_xlsx(reglasHogaresFrame2, "C:\\Users\\Lester\\Documents\\MAESTRIA\\INTO MINERIA DE DATOS\\proyecto_parte1\\ReglasHogar_Apriori2.xlsx")



#kmeans

dataHogaresFiltrado2 <- na.omit(dataHogaresFiltrado)


clusterHogar <- kmeans(dataHogaresFiltrado2, centers=5)

ggplot(dataHogaresFiltrado2, aes(x = P01D16, y = P01A04, color = as.factor(clusterHogar$cluster)))+
  geom_point(alpha = 0.6, size = 2) +
  geom_point(data = as.data.frame(clusterHogar$centers), aes(x=P01D16, y = P01A04), color = "black", size=4, shape=17)+
  labs(title = "Edad vs hogares en casa", color ="Cluster de emigración")+
  theme_minimal()