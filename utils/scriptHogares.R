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
install.packages("ggplot2")
library(ggplot2)
install.packages("ggalt")
library(ggalt)


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


dataHogares2Filtrado3 <- dataHogares2[, c("DEPTO", "DOMINIO","POBREZA","TOTAL_PERS", "PPB02", "P01G01","P01A01", "P01A02", "P01A03", "P01A04", "P01A05A", "P01A05B", "P01A05C", "P01A05E", "P01A05F", "P01A06", "P01B01", "P01D01", "P01D02", "P01D16", "P01D17", "P01D22", "P01D34", "P01H02", "P01H07", "P01H08", "P01H09", "P01I01B", "P02B01")]



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



#kmeans

dataHogares2Filtrado3$P01A03[dataHogares2Filtrado3$P01A03 == 98] <- 6
dataHogares2Filtrado3$P01D16[dataHogares2Filtrado3$P01D16 == 98] <- 6


dataHogares2Filtrado3_1 <- na.omit(dataHogares2Filtrado3)


clusterHogar <- kmeans(dataHogares2Filtrado3_1, centers=4)
#1
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

#2
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


#3
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


#4
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




#5
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





