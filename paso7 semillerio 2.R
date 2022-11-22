remove(list = ls())
gc()

setwd("C:/Users/lnfernandez/Desktop/posgrado/DM EyN/competencia final")

library(dplyr)
library(data.table)
library(matrixStats)
semillero_prueba <- read.csv("bases finales/deflacionada.csv")
semillero2 <- read.csv("bases finales/rankeada final.csv")

left_join(semillero_prueba, semillero2, by=c("foto_mes","numero_de_cliente"))->semillero_prueba

rowMaxs(as.matrix(semillero_prueba[,3:ncol(semillero_prueba)]))->semillero_prueba$maximo
rowMeans(semillero_prueba[,3:(ncol(semillero_prueba)-1)])->semillero_prueba$media



experimento<-"semillerio_maximo"
directorio<-"final"

dir.create(directorio)
setwd(directorio)

cant<-dir()



ifelse(semillero_prueba$maximo>=0.049,1,0)->semillero_prueba$predicted
fwrite( semillero_prueba[ , c("numero_de_cliente", "predicted")], 
        file= paste0(  "cerocinco_maximo_",length(cant)+1,".csv" ),
        sep= "," )

table(semillero_prueba$predicted)


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
setorder( semillero_prueba, -media )
cortes <- seq( 10000, 15000, by=500 )
for( envios  in  cortes ){
  semillero_prueba$predicted<-0
  semillero_prueba[ 1:envios, ]$predicted<-1

  fwrite( semillero_prueba[ , c("numero_de_cliente", "predicted")], 
          file= paste0(  "medias_", envios, ".csv" ),
          sep= "," )
}

#### prueba ranking ####

semillero_prueba <- read.csv("bases finales/deflacionada.csv")
semillero2 <- read.csv("C:/Users/lnfernandez/Downloads/exp_competencia final_resultados_predicciones_rankeada (1).csv")

left_join(semillero_prueba, semillero2, by=c("foto_mes","numero_de_cliente"))->semillero_prueba

experimento<-"2semillerios_con_ranking"
directorio<-"2semillerios"
fin1<-ncol(semillero_prueba)
for (col_num in 3:fin1) {
  
  rank(semillero_prueba[,col_num])->semillero_prueba[, paste0("ranking",col_num)]
  
}

rowSums(semillero_prueba[,(fin1+1):ncol(semillero_prueba)])->semillero_prueba$suma

dir.create( directorio,  showWarnings = FALSE ) 
#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
setorder( semillero_prueba, -suma )
cortes <- seq( 4000, 8000, by=250 )
for( envios  in  cortes ){
  semillero_prueba$predicted<-0
  semillero_prueba[ 1:envios, ]$predicted<-1
  
  fwrite( semillero_prueba[ , c("numero_de_cliente", "predicted")], 
          file= paste0(  directorio,"/",experimento, "_", envios, ".csv" ),
          sep= "," )
}

#### ejemplos ####
semillero_prueba <- read.csv("C:/Users/lnfernandez/Downloads/exp_competencia final_resultados_predicciones_rankeada (1).csv")
rowMeans(semillero_prueba[,3:ncol(semillero_prueba)])->semillero_prueba$suma2
ifelse(semillero_prueba$suma2>=0.05,1,0)->semillero_prueba$predicted


fwrite( exp_competencia.final_resultados_predicciones_rankeada[ , c("numero_de_cliente", "predicted")], 
        file= paste0(  directorio,"/rankeada2.csv" ),
        sep= "," )

table(semillero_prueba$predicted)
