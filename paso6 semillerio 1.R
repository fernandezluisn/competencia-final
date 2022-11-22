#con 256 gb anda

#¿Usar más meses?

#limpio lamemoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

setwd("~/buckets/b1/exp/competencia final/")

require("data.table")
require("lightgbm")
library(dplyr)


#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento  <- "resultados"
PARAM$nombre<-"rankeada"
PARAM$input$dataset       <- "base_FINAL2.csv.gz"


PARAM$input$training      <- c( 201912,201911,201910,201909,201908,201907,201906,201905,201904,201903,201902,201901,
                                202001,202002,202011,202012,
                                202101,202102,202103, 202104, 202105,202106,202107 )
PARAM$input$future        <- c( 202109 )

BO_log <- read.csv("~/buckets/b1/exp/competencia final/BO_log rank.csv", sep=";")

maximo=max(BO_log$ganancia)
BO_log<-BO_log %>% filter(ganancia==maximo)

PARAM$finalmodel$max_bin           <-     31
PARAM$finalmodel$learning_rate     <-      BO_log$learning_rate   #0.0142501265 VER!!!!
PARAM$finalmodel$num_iterations    <-    BO_log$num_iterations  #615
PARAM$finalmodel$num_leaves        <-   BO_log$num_leaves  #784
PARAM$finalmodel$min_data_in_leaf  <-    BO_log$min_data_in_leaf #5628
PARAM$finalmodel$feature_fraction  <-      BO_log$feature_fraction  #0.8382482539

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa


#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)

#aplico el modelo a los datos sin clase
dapply  <- dataset[ foto_mes== PARAM$input$future ]



#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#--------------------------------------


#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% PARAM$input$training, train  := 1L ]



table(dataset$train)

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( paste0("~/buckets/b1/exp/competencia final/",PARAM$experimento, "/" ), showWarnings = FALSE )
setwd( paste0("~/buckets/b1/exp/competencia final/",PARAM$experimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO



#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )

if( !file.exists( paste0("predicciones_",PARAM$nombre,".csv" )) ) {
  print("Se crea")
  nInicial=1
}else{
  print("ya existe")
  tb_entrega<-read.csv(paste0("predicciones_",PARAM$nombre,".csv"))
  nInicial=ncol(tb_entrega)-1
}

for(semillaActual in nInicial:40){
  
  print(paste0("SEMILLA ACTUAL ",semillaActual ))
  #genero el modelo
  #estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
  modelo  <- lgb.train( data= dtrain,
                        param= list( objective=          "binary",
                                     max_bin=            PARAM$finalmodel$max_bin,
                                     learning_rate=      PARAM$finalmodel$learning_rate,
                                     num_iterations=     PARAM$finalmodel$num_iterations,
                                     num_leaves=         PARAM$finalmodel$num_leaves,
                                     max_depth=  -1,                 # -1 significa no limitar,  por ahora lo dejo fijo
                                     min_gain_to_split= 0.0,         #por ahora, lo dejo fijo
                                     min_sum_hessian_in_leaf= 0.001, #por ahora, lo dejo fijo
                                     lambda_l1= 0.0,                 #por ahora, lo dejo fijo
                                     lambda_l2= 0.0, 
                                     min_data_in_leaf=   PARAM$finalmodel$min_data_in_leaf,
                                     feature_fraction=   PARAM$finalmodel$feature_fraction,
                                     seed=               semillaActual
                        )
  )
  
  #aplico el modelo a los datos nuevos
  prediccion  <- predict( modelo, 
                          data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )
  
  
  
  if(semillaActual==1){
    #genero la tabla de entrega
    tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
    tb_entrega[  , prob := prediccion ]
  }else{
    tb_entrega[,paste0("prob", semillaActual)]<-prediccion
  }
  
  fwrite( tb_entrega, 
          file= paste0("predicciones_",PARAM$nombre,".csv") ,
          sep= "," )
  
  print("SE guardó")
  print("SE guardó")
  print("")
  print("")
  
  
}



