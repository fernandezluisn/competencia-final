setwd( "~/buckets/b1/exp/competencia final" )
remove(list=ls())
gc()

library(data.table)
library(dplyr)
library(beepr)
require("Rcpp")

require("ranger")

fread("../paso2 drifting/dataset.csv.gz")->data_orig
names(data_orig)->vars_orig
remove(data_orig)
gc()

#### 6 m####
fread("~/buckets/b1/exp/competencia final/dataset_5m.csv.gz")->data_6


#### dataset 3 ####
fread("~/buckets/b1/exp/competencia final/dataset_3m.csv.gz")->data_3
names(data_3)->vars3
setdiff(vars3,vars_orig)->vars_join3


c("numero_de_cliente", "foto_mes",'clase_ternaria',vars_join3)->vars_join3

data_3[,..vars_join3]->data_3b
remove(data_3)
gc()
data_3b[data_6,,on=c("numero_de_cliente", "foto_mes",'clase_ternaria')]->baseF
remove(data_3b,data_6)
gc()
#### dataset lag1 ####
fread("~/buckets/b1/exp/competencia final/dataset_4m.csv.gz")->data_3
names(data_3)->vars3
setdiff(vars3,vars_orig)->vars_join3


c("numero_de_cliente", "foto_mes",'clase_ternaria',vars_join3)->vars_join3

data_3[,..vars_join3]->data_3b
remove(data_3)
gc()
data_3b[baseF,,on=c("numero_de_cliente", "foto_mes",'clase_ternaria')]->baseF

remove(data_3b)




fwrite( baseF,
        "base_tendencias.csv.gz",
        logical01= TRUE,
        sep= "," )




