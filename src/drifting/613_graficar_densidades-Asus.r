#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")


#------------------------------------------------------------------------------

graficar_campo  <- function( campo, campo_clase, valores_clase )
{

  #quito de grafico las colas del 5% de las densidades
  qA  <- quantile(  dataset[ foto_mes==202101 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  qB  <- quantile(  dataset[ foto_mes==202102 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  qC  <- quantile(  dataset[ foto_mes==202103 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  qD  <- quantile(  dataset[ foto_mes==202104 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )

  xxmin  <- pmin( qA[[1]], qB[[1]],qC[[1]], qD[[1]])
  xxmax  <- pmax( qA[[2]], qB[[2]],qC[[2]], qD[[2]])

  densidad_A  <- density( dataset[ foto_mes==202101 & get(campo_clase) %in% valores_clase, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )

  densidad_B  <- density( dataset[ foto_mes==202102 & get(campo_clase) %in% valores_clase, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )
  densidad_C  <- density( dataset[ foto_mes==202103 & get(campo_clase) %in% valores_clase, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )
  
  densidad_D  <- density( dataset[ foto_mes==202104 & get(campo_clase) %in% valores_clase, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )
  
  

  plot( densidad_A,
        col="blue",
        xlim= c( xxmin, xxmax ),
        ylim= c( 0, pmax( max(densidad_A$y), max(densidad_B$y),max(densidad_C$y) ) ),
        main= paste0( campo, ",   ", campo_clase, " in ",  paste( valores_clase,collapse=",")) 
      )

  lines(densidad_B, col="red", lty=2)
  lines(densidad_C, col="green", lty=2)
  lines(densidad_D, col="black", lty=2)
  
  legend(  "topright",  
           legend=c("202101", "202102","202103","202104"),
           col=c("blue", "red","green","maroon"), lty=c(1,2))

}
#------------------------------------------------------------------------------
#Aqui comienza el programa
setwd("E:/Exe/FCEN/NextCloud/Documents/Maestría/DMEyF")

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/competencia2_2022.csv.gz")


dataset  <- dataset[  foto_mes %in% c( 202101, 202102,202103,202104 ) ]

#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ ((foto_mes==202101)|(foto_mes==202102)|(foto_mes==202103)|(foto_mes==202104)), 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]


dataset[,caja_ahorro_total := mcaja_ahorro + mcaja_ahorro_dolares + mcaja_ahorro_adicional + mcaja_ahorro_adicional]
dataset[,saldo_total := mcuenta_corriente + mcaja_ahorro + mcaja_ahorro_dolares + mcaja_ahorro_adicional + mcaja_ahorro_adicional]
dataset[,inversiones_total := mplazo_fijo_dolares+mplazo_fijo_pesos + minversion1_pesos + minversion1_dolares + minversion2]
dataset[mcuenta_corriente<0,descubierto:= -1*mcuenta_corriente ]
dataset[,prestamos := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios]
dataset[,consumo_tarjeta_total:= mtarjeta_visa_consumo+mtarjeta_master_consumo]
dataset[,financiamiento_corto:= descubierto + consumo_tarjeta_total]
dataset[,financiamiento_total := financiamiento_corto + prestamos]


dataset[,inversiones_saldo_total:= inversiones_total/saldo_total]
dataset[,inversiones_caja_ahorro_total:= inversiones_total/caja_ahorro_total]

dataset[,prestamos_saldo_total := prestamos / saldo_total ]
dataset[,prestamos_caja_ahorro := prestamos / caja_ahorro_total] 

dataset[,financiamiento_corto_saldo_total := financiamiento_corto / saldo_total ]
dataset[,financiamiento_corto_caja_ahorro := financiamiento_corto / caja_ahorro_total] 

dataset[,financiamiento_corto_saldo_total := financiamiento_corto / saldo_total ]
dataset[,financiamiento_corto_caja_ahorro := financiamiento_corto / caja_ahorro_total] 

dataset[,financiamiento_total_saldo_total := financiamiento_total / saldo_total]
dataset[,financiamiento_total_ahorro := financiamiento_total / caja_ahorro_total]

dataset[,inversion_antiguedad:= inversiones_total /cliente_antiguedad]
dataset[,antiguedad_relativa := (cliente_antiguedad/12)/cliente_edad]
dataset[,seguros:= cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales]



# Entreno el modelo
# utilizo los mejores hiperparametros encontrados en una Bayesian Optimizationcon 5-fold Cross Validation
modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",
                 data=      dataset[ ((foto_mes==202101)|(foto_mes==202102)|(foto_mes==202103)) ],  #los datos donde voy a entrenar
                 xval=         0,
                 cp=           -1.45,
                 minsplit=    1180,
                 minbucket=     275,
                 maxdepth=      8)


campos_modelo  <- names( modelo$variable.importance )
campos_buenos  <- c( campos_modelo,  setdiff( colnames(dataset), campos_modelo ) )
campos_buenos  <-  setdiff(  campos_buenos,  c( "foto_mes","clase_ternaria","clase_binaria" ) )


dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/DR6130/", showWarnings = FALSE )
setwd("./exp/DR6130/")



# pdf("densidades_01_05_SINBAJA2Camposnuevos.pdf")

for( campo in  campos_buenos )
{
  cat( campo, "  " )
  
  graficar_campo( campo, "clase_ternaria", c( "BAJA+1", "BAJA+2", "CONTINUA" ) )
  graficar_campo( campo, "clase_ternaria", c( "BAJA+1", "BAJA+2" ) )
  #graficar_campo( campo, "clase_ternaria", c( "BAJA+2" ) )
  graficar_campo( campo, "clase_ternaria", c( "BAJA+1" ) )
  graficar_campo( campo, "clase_ternaria", c( "CONTINUA" ) )
}

dev.off()

for (i in colnames(dataset)) {
  
  if (dataset[i <0,.N]>0){

  }
}

campos_procesar <- setdiff( colnames(dataset), "clase_ternaria" )

for( campo in campos_procesar )
{
  if(  dataset[ get(campo) < 0, .N ]  > 0 ) {
    dataset[   , paste0( campo, "_neg" ) := ifelse( get(campo)< 0, get(campo), 0 ) ]
    dataset[   , paste0( campo, "_pos" ) := ifelse( get(campo)> 0, get(campo), 0 ) ]
  }
}

frank(dataset, col="ctrx_quarter")
dataset$mcuenta_corriente
frank(dataset, col="mcuenta_corriente")
(frank(dataset, col="mcuenta_corriente")-1)/.N
frank(dataset$mcuenta_corriente)-1/.N


require(readODS)

diccionario <- read_ods("./TareasHogar/DiccionarioDatos.ods")
diccionario[[3]]
colnames(diccionario[unidad])

camposnomonet <-diccionario[unidad !="pesos",campo]

camposnomonet


camposnomonetr<- setdiff(camposnomonet,c("ctrx_quarter","cprestamos_personales","cpayroll_trx","cproductos","ccomisiones_mantenimiento","ccajas_consultas","ccajas_otras")) #cprestamos_personales

dataset[,sapply(.SD,is.na)]

sapply(dataset, function(x) sum(is.na(x)))

dataset[is.na(dataset)]<-0

(frank(dataset$ctrx_quarter,ties.method="dense")-1)/(nrow(dataset)-1)

dataset[foto_mes==202103,ctrx_quarter]

nrow(dataset)



dataset[foto_mes==202103, .SD]


dtrain[, paste0("rank_", var.monet$campo) := lapply(.SD, frank,ties.method="dense"),
       .SDcols = var.monet$campo]

camposarankear<- setdiff( colnames(dataset),c(camposnomonetr,"clase_binaria","clase_ternaria"))








dataset[foto_mes==202103   , paste0( campo, "rank" ) := lapply(get(campo), frank,ties.method="dense") ]
    
  





#HACER LOOP CON RANKING A TODAS LAS COLUMNAS MONETARIAS.



campos_procesar <- setdiff(colnames(dataset), c("clase_ternaria","clase_binaria" ))

campos_procesar

campos_dd <- c("mcuentas_saldo", "mcuenta_corriente", "prestamos_saldo_total", "mprestamos_personales", "mcomisiones", "mcomisiones_otras", "ccomisiones_otras",
               "mpasivos_margen", "ccajas_otras", "mpayroll", "mrentabilidad_annual", "mcaja_ahorro", "Master_mfinanciacion_limite", "prestamos_caja_ahorro", "Visa_mpagominimo",
"Visa_msaldototal", "Visa_msaldopesos", "mtarjeta_visa_consumo", "consumo_tarjeta_total", "Visa_mpagospesos","Visa_mfinanciacion_limite", "Visa_Finiciomora","financiamiento_total_ahorro")


colnames(dataset)

campos_dd

dataset[foto_mes==202104]

dataset1  <- dataset[foto_mes==202103,.SD]

dataset[foto_mes==202103]
               
for( campo in campos_dd )
{
 
    dataset1[foto_mes==202103   , paste0( campo, "_rank" ) := lapply(get(campo), frank,ties.method="dense")]
                                                                    
  
      }

a<- nrow(dataset[foto_mes==202103])
b<- nrow(dataset[foto_mes==202105])

dataset[foto_mes==202103,mcuentas_saldo,mcuentas_saldo_rank:= (frank(dataset$mcuentas_saldo,ties.method="dense")-1)/(a-1)]
dataset[foto_mes==202105,mcuentas_saldo_rank:= (frank(dataset$mcuentas_saldo,ties.method="dense")-1)/(a-1)]

dataset$mcuentas_saldo_rank

setDT(diccionario)
