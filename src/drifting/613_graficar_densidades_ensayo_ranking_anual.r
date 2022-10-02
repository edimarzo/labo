#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")


#------------------------------------------------------------------------------

graficar_campo  <- function( campo, campo_clase, valores_clase )
{

  #quito de grafico las colas del 5% de las densidades
  qA  <- quantile(  dataset[ foto_mes==202103 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  qB  <- quantile(  dataset[ foto_mes==202105 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )

  xxmin  <- pmin( qA[[1]], qB[[1]] )
  xxmax  <- pmax( qA[[2]], qB[[2]] )

  densidad_A  <- density( dataset[ foto_mes==202103 & get(campo_clase) %in% valores_clase, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )

  densidad_B  <- density( dataset[ foto_mes==202105 & get(campo_clase) %in% valores_clase, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )

  plot( densidad_A,
        col="blue",
        xlim= c( xxmin, xxmax ),
        ylim= c( 0, pmax( max(densidad_A$y), max(densidad_B$y) ) ),
        main= paste0( campo, ",   ", campo_clase, " in ",  paste( valores_clase,collapse=",")) 
      )

  lines(densidad_B, col="red", lty=2)
  
  legend(  "topright",  
           legend=c("202003", "202005"),
           col=c("blue", "red"), lty=c(1,2))

}
#------------------------------------------------------------------------------
#Aqui comienza el programa
setwd("E:/Exe/FCEN/NextCloud/Documents/Maestría/DMEyF")

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/competencia2_2022.csv.gz")

dataset  <- dataset[  foto_mes %in% c( 202103, 202105 ) ]

#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202103, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

dataset[ foto_mes==202105, 
         clase_ternaria :=  "CONTINUA" ]


##------------------------------------------------------------------------------


#NUEVOS FEATURES

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


#DIVISIÓN DATASET Y ELIMINACION DE COLUMNAS MODIFICADAS

campos_procesar <- setdiff( colnames(dataset), "clase_ternaria" )

for( campo in campos_procesar )
{
  if(  dataset[ get(campo) < 0, .N ]  > 0 ) {
    dataset[   , paste0( campo, "_neg" ) := ifelse( get(campo)< 0, get(campo), 0 ) ]
    dataset[   , paste0( campo, "_pos" ) := ifelse( get(campo)> 0, get(campo), 0 ) ]
    dataset[, paste0(campo) := NULL] 
  }
}
dataset[is.na(dataset)]<-0

retocarR<-c("financiamiento_total_saldo_total_neg","financiamiento_corto_saldo_total_neg","mcuentas_saldo_neg","saldo_total_neg","descubierto","mcuenta_corriente_neg","prestamos","mprestamos_personales",
            "prestamos_saldo_total_neg","mcomisiones_otras_pos","mcomisiones_pos","mpasivos_margen_pos", "ccomisiones_mantenimiento", "mpayroll",
            "Master_mfinanciacion_limite","prestamos_caja_ahorro_pos","Visa_mpagominimo","Visa_msaldototal_pos","financiamiento_corto", "financiamiento_corto_caja_ahorro_pos","mrentabilidad_annual_neg",
            "financiamiento_total_saldo_total_pos","mcaja_ahorro_pos","Visa_msaldopesos_pos","financiamiento_corto_saldo_total_pos","prestamos_saldo_total_pos","mtarjeta_visa_consumo",
            "consumo_tarjeta_total","mcuentas_saldo_pos","Visa_mfinanciacion_limite","chomebanking_transacciones",
            "financiamiento_total_ahorro_pos", "mactivos_margen_pos","financiamiento_total","mrentabilidad_annual_pos", "Visa_mconsumospesos_pos","Visa_mconsumototal_pos",
            "mtransferencias_emitidas","mtarjeta_master_consumo","mprestamos_hipotecarios","mpasivos_margen_neg","inversiones_saldo_total_neg","caja_ahorro_total_neg",
            "mcaja_ahorro_dolares_pos","Master_mpagominimo","Master_msaldototal_pos","Master_mconsumospesos_pos","Master_mconsumototal_pos",
            "Master_msaldopesos_pos","mcuenta_debitos_automaticos") #p64

#RETOCAR CON RANK SEGUN MESES SOLO AQUELLAS VARIABLES MONETARIAS

dataset1<-dataset[foto_mes == 202103]
dataset2<-dataset[foto_mes == 202105]


for( campo in retocarR )
  
{
  dataset1[, paste0("auto_r_", campo, sep = "") := (frankv(dataset1, cols = campo) - 1) / (length(dataset1[, get(campo)]) - 1)] # rankeo entre 0 y 1
  dataset1[, paste0(campo) := NULL]  # elimino atributos nuevos
  
}

for( campo in retocarR )
  
{
  dataset2[, paste0("auto_r_", campo, sep = "") := (frankv(dataset2, cols = campo) - 1) / (length(dataset2[, get(campo)]) - 1)] # rankeo entre 0 y 1
  dataset2[, paste0(campo) := NULL]  # elimino atributos nuevos
  
}


#https://www.r-bloggers.com/2021/12/how-to-use-the-scale-function-in-r/
#scale

#retocarZ <- c("ccajas_consultas","ccajas_otras","ccomisiones_otras","ccajas_depositos","Master_fultimo_cierre")


#for( campo in retocarZ )

#{
#dataset1[, paste0("auto_z_", campo, sep = "") := scale(get(campo),center=TRUE, scale = TRUE)] # rankeo entre 0 y 1
#dataset1[, paste0(campo) := NULL]  # elimino atributos nuevos

#}


#for( campo in retocarZ )

#{
#dataset2[, paste0("auto_z_", campo, sep = "") := scale(get(campo),center=TRUE, scale = TRUE)] # rankeo entre 0 y 1
#dataset2[, paste0(campo) := NULL]  # elimino atributos nuevos

#}





dataset <- rbind(dataset1,dataset2)




# Entreno el modelo
# utilizo los mejores hiperparametros encontrados en una Bayesian Optimizationcon 5-fold Cross Validation
modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",
                 data=      dataset[ foto_mes==202103 ],  #los datos donde voy a entrenar
                 xval=         0,
                 cp=           -0.69,
                 minsplit=    870,
                 minbucket=     9,
                 maxdepth=      9)


campos_modelo  <- names( modelo$variable.importance )
campos_buenos  <- c( campos_modelo,  setdiff( colnames(dataset), campos_modelo ) )
campos_buenos  <-  setdiff(  campos_buenos,  c( "foto_mes","clase_ternaria","clase_binaria" ) )


dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/DR6130/", showWarnings = FALSE )
setwd("./exp/DR6130/")


dataset[is.na(dataset)]<-0
pdf("03_05_solo_rankanual_v5.pdf")

for( campo in  campos_buenos )
{
  cat( campo, "  " )
  
  graficar_campo( campo, "clase_ternaria", c( "BAJA+1", "BAJA+2", "CONTINUA" ) )
  #graficar_campo( campo, "clase_ternaria", c( "BAJA+1", "BAJA+2" ) )
  #graficar_campo( campo, "clase_ternaria", c( "BAJA+2" ) )
  #graficar_campo( campo, "clase_ternaria", c( "BAJA+1" ) )
  #graficar_campo( campo, "clase_ternaria", c( "CONTINUA" ) )
}

dev.off()

