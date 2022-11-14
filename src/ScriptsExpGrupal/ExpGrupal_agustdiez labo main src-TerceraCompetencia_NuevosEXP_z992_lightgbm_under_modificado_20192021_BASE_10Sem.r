# permite manejar el sampling_total y el undersampling de la clase mayoritaria

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")




# Poner sus semillas
semillas <- c(732497,
              681979,
              281887,
              936659,
              692089)

#Parametros del script
PARAM  <- list()
PARAM$experimento <- "TS9320_TRAINING20192021_Base"

PARAM$exp_input  <- "FE9250_Experimento"

PARAM$future       <- c( 202107 )

#[201901, 202105]

PARAM$final_train  <- c( 201905,201906,201907,201908,201909,201910,201911,201912, 202011, 202012, 202101, 202102, 202103)

PARAM$train$training     <- c( 201905,201906,201907,201908,201909,201910,201911,201912, 202011, 202012, 202101, 202102, 202103)
PARAM$train$validation   <- c( 202104 )
PARAM$train$testing      <- c( 202105 )

PARAM$train$sampling_total  <- 1  # 1.0 significa que NO se hace sampling total,  0.3 es quedarse con el 30% de TODOS los registros
PARAM$train$undersampling_mayoritaria  <- 0.4   # 1.0 significa NO undersampling ,  0.1  es quedarse con el 10% de los CONTINUA

#Atencion, las semillas deben ser distintas
PARAM$train$semilla_sampling  <- 732497
PARAM$train$semilla_under     <- 936659
# FIN Parametros del script


#------------------------------------------------------------------------------

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

setwd( "~/buckets/b1/" )

#Aqui se debe poner la carpeta de la computadora local
#setwd("D:/economia_finanzas")  #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
#esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
dataset_input  <- paste0( "./exp/", PARAM$exp_input, "/dataset_lags2_tendencia3.csv.gz" )
dataset  <- fread( dataset_input )


#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO


setorder( dataset, foto_mes, numero_de_cliente )

#grabo los datos del futuro
# aqui JAMAS se hace sampling
fwrite( dataset[ foto_mes %in% PARAM$future, ],
        file= "dataset_future.csv.gz",
        logical01= TRUE,
        sep= "," )

#grabo los datos donde voy a entrenar los Final Models
# aqui  JAMAS se hace sampling
fwrite( dataset[ foto_mes %in% PARAM$final_train, ],
        file= "dataset_train_final.csv.gz",
        logical01= TRUE,
        sep= "," )



#grabo los datos donde voy a hacer la optimizacion de hiperparametros
set.seed( PARAM$train$semilla_sampling )
dataset[ foto_mes %in% PARAM$train$training , azar_sampling := runif( nrow(dataset[foto_mes %in% PARAM$train$training ]) ) ]


set.seed( PARAM$train$semilla_under )
dataset[ foto_mes %in% PARAM$train$training , azar_under := runif( nrow(dataset[foto_mes %in% PARAM$train$training ]) ) ]

dataset[  , fold_train := 0L ]
dataset[ foto_mes %in% PARAM$train$training & 
           ( azar_sampling <= PARAM$train$sampling_total ) &
           ( azar_under <= PARAM$train$undersampling_mayoritaria | clase_ternaria %in% c( "BAJA+1", "BAJA+2" ) )
         , fold_train := 1L ]

#Se valida SIN sampling de ningun tipo
dataset[  , fold_validate := 0L ]
dataset[ foto_mes %in% PARAM$train$validation, fold_validate := 1L ]

#Se testea SIN sampling de ningun tipo
dataset[  , fold_test := 0L ]
dataset[ foto_mes %in% PARAM$train$testing, fold_test := 1L ]


fwrite( dataset[ fold_train + fold_validate + fold_test >= 1 , ],
        file= "dataset_training.csv.gz",
        logical01= TRUE,
        sep= "," )






##----------------------------------------------------------------
#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU

# ZZ final que necesita de UNDERSAMPLING

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

require("lightgbm")

#Parametros del script
PARAM  <- list()
PARAM$experimento <- "ZZ9920_BASE_10Semillas"
PARAM$exp_input <- "HT9420_TRAINING2019202"
TS <- "TS9320_TRAINING20192021_Base"

PARAM$modelos  <- 1
# FIN Parametros del script

#PARAM$semilla_primos <- 936659
#PARAM$semillerio <- 2 # ¿De cuanto será nuestro semillerio?
# FIN Parametros del script

# genero un vector de una cantidad de PARAM$semillerio  de semillas,  buscando numeros primos al azar
#primos <- generate_primes(min = 100000, max = 1000000) # genero TODOS los numeros primos entre 100k y 1M
#set.seed(PARAM$semilla_primos) # seteo la semilla que controla al sample de los primos
#ksemillas <- sample(primos)[1:PARAM$semillerio] # me quedo con  PARAM$semillerio primos al azar

semillas <- c(732497,
              681979,
              281887,
              936659,
              692089,
              306529,
              472993,
              669989,
              775163,
              996689
)

#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

base_dir <- "~/buckets/b1/"

#creo la carpeta donde va el experimento
dir.create( paste0( base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#leo la salida de la optimizaciob bayesiana
arch_log  <- paste0( base_dir, "exp/", PARAM$exp_input, "/BO_log.txt" )
tb_log  <- fread( arch_log )
setorder( tb_log, -ganancia )

#leo el nombre del expermento de la Training Strategy
#arch_TS  <- paste0( base_dir, "exp/", PARAM$exp_input, "/TrainingStrategy.txt" )
#TS  <- readLines( arch_TS, warn=FALSE )

#leo el dataset donde voy a entrenar el modelo final
arch_dataset  <- paste0( base_dir, "exp/", TS, "/dataset_train_final.csv.gz" )
dataset  <- fread( arch_dataset )

#leo el dataset donde voy a aplicar el modelo final
arch_future  <- paste0( base_dir, "exp/", TS, "/dataset_future.csv.gz" )
dfuture <- fread( arch_future )


#defino la clase binaria
dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 )  ]

campos_buenos  <- setdiff( colnames(dataset), c( "clase_ternaria", "clase01") )


#genero un modelo para cada uno de las modelos_qty MEJORES iteraciones de la Bayesian Optimization
for( i in  1:PARAM$modelos )
{
  for( semilla_modelo  in  semillas )
  {
  
  parametros  <- as.list( copy( tb_log[ i ] ) )
  iteracion_bayesiana  <- parametros$iteracion_bayesiana

  arch_modelo  <- paste0( "modelo_" ,
                          sprintf( "%02d", i ),
                          "_",
                          sprintf( "%03d", iteracion_bayesiana ),
                          ".model" )


  #creo CADA VEZ el dataset de lightgbm
  dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ , campos_buenos, with=FALSE] ),
                          label=   dataset[ , clase01],
                          weight=  dataset[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                          free_raw_data= FALSE
                        )

  ganancia  <- parametros$ganancia

  #elimino los parametros que no son de lightgbm
  parametros$experimento  <- NULL
  parametros$cols         <- NULL
  parametros$rows         <- NULL
  parametros$fecha        <- NULL
  parametros$prob_corte   <- NULL
  parametros$estimulos    <- NULL
  parametros$ganancia     <- NULL
  parametros$iteracion_bayesiana  <- NULL

  if( ! ("leaf_size_log" %in% names(parametros) ) )  stop( "El Hyperparameter Tuning debe tener en BO_log.txt  el pseudo hiperparametro  lead_size_log.\n" )
  if( ! ("coverage" %in% names(parametros) ) ) stop( "El Hyperparameter Tuning debe tener en BO_log.txt  el pseudo hiperparametro  coverage.\n" )
  
  #Primero defino el tamaño de las hojas
  parametros$min_data_in_leaf  <- pmax( 1,  round( nrow(dtrain) / ( 2.0 ^ parametros$leaf_size_log ))  )
  #Luego la cantidad de hojas en funcion del valor anterior, el coverage, y la cantidad de registros
  parametros$num_leaves  <-  pmin( 131072, pmax( 2,  round( parametros$coverage * nrow( dtrain ) / parametros$min_data_in_leaf ) ) )
  cat( "min_data_in_leaf:", parametros$min_data_in_leaf,  ",  num_leaves:", parametros$num_leaves, "\n" )

  #ya no me hacen falta
  parametros$leaf_size_log  <- NULL
  parametros$coverage  <- NULL

  #Utilizo la semilla definida en este script
  #parametros$seed  <- ksemilla
  
  
 
  #genero el modelo entrenando en los datos finales
  set.seed(semilla_modelo)
  modelo_final  <- lightgbm( data= dtrain,
                             param=  parametros,
                             verbose= -100 )

  #grabo el modelo, achivo .model
  lgb.save( modelo_final,
            file= arch_modelo )

  #creo y grabo la importancia de variables
  tb_importancia  <- as.data.table( lgb.importance( modelo_final ) )
  fwrite( tb_importancia,
          file= paste0( "impo_", 
                        sprintf( "%02d", i ),
                        "_",
                        sprintf( "%03d", iteracion_bayesiana ),
                        "_",
                        sprintf( "%03d", semilla_modelo ),
                        ".txt" ),
          sep= "\t" )


  #genero la prediccion, Scoring
  prediccion  <- predict( modelo_final,
                          data.matrix( dfuture[ , campos_buenos, with=FALSE ] ) )

  tb_prediccion  <- dfuture[  , list( numero_de_cliente, foto_mes ) ]
  tb_prediccion[ , prob := prediccion ]


  nom_pred  <- paste0( "pred_",
                       sprintf( "%02d", i ),
                       "_",
                       sprintf( "%03d", iteracion_bayesiana),
                       "_",
                       sprintf( "%03d", semilla_modelo ),
                       ".csv"  )

  fwrite( tb_prediccion,
          file= nom_pred,
          sep= "\t" )


  #genero los archivos para Kaggle
  cortes  <- seq( from=  9000,
                  to=   15000,
                  by=     1000 )


  setorder( tb_prediccion, -prob )

  for( corte in cortes )
  {
    tb_prediccion[  , Predicted := 0L ]
    tb_prediccion[ 1:corte, Predicted := 1L ]

    nom_submit  <- paste0( PARAM$experimento, 
                           "_",
                           sprintf( "%02d", i ),
                           "_",
                           sprintf( "%03d", iteracion_bayesiana ),
                           "_",
                           sprintf( "%03d", semilla_modelo ),
                           "_",
                           sprintf( "%05d", corte ),
                           ".csv" )

    fwrite(  tb_prediccion[ , list( numero_de_cliente, Predicted ) ],
             file= nom_submit,
             sep= "," )

  }


  #borro y limpio la memoria para la vuelta siguiente del for
  rm( tb_prediccion )
  rm( tb_importancia )
  rm( modelo_final)
  rm( dtrain )
  gc()
}
}
