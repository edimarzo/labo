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
PARAM$experimento <- "ZZ9420_Multi_EXP2"
PARAM$exp_input <- "HT9420_TRAINING2019202"
TS <- "TS9320_TRAINING20192021_EXP2"
PARAM$modelos  <- 1
# FIN Parametros del script

ksemilla  <- 936659

#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

setwd( "~/buckets/b1" )


base_dir <- "~/buckets/b1/"

#creo la carpeta donde va el experimento
dir.create( paste0( base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

##leo la salida de la optimizaciob bayesiana
#arch_log  <- paste0( base_dir, "exp/", PARAM$exp_input, "/BO_log.txt" )
#tb_log  <- fread( arch_log )
#setorder( tb_log, -ganancia )

#leo el nombre del expermento de la Training Strategy
arch_TS  <- paste0( base_dir, "exp/", PARAM$exp_input, "/TrainingStrategy.txt" )
TS  <- readLines( arch_TS, warn=FALSE )

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
  
  
  PARAM$semillas_particion <- 2
  PARAM$semillas_modelo    <- 2
  
  
  #genero dos vectores de semillas
  primos  <- generate_primes(min=100000, max=1000000)  #genero TODOS los numeros primos entre 100k y 1M
  set.seed( PARAM$semilla1) #seteo la semilla que controla al sample de los primos
  ksemillas_modelo  <- sample(primos)[ 1:PARAM$semillas_modelo ]   #me quedo con PARAM$semillerio primos al azar
  set.seed( PARAM$semilla2 ) #seteo la semilla que controla al sample de los primos
  ksemillas_particion  <- sample(primos)[ 1:PARAM$semillas_particion ]   #me quedo con PARAM$semillerio primos al azar
  
  
  tb_optimos  <- data.table(  particion=integer(),
                              semilla= integer(),
                              public_corte= numeric(),
                              public_gan=  numeric(),
                              private_corte= numeric(),
                              private_gan=  numeric() )
  
  tb_cortes  <- data.table(  particion=integer(),
                             semilla= integer(),
                             corte= integer(),
                             ganancia= numeric(),
                             public_gan=  numeric(),
                             private_gan= numeric() )
  
  
  #donde voy a grabar todo
  pdf( "KaggleHack.pdf" )
  
  
  
  
  
  
  
  for( semilla_modelo  in  ksemillas_modelo )
  {

  #Utilizo la semilla definida en este script
  parametros$seed  <- semilla_modelo
  
  #genero el modelo entrenando en los datos finales
  set.seed( parametros$seed )
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
                        ".txt" ),
          sep= "\t" )


  #genero la prediccion, Scoring
  prediccion  <- predict( modelo_final,
                          data.matrix( dfuture[ , campos_buenos, with=FALSE ] ) )
  
  

  tb_prediccion  <- dfuture[  , list( numero_de_cliente, foto_mes ) ]
  tb_prediccion[ , prob := prediccion ]
  
  
  #Tabla de optimos
  gan_public_mejor <- tb_entrega[ fold==1 , max(gan_public, na.rm=TRUE)  ]
  public_x_mejor   <- tb_entrega[ gan_public==gan_public_mejor,  mean(x) ]
  
  gan_private_mejor <- tb_entrega[ fold==2 , max(gan_private, na.rm=TRUE)  ]
  private_x_mejor   <- tb_entrega[ gan_private==gan_private_mejor,  mean(x) ]
  
  tb_optimos  <- rbind( tb_optimos,
                        list( semilla_particion,
                              semilla_modelo,
                              public_x_mejor,
                              gan_public_mejor,
                              private_x_mejor,
                              gan_private_mejor ) )
  
  fwrite( tb_optimos,
          file= "tb_optimos.txt",
          sep= "\t" )
  
  #Tabla de cortes
  for( corte in seq( from=2000, to=20000, by=100 ) )
  {
    total    <- tb_entrega[ x <= corte,  sum( ganancia,    na.rm=TRUE ) ]
    public   <- tb_entrega[ x <= corte & fold==1, 2*sum( ganancia, na.rm=TRUE ) ]
    private  <- tb_entrega[ x <= corte & fold==2, 2*sum( ganancia, na.rm=TRUE ) ]
    
    tb_cortes  <-  rbind( tb_cortes,
                          list( semilla_particion,
                                semilla_modelo,
                                corte,
                                total,
                                public,
                                private ) )
  }
  
  fwrite( tb_cortes,
          file= "tb_cortes.txt",
          sep= "\t" )
  
  
  xtope  <- 18000
  
  plot( x= tb_entrega[ 1:xtope, x],
        y= tb_entrega[ 1:xtope, gan_acum],
        main=  paste0( "Curvas Ganancia, particion: ", semilla_particion ),
        xlab= "Envios",
        ylab= "Ganancia",
        ylim= c(1, 28000000 ),
        col= "black",
        type= "l",
  )
  
  lines( x= tb_entrega[ x < xtope & fold==1, x ] ,
         y= tb_entrega[ x < xtope & fold==1, gan_public ],
         col= "blue",
         pch= 15 
  )
  
  lines( x= tb_entrega[ x < xtope & fold==2, x ] ,
         y= tb_entrega[ x < xtope & fold==2, gan_private ],
         col= "red",
         pch= 15 
  )
  
  
  legend("topleft", 
         legend= c("todo", "Public", "Private"),
         col= c( "black", "blue", "red"),
         lty= c(1,1,1),
         pch= c(20,15,15), 
  )
  }
}


#termino la impresion
dev.off()

