# para correr el Google Cloud
#   8 vCPU
#  32 GB memoria RAM
# 256 GB espacio en disco

# son varios archivos, subirlos INTELIGENTEMENTE a Kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("primes")
require("lightgbm")


#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento  <- "DA8820"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202101 )
PARAM$input$future        <- c( 202103 )

PARAM$semilla1  <- 306529 
PARAM$semilla2  <- 775163
PARAM$semillas_particion <- 5
PARAM$semillas_modelo    <- 50

PARAM$finalmodel$max_bin           <-     31
PARAM$finalmodel$learning_rate     <-      0.0635203137578911   #0.0142501265
PARAM$finalmodel$num_iterations    <-    91  #615
PARAM$finalmodel$num_leaves        <-   55  #784
PARAM$finalmodel$min_data_in_leaf  <-   4582  #5628
PARAM$finalmodel$feature_fraction  <-      0.347723028834444  #0.8382482539
PARAM$finalmodel$semilla           <- 775163 #(306529, 472993, 669989, 775163, 996689) #reemplazar por las propias semillas


#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
          by= agrupa ]
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
setwd("E:/Exe/FCEN/NextCloud/Documents/Maestría/DMEyF"  )


#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)


#CAMPOS NO-MONETARIOS
require(readODS)

diccionario <- read_ods("./TareasHogar/DiccionarioDatos.ods")

dic<-data.table(diccionario)
camposnomonet <-dic[unidad!= "pesos", campo]


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


#TOMO TODOS LOS ATRIBUTOS MONETARIOS SOBRE LOS CUALES VOY A APLICAR EL RANKING

atributosRank<-setdiff(colnames(dataset), c(camposnomonet,"clase_ternaria","clase_binaria"))


#RETOCAR CON RANK SEGUN MESES SOLO AQUELLAS VARIABLES MONETARIAS

dataset1<-dataset[foto_mes == 202103]
dataset2<-dataset[foto_mes == 202105]


for( campo in atributosRank )
  
{
  dataset1[, paste0("auto_r_", campo, sep = "") := (frankv(dataset1, cols = campo) - 1) / (length(dataset1[, get(campo)]) - 1)] # rankeo entre 0 y 1
  dataset1[, paste0(campo) := NULL]  # elimino atributos nuevos
  
}

for( campo in atributosRank )
  
{
  dataset2[, paste0("auto_r_", campo, sep = "") := (frankv(dataset2, cols = campo) - 1) / (length(dataset2[, get(campo)]) - 1)] # rankeo entre 0 y 1
  dataset2[, paste0(campo) := NULL]  # elimino atributos nuevos
  
}

dataset <- rbind(dataset1,dataset2)

dataset[is.na(dataset)]<-0


#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

dataset[  , ganancia :=  ifelse( clase_ternaria == "BAJA+2", 78000, -2000 ) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "ganancia") )

#--------------------------------------

#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", PARAM$experimento, "/" ), showWarnings = FALSE )
setwd( paste0("./exp/", PARAM$experimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO


dfuturo  <- dataset[ foto_mes== PARAM$input$future ]

#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% PARAM$input$training, train  := 1L ]

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )

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

for( semilla_particion in  ksemillas_particion )
{
  #particiono estratificdamente 50/50 los datos del futuro
  particionar( dfuturo, 
               division= c(1,1),   #particion 50% / 50%
               agrupa= "clase_ternaria",
               seed= semilla_particion )


  for( semilla_modelo  in  ksemillas_modelo )
  {
    #los campos que se van a utilizar
    campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "ganancia", "train", "fold") )


    #genero el modelo
    #estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
    modelo  <- lgb.train( data= dtrain,
                          param= list( objective=          "binary",
                                       max_bin=            PARAM$finalmodel$max_bin,
                                       learning_rate=      PARAM$finalmodel$learning_rate,
                                       num_iterations=     PARAM$finalmodel$num_iterations,
                                       num_leaves=         PARAM$finalmodel$num_leaves,
                                       min_data_in_leaf=   PARAM$finalmodel$min_data_in_leaf,
                                       feature_fraction=   PARAM$finalmodel$feature_fraction,
                                       seed=               semilla_modelo
                                      )
                        )


    #aplico el modelo a los datos nuevos
    prediccion  <- predict( modelo, 
                            data.matrix( dfuturo[, campos_buenos, with=FALSE ]) )

    #genero la tabla de entrega
    tb_entrega  <- dfuturo[ , list( fold, ganancia ) ]
    tb_entrega[  , prob := prediccion ]
    
    #ordeno por probabilidad descendente
    setorder( tb_entrega, -prob )

    tb_entrega[ , x := .I ]
    tb_entrega[ , gan_acum := cumsum( ganancia ) ]
    tb_entrega[ fold==1,  gan_public  :=  2*cumsum( ganancia ) ]
    tb_entrega[ fold==2,  gan_private :=  2*cumsum( ganancia ) ]


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
