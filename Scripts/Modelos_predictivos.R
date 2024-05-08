###
#
# Programación problem set 3
# Predicción del precio de venta.
#
###

# Preparación del ambiente ------------------------------------------------
rm(list=ls())

libraries = c("tidyverse", "stats", "stargazer", "caret", "glmnet", "xtable",
              "rpart.plot", "sf", "spatialsample", "purrr") 

if(length(setdiff(libraries, rownames(installed.packages()))) > 0){
  install.packages(setdiff(libraries, rownames(installed.packages())))
}

invisible(sapply(libraries, require, character.only = TRUE,quietly = TRUE))

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)

#Se establece una semilla
set.seed(84751309)

#Se importa la base
train_db = readRDS(paste0(path,"Stores/Propiedades_final.rds"))
#train_db = read.csv(paste0(path, "Stores/Pre_procesadas/train.csv"))

#Algo de carpintería antes de empezar para completar la base.
Carpinteria = function(x, train = T){
  
  #Nos quedamos con las observaciones del test o train.
  if (train){
    x = filter(x, tipo == "train")
  } else {
    x = filter(x, tipo == "test")
  }
  
  #Hay algunas variables que son missings todavía, las vamos a imputar por el 
  #promedio de la variable entre cada grupo (tipo_propiedad, bedrooms)
  
  x = x %>% group_by(bedrooms, property_type) %>%
    #Se calculan las medias auxiliares.
    mutate(media_rooms = mean(rooms, na.rm = T)) %>%
    mutate(media_surface = mean(surface_covered, na.rm = T)) %>%
    mutate(media_bathrooms = mean(bathrooms, na.rm = T)) %>%
    mutate(media_parqueadero = mean(parqueaderos, na.rm = T)) %>%
    #Se reemplazan los valores faltantes.
    mutate(rooms = ifelse(is.na(rooms), media_rooms, rooms)) %>%
    mutate(surface_covered = ifelse(is.na(surface_covered), 
                                    media_surface, surface_covered)) %>%
    mutate(bathrooms = ifelse(is.na(bathrooms), media_bathrooms, 
                              bathrooms)) %>%
    mutate(parqueaderos = ifelse(is.na(parqueaderos), 
                                 media_parqueadero, parqueaderos)) %>%
    ungroup()
  
  #Nos quedamos con las variables relevantes.
  x = x[,!grepl("media_", colnames(x))]  
  
  return(x)
}

train_db = Carpinteria(x = train_db)
train_db = train_db[complete.cases(train_db),]

#Ahora se entrenarán distintos modelos de aprendizaje estadístico para
# Modelo LM ---------------------------------------------------------------
#Por simplicidad se define la ecuación en una variable.
model = price ~ surface_covered + rooms + bathrooms + property_type +
  parqueaderos + Colegios + parques + hospitales + turismo + supermercado +
  restaurantes + mall

#Se estima el modelo de regresión.
lm_normal = lm(model, train_db)

#Se calcula el MAE.
lm_normal_pred = predict(lm_normal, newdata = train_db)
lm_normal_MAE = MAE(lm_normal_pred, train_db$price, na.rm= T)


# CV espacial -------------------------------------------------------------

#Los siguientes modelos requerirán calibrar distintos hiperpárametros. Para ello
#podemos recurrir al viejo conocido, CV. Pero en este caso dividiremos la muestra
#en folds según la ubicación geográfica y las distancias para evitar la 
#autocorrelación espacial.

#Especificamos que la base es un obketo sf.
train_db = st_as_sf(train_db, coords = c("lon", "lat"), 
                    crs = 4326)

#Creamos los folds y sus buffers correspondientes. 
folds = 5
CV_bloques = spatial_block_cv(train_db, v = folds) #La dividimos en 5 folds.

#Gráficamente.
png(filename = paste0(path, "Views/CV_espacial.png"),
    width = 1464, height = 750)
autoplot(CV_bloques$splits[[1]]) + theme_bw()
dev.off()

#Carpintería para obtener el id de las observaciones en cada fold.
Indices = list()

#Se llena la lista con los indices de fold
for (i in 1:folds){
  Indices[[i]] = CV_bloques$splits[[i]]$in_id
}

#Especificamos los controles.
control = trainControl(method = "cv", index = Indices)

# Red elástica ------------------------------------------------------------
#Con la misma forma funcional anterior, vamos a aplicar una red elástica 
#para ver si podemos mejorar las predicciones.

#Para los hiperpárametros, nos vamos a apoyar en glmnet para obtener los lambda

X = model.matrix(~ surface_covered + rooms + bathrooms + property_type +
                   parqueaderos + Colegios + parques + hospitales + turismo + 
                   supermercado + restaurantes + mall, train_db)
X = X[,-1]
Y = train_db$price

#Se corre la función glmnet para obtener una grilla de valores de lambda.
aux_lambda_1 <- glmnet(
  x = X,
  y = Y,
  alpha = 0
)

#Para ahorrar ram
rm(X)
rm(Y)

#Se definen los parámetros para realizar la búsqueda del parámetro óptimo.

grilla = expand.grid(alpha = seq(0,1,by = 0.25),
                     lambda = aux_lambda_1$lambda)

#Corremos la CV
Elastic_net = train(model, data = train_db, method = "glmnet",
                    trControl = control, tuneGrid = grilla,
                    metric = "MAE")

#La relación entre el lambda y el alpha gráficamente:
png(filename = paste0(path, "Views/Enet_regresion.png"),
    width = 1464, height = 750)
plot(Elastic_net)
dev.off()

#Los párametros óptimos.
Parametros = Elastic_net$bestTune 


#Las métricas del modelo óptimo.
Enet_matrix = as.data.frame(Elastic_net$results)
Enet_matrix = filter(Enet_matrix, alpha == Parametros[1,1] & 
                       lambda == Parametros[1,2])


# CART --------------------------------------------------------------------
#Le vamos a encimar CV para encontrar el mejor valor de la poda.
tree_cp = train(model, data = train_db,
                method = "rpart",
                trControl = control,
                tuneGrid = expand.grid(cp = seq(0.001, 0.9, length.out = 50)),
                metric = "MAE")

#El mejor valor de poda.
Poda = tree_cp$bestTune$cp

#Para obtener el MAE con el mejor parámetro.
MAE_tree = tree_cp$results[tree_cp$results$cp==Poda, "MAE"]


# Random forest -----------------------------------------------------------
#En RF nos interesarán dos párametros: qué tan profundo haremos el árbol y 
#el número de variables aleatorias que puede escoger en cada división del árbol.

#Esa búsqueda la específicamos en la grilla
Grilla = expand.grid( mtry = c(4:7),
  splitrule = "variance",
  min.node.size = seq(100,1000, length.out = 10))


RF_CV = train(model, data=train_db, method = "ranger", trControl = control,
  tuneGrid=Grilla, ntree = 200, metric = "MAE")

#Se guarda la métrica del MAE para los hiperpárametros óptimos.
MAE_RF = RF_CV$results[which.min(RF_CV$results$MAE),"MAE"]


# Boosting ----------------------------------------------------------------

#Se define una grilla para probar cuál de las combinaciones de hiperpárametros
#otorga la mejor estimación en términos del MAE
Grilla_boost = expand.grid(n.trees= seq(300,1000, length.out = 6), #O el número de aprendizajes de 
                           #boosting (cuántos árboles va a estimar)
                           interaction.depth = c(10:15), #Qué tan profundo serán los
                           #árboles que se estimarán en cada iteración. 
                           shrinkage = 0.01, #Qué tanto vamos a relantizar el
                           #aprendizaje.
                           n.minobsinnode = seq(100,1000, length.out = 10) #Cuántas observaciones debe 
                           #tener un nodo para volverse final.
)

Arbol_boost = train(model, data = train_db, method = "gbm", trControl = control,
                    tuneGrid = Grilla_boost, verbose = F, metric = "MAE")


#El MAE del boosting. 
MAE_boost = Arbol_boost$results[which.min(Arbol_boost$results$MAE),"MAE"]





# MAE e hiperpárametros óptimos. -----------------------------------------------------
#Se guardan los MAE del precio de venta.

#Para el RF toca a pie
#aux = (Pred_aux$train_final.Ingreso_disponible-Pred_aux$Ingreso_pred_RF)^2 |>
#  mean() |> sqrt()

MAE = data.frame("Modelo" = c("Regresión", "Enet", "Árbol_CP", "RF", "Boosting"),
                  "MAE" = c(lm_normal_MAE/1e6, 
                            Enet_matrix[1,"MAE"], 
                            MAE_tree, MAE_RF, MAE_boost))


xtable(MAE)
saveRDS(MAE, paste0(path,"Stores/MAE_precios.rds"))



Hiperparametros = data.frame("Modelo" = c("Enet","Árbol_CP", "RF_CV"),
                             "Alpha" = c(Parametros[1,"alpha"], 
                                         Poda, NA),
                             "Lambda" = c(Parametros[1,"lambda"], NA, NA),
                             "mtry" = c(NA, NA, RF_CV$bestTune[1,"mtry"]),
                             "min.node.size" = c(NA, NA, RF_CV$bestTune[1,"min.node.size"])
)

xtable(Hiperparametros)
saveRDS(Hiperparametros, paste0(path,"Stores/Hiperparametros.rds"))

#Por simplicidad para el latex mando los del boosting aparte.
Boosting = data.frame(Arbol_boost$bestTune)
saveRDS(Boosting, paste0(path,"Stores/Hiperparametros_boosting.rds"))








































