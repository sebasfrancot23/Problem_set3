###
#
# Programación problem set 3
# Predicción del precio de venta.
#
###

# Preparación del ambiente ------------------------------------------------
rm(list=ls())

libraries = c("tidyverse", "stats", "stargazer", "caret", "glmnet", "xtable",
              "rpart.plot") 

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



# MAE de los modelos. -----------------------------------------------------
#Se guardan los MAE del precio de venta.

#Para el RF toca a pie
#aux = (Pred_aux$train_final.Ingreso_disponible-Pred_aux$Ingreso_pred_RF)^2 |>
#  mean() |> sqrt()

MAE = data.frame("Modelo" = c("Regresión"),
                  "MAE" = c(lm_normal_MAE/1e6))


xtable(MAE)
saveRDS(MAE, paste0(path,"Stores/MAE_precios.rds"))














































