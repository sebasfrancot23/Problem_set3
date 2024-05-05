###
#
# Programación problem set 3
# Predicciones para kaggle.
#
###

# Preparación del ambiente ------------------------------------------------
rm(list=setdiff(ls(), c("lm_normal", "Carpinteria")))

libraries = c("tidyverse", "stats", "stargazer", "caret", "glmnet", "xtable",
              "rpart.plot") 

if(length(setdiff(libraries, rownames(installed.packages()))) > 0){
  install.packages(setdiff(libraries, rownames(installed.packages())))
}

invisible(sapply(libraries, require, character.only = TRUE,quietly = TRUE))

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)

#Se importa la base
test_db = readRDS(paste0(path,"Stores/Propiedades_final.rds"))
read.csv(paste0(path, "Stores/Pre_procesadas/submission_template.csv")) |> dim()

test_db = Carpinteria(test_db, F)
test_db = test_db %>% mutate(surface_covered = 
                               ifelse(is.na(surface_covered),
                                      mean(surface_covered, na.rm = T), 
                                      surface_covered)) %>%
  mutate(parqueaderos = ifelse(is.na(parqueaderos), mean(parqueaderos, na.rm = T),
                               parqueaderos)) %>%
  mutate(bathrooms = ifelse(is.na(bathrooms), mean(bathrooms, na.rm = T),
                            bathrooms))

#Realizamos las predicciones con los datos de validación para cada modelo.

#Para el modelo de regresión sencillo.
Predicciones = data.frame("property_id" = test_db$property_id,
                          "price" = predict(lm_normal, newdata = test_db))

#Se exportan al CSV.
write.csv(Predicciones, paste0(path, "Stores/Predicciones/regression_lm.csv"),
          row.names = F, sep = ",")

vis_dat(test_db)






























