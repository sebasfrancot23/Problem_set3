###################################
#
# Programación problem set 3
# Importación y limpieza de datos.
#
#######################


# Preparación del ambiente ------------------------------------------------
rm(list=ls())

libraries = c("tidyverse", "skimr") 

if(length(setdiff(libraries, rownames(installed.packages()))) > 0){
  install.packages(setdiff(libraries, rownames(installed.packages())))
}

invisible(sapply(libraries, require, character.only = TRUE,quietly = TRUE))

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)


# Importación bases de datos ----------------------------------------------


#Se importa la información del train.
train = read.csv(paste0(path, "Stores/Pre_procesadas/train.csv"))

#Se importa la información del test.
test = read.csv(paste0(path, "Stores/Pre_procesadas/test.csv"))

#Por simplicidad para la limpieza y las estadísticas descriptivas se unen en una
#misma base de datos.
train$tipo = "train"
test$tipo = "test" #Para identificar las observaciones
aux = rbind(train, test)

rm(train, test) #Para ahorrar ram

#Se define una función auxiliar para realizar la limpieza de las bases.
Limpieza_bases = function(x){
  
  x = x %>% select(-city, -month, -year, -bedrooms) %>%
    filter(operation_type == "Venta")
  return(x)
}

Limpieza_bases(aux)




















