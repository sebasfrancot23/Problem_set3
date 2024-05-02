###################################
#
# Programación problem set 3
# Importación y limpieza de datos.
#
#######################


# Preparación del ambiente ------------------------------------------------
rm(list=ls())

libraries = c("tidyverse", "skimr", "visdat", "stringr") 

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
  
  #Nos quedamos con las variables de interés.
  x = x %>% select(-city, -month, -year, -bedrooms) %>%
    filter(operation_type == "Venta")
  
  #Hay algunos missings en description, esos sí toca quitarlos para poder
  #sacar información de ese string.
  x = x[!is.na(x$description), ]
  return(x)
}


aux = Limpieza_bases(aux)

#Para completar la información de las habitaciones, se extrae esta información 
#de la descripción de las propiedades.

#Primero busco por el número de habitaciones.
patron = "\\b(\\d+)\\s*((?:[Hh]ab\\w*|[Aa]lcoba\\w*|[Dd]ormitorio\\w*))\\b"

aux = aux %>% mutate(piezas_aux = ifelse(str_detect(description, patron),
                                         str_extract(description, patron)
                                         ,NA)) %>% #Ahora nos quedamos con el número.
  mutate(piezas_aux = ifelse(!is.na(piezas_aux),
                             as.numeric(substr(piezas_aux,1,1)),
                             NA)) %>%
  mutate(rooms = ifelse(is.na(rooms),
                        piezas_aux, rooms))

#Qué pasa si el número de alcobas lo escribió con una palabra?
patron = "\\b([^\\d\\s]+)\\s*(?:[Hh]ab\\w*|[Aa]lcoba\\w*|[Dd]ormitorio\\w*)\\b"

aux = aux %>% mutate(piezas_aux = ifelse(str_detect(description, patron),
                                         str_extract(description, patron)
                                         ,NA))
#Nos quedamos con la primera palabra.
lista = str_split(aux$piezas_aux, " ")

#A pie comparamos la primera palabra y la volvemos un número.
for (i in 1:length(lista)){
  if (is.na(lista[[i]][1])){
    next
  } else if (lista[[i]][1]== "una"){
    aux[i,"rooms"] = 1
  } else if (lista[[i]][1]== "un"){
    aux[i,"rooms"] = 1
  } else if (lista[[i]][1]== "dos"){
    aux[i,"rooms"] = 2
  } else if (lista[[i]][1]== "tres"){
    aux[i,"rooms"] = 3
  } else if (lista[[i]][1]== "cuatro"){
    aux[i,"rooms"] = 4
  } else if (lista[[i]][1]== "cinco"){
    aux[i,"rooms"] = 5
  } else if (lista[[i]][1]== "seis"){
    aux[i,"rooms"] = 6
  } else if (lista[[i]][1]== "siete"){
    aux[i,"rooms"] = 7
  } else if (lista[[i]][1]== "ocho"){
    aux[i,"rooms"] = 8
  } else if (lista[[i]][1]== "nueve"){
    aux[i,"rooms"] = 9
  } else if (lista[[i]][1]== "diez"){
    aux[i,"rooms"] = 10
  } 
}


vis_dat(aux)



# Patrón ajustado
patron <- "\\b([^\\d\\s]+)\\s*(?:[Hh]ab\\w*|[Aa]lcoba\\w*|[Dd]ormitorio\\w*)\\b"



# Crear el data frame
df <- data.frame(Texto = c("3 habs", 
                           "tu madre en chanclas", 
                           "Nada", 
                           "nada nada dos alcobas nada nada adios",
                           "probandomaskaskjjkaf 4 habs laklñksd"),
                 stringsAsFactors = FALSE)

# Crear la nueva columna utilizando mutate de dplyr
df <- df %>% 
  mutate(Nueva_Columna = ifelse(str_detect(Texto, patron),
                                str_extract(Texto, patron),
                                NA))

# Imprimir el data frame resultante
print(df)














