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
  
  #Hay algunos missings en description, esos sí toca quitarlos para poder
  #sacar información de ese string.
  x = x[!is.na(x$description), ]
  
  #Nos quedamos con las variables de interés.
  x = x %>% select(-city, -month, -year) %>%
    filter(operation_type == "Venta") %>% #La variable property_type la volvemos factor
    mutate(property_type = factor(property_type))
  
  #Se completa la información de las habitaciones con la descripción de las
  #propiedades.
  
  #Primero busco por el número.
  patron = "\\b(\\d+)\\s*((?:[Hh]ab\\w*|[Aa]lcoba\\w*|[Dd]ormitorio\\w*))\\b"
  
  x = x %>% mutate(piezas_aux = ifelse(str_detect(description, patron),
                                           str_extract(description, patron)
                                           ,NA)) %>% #Ahora nos quedamos con el número.
    mutate(piezas_aux = ifelse(!is.na(piezas_aux),
                               as.numeric(substr(piezas_aux,1,1)),
                               NA)) %>% #Lo reemplazamos en la original.
    mutate(rooms = ifelse(is.na(rooms),
                          piezas_aux, rooms))
  
  #Qué pasa si el número de alcobas lo escribió con una palabra?
  patron = "\\b([^\\d\\s]+)\\s*(?:[Hh]ab\\w*|[Aa]lcoba\\w*|[Dd]ormitorio\\w*)\\b"
  
  x = x %>% mutate(piezas_aux = ifelse(str_detect(description, patron),
                                           str_extract(description, patron)
                                           ,NA))
  #Nos quedamos con la primera palabra.
  lista = str_split(x$piezas_aux, " ")
  
  #A pie comparamos la primera palabra y la volvemos un número.
  for (i in 1:length(lista)){
    if (is.na(lista[[i]][1])){
      next
    } else if (lista[[i]][1]== "una"){
      x[i,"rooms"] = 1
    } else if (lista[[i]][1]== "un"){
      x[i,"rooms"] = 1
    } else if (lista[[i]][1]== "dos"){
      x[i,"rooms"] = 2
    } else if (lista[[i]][1]== "tres"){
      x[i,"rooms"] = 3
    } else if (lista[[i]][1]== "cuatro"){
      x[i,"rooms"] = 4
    } else if (lista[[i]][1]== "cinco"){
      x[i,"rooms"] = 5
    } else if (lista[[i]][1]== "seis"){
      x[i,"rooms"] = 6
    } else if (lista[[i]][1]== "siete"){
      x[i,"rooms"] = 7
    } else if (lista[[i]][1]== "ocho"){
      x[i,"rooms"] = 8
    } else if (lista[[i]][1]== "nueve"){
      x[i,"rooms"] = 9
    } else if (lista[[i]][1]== "diez"){
      x[i,"rooms"] = 10
    } 
  }
  
  x = select(x, -piezas_aux)
  
  #Ahora completamos la información de los baños. Fíjate que los baños no aparecen con
  #la ñ.
  
  #Primero busco por el número.
  patron = "\\b(\\d+)\\s*((?:[Bb]ao\\w*|[Bb]ano\\w*|[Bb]año\\w*))\\b"
  
  #Creamos la variable.
  x = x %>% mutate(baos_aux = ifelse(str_detect(description, patron),
                                         str_extract(description, patron), NA)) %>%
    #Nos quedamos con el número de la cadena.
    mutate(baos_aux = ifelse(!is.na(baos_aux),
                             as.numeric(substr(baos_aux,1,1)),
                             NA)) %>% #Lo metemos en la original
    mutate(bathrooms = ifelse(is.na(bathrooms),
                              baos_aux, bathrooms)) 
  
  #De nuevo, qué pasa si el número es una palabra?
  patron = "\\b([^\\d\\s]+)\\s*((?:[Bb]ao\\w*|[Bb]ano\\w*|[Bb]año\\w*))\\b"
  
  x = x %>% mutate(baos_aux = ifelse(str_detect(description, patron),
                                         str_extract(description, patron)
                                         ,NA))
  
  #Nos quedamos con la primera palabra.
  lista = str_split(x$baos_aux, " ")
  
  #A pie comparamos la primera palabra y la volvemos un número.
  for (i in 1:length(lista)){
    if (is.na(lista[[i]][1])){
      next
    } else if (lista[[i]][1]== "un" | lista[[i]][1]== "con"){
      x[i,"bathrooms"] = 1
    } else if (lista[[i]][1]== "dos"){
      x[i,"bathrooms"] = 2
    } else if (lista[[i]][1]== "tres"){
      x[i,"bathrooms"] = 3
    } else if (lista[[i]][1]== "cuatro"){
      x[i,"bathrooms"] = 4
    } else if (lista[[i]][1]== "cinco"){
      x[i,"bathrooms"] = 5
    } else if (lista[[i]][1]== "seis"){
      x[i,"bathrooms"] = 6
    } else if (lista[[i]][1]== "siete"){
      x[i,"bathrooms"] = 7
    } else if (lista[[i]][1]== "ocho"){
      x[i,"bathrooms"] = 8
    } else if (lista[[i]][1]== "nueve"){
      x[i,"bathrooms"] = 9
    } else if (lista[[i]][1]== "diez"){
      x[i,"bathrooms"] = 10
    } 
  }
  
  x = select(x, -baos_aux)
  
  #Intentemos llenar la variable de metros cuadrados construidos.
  #Buscamos el número, pero es muy duro que pongan los mt2 en palabras.
  patron = "\\b(\\d+)\\s*((?:[Mm]etros|[Mm]2|[Mm]ts|[Mm]mts))\\b"
  
  #Creamos la variable.
  x = x %>% mutate(metros = ifelse(str_detect(description, patron),
                                       str_extract(description, patron), NA)) %>%
    #nos quedamos solo con los números (sin incluir el 2 de cuadrados)
    mutate(metros = sub("(\\d+).*", "\\1",metros)) %>% #Lo reemplazamos en la var.
    mutate(surface_covered = ifelse(is.na(surface_covered), 
                                    as.numeric(metros),surface_covered)) %>%
    select(-metros, -surface_total)
  
  #Se crea una variable que nos diga cuántos parqueaderos tiene
  patron = "\\b(\\d+)\\s*((?:[Pp]arquea\\w*|[Gg]araje\\w*|[Cc]arro\\w*))\\b"
  
  x = x %>% mutate(parqueaderos = ifelse(str_detect(description, patron),
                                             str_extract(description, patron),
                                             NA)) %>% #Nos quedamos con los números.
    mutate(parqueaderos = as.numeric(sub("(\\d+).*", "\\1",parqueaderos)))
  
  #O con letras.
  patron = "\\b([^\\d\\s]+)\\s*((?:[Pp]arquea\\w*|[Gg]araje\\w*|[Cc]arro\\w*))\\b"
  x = x %>% mutate(par_aux = ifelse(str_detect(description, patron),
                                        str_extract(description, patron),
                                        NA))
  
  #Nos quedamos con la primera palabra.
  lista = str_split(x$par_aux, " ")
  
  #A pie comparamos la primera palabra y la volvemos un número.
  for (i in 1:length(lista)){
    if (is.na(lista[[i]][1])){
      next
    } else if (lista[[i]][1]== "un"){
      x[i,"parqueaderos"] = 1
    } else if (lista[[i]][1]== "dos"){
      x[i,"parqueaderos"] = 2
    } else if (lista[[i]][1]== "tres"){
      x[i,"parqueaderos"] = 3
    } else if (lista[[i]][1]== "cuatro"){
      x[i,"parqueaderos"] = 4
    } else if (lista[[i]][1]== "cinco"){
      x[i,"parqueaderos"] = 5
    } else if (lista[[i]][1]== "seis"){
      x[i,"parqueaderos"] = 6
    } else if (lista[[i]][1]== "siete"){
      x[i,"parqueaderos"] = 7
    } else if (lista[[i]][1]== "ocho"){
      x[i,"parqueaderos"] = 8
    } else if (lista[[i]][1]== "nueve"){
      x[i,"parqueaderos"] = 9
    } else if (lista[[i]][1]== "diez"){
      x[i,"parqueaderos"] = 10
    } 
  }
  
  return(x)
}

#Se corre la función.
aux = Limpieza_bases(aux)


# Calculo de distancias. --------------------------------------------------
#Acá se calculará la distancia que hay entre las propiedades y distintos 
#sitios de interés en Bogotá.


  
vis_dat(aux)  
  















