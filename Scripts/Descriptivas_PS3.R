###
#
# Programación problem set 3
# Estadísticas descriptivas.
#
###
# Plan de acción: 
# A las variables continuas sacales las estadísticas de siempre: N, media, sd, percentiles.
# A las categóricas hace un gráfico de barras para ver el número de datos por categoría.
# Si hay continuas con valores atípicos, hace un histograma para analizarlos mejor.

# Preparación del ambiente ------------------------------------------------
rm(list=setdiff(ls(), c("aux", "poligonos")))

libraries = c("ggplot2", "tidyverse", "skimr", "stargazer", "gridExtra", "ggpubr",
              "leaflet", "htmlwidgets", "sf") 

if(length(setdiff(libraries, rownames(installed.packages()))) > 0){
  install.packages(setdiff(libraries, rownames(installed.packages())))
}

invisible(sapply(libraries, require, character.only = TRUE,quietly = TRUE))

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)
aux = readRDS(paste0(path,"Stores/Propiedades_final.rds"))

# Análisis de missing values. ---------------------------------------------
#Tabla con total de missings y proporción al total de datos.

#Se crea una función auxiliar para calcular los missings values.
Missings_aux = function(x){
  y = skim(x[,-c(1,9:11,20,21)]) %>% select(skim_variable, n_missing)
  y$proporcion = round((y$n_missing/dim(x)[1]*100),2)
  return(y)
}

Missings = Missings_aux(aux)

#En Latex.
stargazer(Missings, type = "latex", title = "Valores faltantes",
          label = "Tabla_missings", summary = FALSE)

saveRDS(Missings, paste0(path,"Stores/analisis_Missings_DB.rds"))

# Estadísticas vars continuas ---------------------------------------------
#Las variables continuas son.
continuas = c("price", "surface_covered", "parqueaderos",
              "parques", "hospitales", "turismo", "supermercado", "restaurantes",
              "mall", "Colegios")

DB_continuas = st_drop_geometry(aux[,colnames(aux) %in% continuas])

#Por propósitos de hacerlo más legible vamos a mostrar el precio en millones.
DB_continuas$price = DB_continuas$price/1e6

#Se calculan distintas estadísticas:
#Número de observaciones que no son missing
Observations = apply(DB_continuas, MARGIN = 2, function(x){
  Non_missings = sum(!is.na(x))
  return(Non_missings)
}) 

#Media
mean_aux = round(apply(DB_continuas, MARGIN = 2, mean, na.rm = TRUE),2)

#SD
sd_aux = round(apply(DB_continuas, MARGIN = 2, sd, na.rm = TRUE),2)

#Percentiles.
percentiles = t(round(apply(DB_continuas, MARGIN = 2, quantile, na.rm = TRUE,
                            probs = c(c(0.05, 0.25, 0.5, 0.75, 0.95))),2))

#En una matriz
Estadisticas_continuas = cbind(continuas, Observations, mean_aux, sd_aux, percentiles)

colnames(Estadisticas_continuas) = c("Nombre","No. Observaciones", "Media", "Desv. Estándar", 
                                     "Per. 5", "Per. 25", "Per. 50", "Per. 75", 
                                     "Per 95")
#Hacia latex
stargazer(Estadisticas_continuas, type = "latex", title = "Estadísticas descriptivas",
          subtitle = "variables continuas", label = "Tabla_continuas",
          summary = FALSE)

saveRDS(Estadisticas_continuas, paste0(path,"Stores/Estadisticas_vars_continuas.rds"))


# Estadísticas discretas --------------------------------------------------
#Se seleccionan las variables discretas.
Discretas = c("property_type", "bedrooms", "bathrooms", "Localidad")

DB_discretas = st_drop_geometry(aux[,colnames(aux) %in% Discretas])
DB_discretas = na.omit(DB_discretas)

#Se hace un gráfico de barras por cada var discreta.
for (i in colnames(DB_discretas)){
  
  if (i=="Localidad"){
    png(filename = paste0(path, "Views/Hist_", i, ".png"),
        width = 1464, height = 750)
    graph_aux = ggplot(DB_discretas, aes(x = as.factor(!!sym(i)))) +
      geom_bar(fill = "skyblue", color = "black", alpha = 0.8, 
               width = 0.5) +
      geom_text(stat = "count", aes(label = after_stat(count)), 
                vjust = -0.5) +
      labs(title = paste0("Distribución variable ", i), 
           y = "Conteo") +
      theme(plot.title = element_text(hjust = 0.5, size = 15),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(angle = 90, size = 14),
            axis.text.y = element_blank(),
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank(), 
            axis.line = element_line(color = "black", size = 1)) 
    print(graph_aux)
    dev.off()
  } else {
    png(filename = paste0(path, "Views/Hist_", i, ".png"),
        width = 1464, height = 750)
    graph_aux = ggplot(DB_discretas, aes(x = as.factor(!!sym(i)))) +
      geom_bar(fill = "skyblue", color = "black", alpha = 0.8, 
               width = 0.5) +
      geom_text(stat = "count", aes(label = after_stat(count)), 
                vjust = -0.5) +
      labs(title = paste0("Distribución variable ", i), 
           y = "Conteo") +
      theme(plot.title = element_text(hjust = 0.5, size = 15),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_blank(),
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank(), 
            axis.line = element_line(color = "black", size = 1)) 
    print(graph_aux)
    dev.off()
  }
}




# Visualización -----------------------------------------------------------

#Hagamos un mapita chiquito para ver la concentración de los datos a través de 
#Bogotá

#Creamos unos códigos de color según el tipo de propiedad.
aux = aux %>%
  mutate(color = case_when(property_type == "Apartamento" ~ "darkorange",
                           property_type == "Casa" ~ "lightblue"))

# # Para centrar el mapa 
# lat_central = mean(aux$lat)
# lon_central = mean(aux$lon)
# 
# # Creamos el gráfico con openstreetmap.
# 
# mapa = leaflet() %>%
#   addTiles() %>%
#   setView(lng = lon_central, lat = lat_central, zoom = 12) %>%
#   addCircleMarkers(lng = aux$lon, lat = aux$lat, color = aux$color, 
#                    fillColor = "black",
#                    fillOpacity = 1, opacity = 1, radius = 0.0001)
# saveWidget(mapa, file = paste0(path,"Views/mapa.html")
#            , selfcontained = TRUE)

#Ahora apoyenemos en las localidades
#Antes del gráfico, hay algunos poligonos que no tienen propiedades (como 
#sumapaz, eso es un páramo, qué casas va a tener). Me quedo con algunos
#poligonos entonces.

poligonos_aux = poligonos[-c(9,14,15),] #Quité Usme, sumapaz y Ciudad Bolivar.

png(filename = paste0(path, "Views/Poligonos.png"),
    width = 1464, height = 750)
ggplot() +
  geom_sf(data = poligonos_aux) +
  geom_sf(data = aux, aes(color = property_type)) +
  labs(color = "Tipo de propiedad") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        legend.key = element_rect(fill = "gray", color = "black"))
dev.off()





















