El próposito de este repositorio es contener la información necesaria para la realización del problem set 3 del curso Big data y y Machine Learning. Para ello, el repositorio contiene las siguientes carpetas:

- Stores: acá se guardan las bases de datos que se emplearon para el entrenamiento de los modelos y los resultados en términos de hiperpárametros óptimos y métricas de desempeño. Las bases de datos se dividen en
dos subcarpetas: pre procesadas y procesadas. En las pre procesadas se encuentran las bases en su versión plain vanilla, antes de cualquier proceso de limpieza de datos. Procesadas contiene la versión
de estas luego de atravesar el proceso de limpieza. Por último, las predicicones contiene archivos CSV con la predicción del precio de las viviendas con la muestra de validación. Estas se encuentran
separadas por el tipo de modelo (por ejemplo, regresión lineal o random forest) que se utilizó.
- Views: en esta carpeta se guardaron las gráficas realizadas en el trabajo. Estas incluyen desde diagramas de barra para contar las variables categóricas, al igual que mapas que permiten dar una idea de la
distribución espacial de los datos.
- Scripts: en esta carpeta se guardan los códigos en el lenguaje R que permitieron realizar todo el trabajo. El título de cada código está pensado para dar contexto sobre su propósito, el cual puede ser
la limpieza de los datos, la corrida de los modelos y la creación de las predicciones para subir a la plataforma kaggle.
