# svm-master
Esta es una sencilla shiny app que implementa SVM sobre un conjunto de datos.
Por defecto, trabaja con un data frame de 3000 observaciones creados por R y clasificados en 3 clasess. 
Puede optar por la opcion de usar un archivo CSV e importarlo en la aplicacion.
Del total de datos, especifique una submuestra, seleccione el tipo de kernel y en base a eso configure los parametros necesarios. 
Se muestra la tabla de confusion, el porcentaje correctamente clasificado segun la prediccion y el total de vectores soporte. En la pestaña Grafica 1, muestra una grafica hecha con Rbokeh, en Grafica 2 se grafica el modelo usando ggplot2, en la pestaña Tabla muestran los datos de la muestra en una tabla y en Sumario, el resumen del modelo SVM implementado
                            
Basada en ejemplos y la documentacion de RStudio Shiny
