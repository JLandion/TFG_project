# Proyecto_TFG
Trabajo Fin de Grado desarrollado en R
## Introducción
En este trabajo se pretende analizar la relación entre el estado de salud de los españoles y las características de su vivivienda y su entorno.

## Descripción de los archivos
Procedo a explicar el contenido de cada archivo que se incluye en el repositorio:
* Base de datos en bruto: Para efectuar el análsis, se emplea la Encuesta Nacional de Salud en España del 2017. 
Estos archivos están en formato excel `.xlsx` con el nombre de `MICRODAT_EA_2017.xlsx` y `MICRODAT_EH_2017.xlsx`,
para la encuesta de adultos (EA) y la encuesta de los hogares (EH).

* Las dos bases de datos mencionadas en el apartado anterior se manipulan, tratan y fusionan en el archivo `transformacion_datos.R`
En este código también se generan tres archivos de datos que pueden abrirse con excel:
    * El archivo `datos_adultos_excel` contiene las variables de interés que están contenidas en la encuesta de adultos
    * El archivo `datos_hogares_excel` contiene las variables de interés que están contenidas en la encuesta de hogares
    * El archivo `datos_def_excel`contiene las variables de interés de ambas bases de datos y es con la que 
    se efectuarán las siguientes estimaciones
    
* Las estiamciones (modelo logit binomial) se efectúan en los script llamados `model_1.R` y `model_2.R`.
* En el archivo `script_logit.R` se incluye los ejemplos incluidos en el PDF del paquete "glm" y así poder ir familiarizándonos 
con las posibilidades que ofrece este paquete, que es el más importante de este proyecto.

