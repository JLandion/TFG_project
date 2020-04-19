library(readxl)
datos_def <- read_excel("datos_def_excel")

datos_modelo <- subset(datos_def, select = c("SAP", "sexo", "edad", "altura", "peso", "niv_est","enf_cron","IMC","fuma", "sit_lab",
                                             "frec_act_fis","estrato", "ingreso", "ingreso_eq","GHQ_12","m2","n_adultos","n_menores",
                                             "n_dormitorios","ruido", "olores","agua","limpieza",
                                             "cont_indus", "cont_otras","escasez_verde", "molest_animal", "delincuencia"))


# Vamos a agrupar los valores  de algunas variables (convertirlas en binarias)
# para que mejoren su significatividad en el modelo: fuma; frec_act_fis; caract. vivienda

attach(datos_modelo)
datos_modelo$frec_act_fis[frec_act_fis == 1 | frec_act_fis == 2] <- 1 # No hace ejercicio
datos_modelo$frec_act_fis[frec_act_fis == 3 | frec_act_fis == 4] <- 0 # Sí hace ejercicio
table(datos_modelo$frec_act_fis)

datos_modelo$sit_lab[sit_lab == 1 | sit_lab == 2 | sit_lab == 3 | sit_lab == 4 | sit_lab == 6 | sit_lab == 7] <- 0 # Si trabaja
datos_modelo$sit_lab[sit_lab == 5] <- 1 # No trabaja

datos_modelo$fuma[fuma == 1 | fuma == 2] <- 1 # Si fuma
datos_modelo$fuma[fuma == 3 | fuma == 4] <- 0 # No fuma
table(datos_modelo$fuma)

seleccion <- c("ruido", "olores","agua","limpieza", "cont_indus", "cont_otras",
             "escasez_verde", "molest_animal", "delincuencia")
datos_modelo[, seleccion] <- datos_modelo[, seleccion] * (datos_modelo[, seleccion] == 1)
table(datos_modelo$olores)

# Analisis descriptivo antes de pasar las variables de tipo numerico a tipo factor
library(summarytools)
t(descr(datos_modelo, stats = c("mean", "sd", "max", "min","n.valid"), na.rm = TRUE))

# Matriz de correlaciones
cor(datos_modelo,use = "complete.obs")

# Pasamos las variables categoricas a tipo factor
datos_modelo$SAP <- factor(datos_modelo$SAP,labels=c("Buena salud","Mala salud"))
datos_modelo$sexo <- factor(datos_modelo$sexo,labels=c("Mujer","Hombre"))
datos_modelo$niv_est <- factor(datos_modelo$niv_est,labels=c("Primaria","Secundaria","Bachillerato","Universidad"))
datos_modelo$enf_cron <- factor(datos_modelo$enf_cron,labels=c("No","Sí"))
datos_modelo$fuma <- factor(datos_modelo$fuma,labels=c("Sí","No"))
datos_modelo$sit_lab <- factor(datos_modelo$sit_lab,labels=c("Si puede","No puede"))
datos_modelo$frec_act_fis <- factor(datos_modelo$frec_act_fis,labels=c("No","Sí"))

etiquetas_hogar <- c("Sí","No")
datos_modelo$ruido <- factor(datos_modelo$ruido, labels = etiquetas_hogar)
datos_modelo$olores <- factor(datos_modelo$olores, labels = etiquetas_hogar)
datos_modelo$agua <- factor(datos_modelo$agua, labels = etiquetas_hogar)
datos_modelo$limpieza <- factor(datos_modelo$limpieza, labels = etiquetas_hogar)
datos_modelo$cont_indus <- factor(datos_modelo$cont_indus, labels = etiquetas_hogar)
datos_modelo$cont_otras <- factor(datos_modelo$cont_otras, labels = etiquetas_hogar)
datos_modelo$escasez_verde <- factor(datos_modelo$escasez_verde, labels = etiquetas_hogar)
datos_modelo$molest_animal <- factor(datos_modelo$molest_animal, labels = etiquetas_hogar)
datos_modelo$delincuencia <- factor(datos_modelo$delincuencia, labels = etiquetas_hogar)

class(datos_modelo$SAP); class(datos_modelo$agua)

# Estimacion sin caracteristicas/entorno de la vivienda
modelo_logit <- glm(SAP ~ sexo + edad + peso + niv_est + enf_cron   + sit_lab + 
                      frec_act_fis   + ingreso_eq + GHQ_12 , 
                    data = datos_modelo, family = binomial(link = "logit"),na.action = "na.omit")

summary.glm(modelo_logit)

# Añadimos las características de la vivienda una a una para evitar posible multicolinealidad
modelo_logit_viv <- glm( SAP ~ sexo + edad + peso + niv_est + enf_cron   + sit_lab +
                               frec_act_fis   + ingreso_eq + GHQ_12 +  
                               n_dormitorios  + delincuencia + cont_otras,
                         data = datos_modelo, family = binomial(link = "logit"),na.action = "na.omit")
# Resultados
summary(modelo_logit_viv)

