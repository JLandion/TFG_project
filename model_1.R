#### PAQUETES USADOS####

#install.packages("readxl")
#install.packages("mlogit")
#install.packages("glm2")




# Importamos la base de datos limpia ####
library(readxl)
datos_def <- read_excel("datos_def_excel")
names(datos_def)
summary(datos_def)
#options(digits = 4,scipen = 5)
#### Analisis descriptivo ####




# Introducimos las variables de interes un objeto subset para que puedan introducirse en el objeto glm
datos_modelo <- subset(datos_def, select = c("SAP", "sexo", "edad", "altura", "peso", "niv_est","enf_cron","IMC","fuma", "sit_lab",
                                             "frec_act_fis","estrato", "ingreso", "ingreso_eq","GHQ_12","m2","n_adultos","n_menores",
                                             "n_dormitorios","ruido", "olores","agua","limpieza",
                                             "cont_indus", "cont_otras","escasez_verde", "molest_animal", "delincuencia"))


# Analisis descriptivo antes de pasar las variables de tipo numerico a tipo factor
#library(summarytools)
#t(descr(datos_modelo, stats = c("mean", "sd", "max", "min"), na.rm =TRUE))

# Matriz de correlaciones
#cor(datos_modelo,use = "complete.obs")

# Convertimos algunas variables en tipo factor y las etiquetamos
# Pasamos las variables categoricas a tipo factor
datos_modelo$SAP <- factor(datos_modelo$SAP,labels=c("Buena salud","Mala salud"))
datos_modelo$sexo <- factor(datos_modelo$sexo,labels=c("Mujer","Hombre"))
datos_modelo$IMC <- factor(datos_modelo$IMC, labels=c("Si_saludable" , "No_saludable"))
datos_modelo$niv_est <- factor(datos_modelo$niv_est,labels=c("Primaria","Secundaria","Bachillerato","Universidad"))
datos_modelo$enf_cron <- factor(datos_modelo$enf_cron,labels=c("No","Si"))
datos_modelo$GHQ_12<-factor(datos_modelo$GHQ_12,labels=c("No prob mental", "Si prob mental"))
datos_modelo$fuma <- factor(datos_modelo$fuma,labels=c("Si","No"))
datos_modelo$sit_lab <- factor(datos_modelo$sit_lab,labels=c("Si puede","No puede"))
datos_modelo$frec_act_fis <- factor(datos_modelo$frec_act_fis,labels=c("No","Si"))

etiquetas_hogar <- c("No","Si")
datos_modelo$ruido <- factor(datos_modelo$ruido, labels = etiquetas_hogar)
datos_modelo$olores <- factor(datos_modelo$olores, labels = etiquetas_hogar)
datos_modelo$agua <- factor(datos_modelo$agua, labels = etiquetas_hogar)
datos_modelo$limpieza <- factor(datos_modelo$limpieza, labels = etiquetas_hogar)
datos_modelo$cont_indus <- factor(datos_modelo$cont_indus, labels = etiquetas_hogar)
datos_modelo$cont_otras <- factor(datos_modelo$cont_otras, labels = etiquetas_hogar)
datos_modelo$escasez_verde <- factor(datos_modelo$escasez_verde, labels = etiquetas_hogar)
datos_modelo$molest_animal <- factor(datos_modelo$molest_animal, labels = etiquetas_hogar)
datos_modelo$delincuencia <- factor(datos_modelo$delincuencia, labels = etiquetas_hogar)

# Comprobamos
#class(datos_modelo$SAP)

# Estimamos primero el modelo con las caracteristicas de la vivienda, para poder estimar el siguiente modelo con 
# exactamente los mismos datos de esta primera estimación y poder llevar a cabo el ANOVA
library(glm2)
modelo_logit_viv <- glm2( SAP ~sexo + edad + IMC + niv_est + enf_cron + 
                               sit_lab + frec_act_fis + GHQ_12  +  
                               n_dormitorios + cont_otras + agua + ruido + delincuencia, 
                         data = datos_modelo, family = binomial(link = "logit"),na.action = "na.omit")
# Resultados
summary(modelo_logit_viv)
#plot(modelo_logit_viv)

# Pseudo R2 (McFadden)
modelo_nulo <- glm(SAP~1, family = "binomial",data = datos_modelo)
pseudo_R_viv_signif <- 1 - logLik(modelo_logit_viv)/logLik(modelo_nulo) 
pseudo_R_viv_signif # 0.35071



# Modelo sin las caracteristicas de la vivienda empleando el dataframe del modelo con 
# las caracteristicas de la vivienda
modelo_logit <- glm2(SAP ~ sexo + edad + IMC + niv_est + enf_cron + 
                          sit_lab + frec_act_fis + GHQ_12,
                    data = modelo_logit_viv$model, family = binomial(link = "logit"),na.action = "na.omit")

# Resultados
summary(modelo_logit)
#plot(modelo_logit)

# Pseudo R2 (McFadden)
pseudo_R <- 1 - logLik(modelo_logit)/logLik(modelo_nulo) 
pseudo_R # 0.34956

# Finalmente, para comparar ambos modelos, realizamos un ANOVA
anova_noing <- anova(modelo_logit, modelo_logit_viv,test = "Chisq")
anova_noing


# Vamos a analizar la bondad del ajuste de otra forma, calculando el porcentaje de aciertos del modelo

# Modelo sin ingreso ni caracteristicas de la vivienda. 
y_fitted_noing <- modelo_logit$fitted.values
y_fitted_noing <- as.integer(y_fitted_noing >= 0.5)
y_noing <- modelo_logit$y
predict_eval_noing <- y_fitted_noing == y_noing
tabla_noing <- table(predict_eval_noing)
tabla_noing[2]/(tabla_noing[1] + tabla_noing[2]) # 0.78229

# Modelo sin ingreso y con caracteristicas de la vivienda
# Este es el que tiene mayor porcentaje de aciertos
y_fitted_noing_viv <- modelo_logit_viv$fitted.values
y_fitted_noing_viv <- as.integer(y_fitted_noing_viv >= 0.5)
y_noing_viv <- modelo_logit_viv$y
predict_eval_noing_viv <- y_fitted_noing_viv == y_noing_viv
tabla_noing_viv <- table(predict_eval_noing_viv)
tabla_noing_viv[2]/(tabla_noing_viv[1] + tabla_noing_viv[2]) # 0.78336


# Curva de ROC

library(pROC)

# Modelo con vivienda y sin ingreso
curva_ROC_viv = roc(modelo_logit_viv$y,modelo_logit_viv$fitted.values)
p_viv <- plot(curva_ROC_viv,
          col = "blue",
          print.auc = TRUE,
          print.auc.x = 0.8,
          print.auc.y = 0.6, 
          xlim = c(1,0),
          ylim = c(0,1),
          xlab = "1 - Especifidad", 
          ylab = "Sensibilidad", 
          main = "Curva de ROC Modelo II",
          print.auc.pattern = "%f")

polygon(with(p_viv, cbind(specificities, sensitivities)), 
        col = rgb(.25,0.31,0.61, alpha = 0.4), 
        border = "blue",
        lwd = 2)

# Modelo sin vivienda ni ingreso
curva_ROC = roc(modelo_logit$y,modelo_logit$fitted.values)
p <- plot(curva_ROC,
          col = "blue",
          print.auc = TRUE,
          print.auc.x = 0.8,
          print.auc.y = 0.6, 
          xlim = c(1,0),
          ylim = c(0,1),
          xlab = "1 - Especifidad", 
          ylab = "Sensibilidad", 
          main = "Curva de ROC Modelo I",
          print.auc.pattern = "%f")

polygon(with(p, cbind(specificities, sensitivities)), 
        col = rgb(.25,0.31,0.61, alpha = 0.4), 
        border = "blue",
        lwd = 2)