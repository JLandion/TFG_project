#### PAQUETES USADOS####

#install.packages("readxl")
#install.packages("mlogit")
#install.packages("glm2")


library(readxl)
datos_def <- read_excel("datos_def_excel")
names(datos_def)
summary(datos_def)
#options(digits = 4,scipen = 5)

# Introducimos las variables de interes un objeto subset para que puedan introducirse en el objeto glm
datos_modelo <- subset(datos_def, select = c("SAP", "sexo", "edad", "altura", "peso", "niv_est","enf_cron","IMC","fuma", "sit_lab",
                                             "frec_act_fis","estrato", "ingreso", "ingreso_eq","GHQ_12","m2","n_adultos","n_menores",
                                             "n_dormitorios","ruido", "olores","agua","limpieza",
                                             "cont_indus", "cont_otras","escasez_verde", "molest_animal", "delincuencia"))


# Analisis descriptivo antes de pasar las variables de tipo numerico a tipo factor
#library(summarytools)
#t(descr(datos_modelo, stats = c("mean", "sd", "min", "max","n.valid"), na.rm = TRUE))

# Matriz de correlaciones
#cor(datos_modelo,use = "complete.obs")

# Convertimos algunas variables en tipo factor y las etiquetamos
# Pasamos las variables categoricas a tipo factor
datos_modelo$SAP <- factor(datos_modelo$SAP,labels=c("Buena salud","Mala salud"))
datos_modelo$sexo <- factor(datos_modelo$sexo,labels=c("Mujer","Hombre"))
datos_modelo$IMC <- factor(datos_modelo$IMC, labels=c("Si saludable, No saludable"))
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
#class(datos_modelo$SAP); class(datos_modelo$agua)

# Estimamos primero el modelo con las caracteristicas de la vivienda, para poder estimar el siguiente modelo con 
# exactamente los mismos datos de esta primera estimación y poder llevar a cabo el ANOVA
library(glm2)
modelo_logit_ing_viv <- glm2( SAP ~ sexo + edad + IMC + niv_est + enf_cron + 
                                    sit_lab + frec_act_fis + GHQ_12 + ingreso_eq +
                                    n_dormitorios  + delincuencia  + cont_otras,
                         data = datos_modelo, family = binomial(link = "logit"),na.action = "na.omit")
# Resultados
summary(modelo_logit_ing_viv)
#plot(modelo_logit_viv)

# Pseudo R2 McFadden
# Este es el que presenta un mejor Pseudo R2 McFadden
modelo_nulo <- glm(SAP~1, family = "binomial",data = datos_modelo)
pseudo_R_ing_viv <- 1 - logLik(modelo_logit_ing_viv)/logLik(modelo_nulo) 
pseudo_R_ing_viv #0.48565


modelo_logit_ing <- glm2(SAP ~ sexo + edad + IMC + niv_est + enf_cron  +  
                               sit_lab + frec_act_fis + GHQ_12 + ingreso_eq , 
                    data = modelo_logit_ing_viv$model, family = binomial(link = "logit"),na.action = "na.omit")
# Resultados
summary.glm(modelo_logit_ing)
#plot(modelo_logit)

# Pseudo R2 McFadden
pseudo_R_viv <- 1 - logLik(modelo_logit_ing)/logLik(modelo_nulo) 
pseudo_R_viv # 0.48489

anova_ing <- anova(modelo_logit_ing, modelo_logit_ing_viv, test = "Chisq")
anova_ing

# Vamos a analizar la bondad del ajuste de otra forma, calculando el porcentaje de aciertos del modelo

# Modelo sin ingreso ni caracteristicas de la vivienda
y_fitted_ing <- modelo_logit_ing$fitted.values
y_fitted_ing <- as.integer(y_fitted_ing >= 0.5)
y_ing <- modelo_logit_ing$y
predict_eval_ing <- y_fitted_ing == y_ing
tabla_ing <- table(predict_eval_ing)
tabla_ing[2]/(tabla_ing[1] + tabla_ing[2]) # 0.7741

# Modelo sin ingreso y con caracteristicas de la vivienda
y_fitted_ing_viv <- modelo_logit_ing_viv$fitted.values
y_fitted_ing_viv <- as.integer(y_fitted_ing_viv >= 0.5)
y_ing_viv <- modelo_logit_ing_viv$y
predict_eval_ing_viv <- y_fitted_ing_viv == y_ing_viv
tabla_ing_viv <- table(predict_eval_ing_viv)
tabla_ing_viv[2]/(tabla_ing_viv[1] + tabla_ing_viv[2]) # 0.77556


# Curva de ROC

library(pROC)

# Modelo sin vivienda y con ingreso
curva_ROC_ing = roc(modelo_logit_ing$y,modelo_logit_ing$fitted.values)
p_ing <- plot(curva_ROC_ing,
              col = "blue",
              print.auc = TRUE,
              print.auc.x = 0.8,
              print.auc.y = 0.6, 
              xlim = c(1,0),
              ylim = c(0,1),
              xlab = "1 - Especifidad", 
              ylab = "Sensibilidad", 
              main = "Curva de ROC Modelo III",
              print.auc.pattern = "%f")

polygon(with(p_ing, cbind(specificities, sensitivities)), 
        col = rgb(.25,0.31,0.61, alpha = 0.4), 
        border = "blue",
        lwd = 2)


# Modelo con vivienda y con ingreso
curva_ROC_ing_viv = roc(modelo_logit_ing_viv$y,modelo_logit_ing_viv$fitted.values)
p_ing_viv <- plot(curva_ROC_ing_viv,
              col = "blue",
              print.auc = TRUE,
              print.auc.x = 0.8,
              print.auc.y = 0.6, 
              xlim = c(1,0),
              ylim = c(0,1),
              xlab = "1 - Especifidad", 
              ylab = "Sensibilidad", 
              main = "Curva de ROC Modelo IV",
              print.auc.pattern = "%f")

polygon(with(p_ing_viv, cbind(specificities, sensitivities)), 
        col = rgb(.25,0.31,0.61, alpha = 0.4), 
        border = "blue",
        lwd = 2)
