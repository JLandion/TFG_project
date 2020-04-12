#### PAQUETES USADOS####

#install.packages("readxl")
#install.packages("dplyr")
#install.packages("openxlsx")
#install.packages("mlogit")
#install.packages("glm2")


library(readxl)
library(dplyr)
library(openxlsx)
library(glm2)
# Importamos la base de datos limpia ####
datos_def <- read_excel("datos_def_excel")
names(datos_def)
summary(datos_def)
#### Analisis descriptivo ####


#### M1: ESTIMACION DEL MODELO SIN CONSIDERAR LAS CARACTERISTICAS DE LA VIVIENDA ####

# Introducimos las variables de interes un objeto subset para que puedan introducirse en el objeto glm
datos_modelo <- subset(datos_def, select = c("SAP", "sexo", "edad","altura","peso", "niv_est","enf_cron","IMC","fuma", "sit_lab",
                                             "frec_act_fis","estrato","ingreso","ingreso_eq","GHQ_12","m2","n_adultos","n_menores",
                                            "ruido", "malos","agua","limpieza",
                                            "cont_indus", "cont_otras","escasez_verde", "molest_animal", "delincuencia"))
head(datos_modelo)
datos_modelo$SAP <- factor(datos_modelo$SAP,labels=c("Buena salud","Mala salud"))
datos_modelo$sexo <- factor(datos_modelo$sexo,labels=c("Mujer","Hombre"))
datos_modelo$niv_est <- factor(datos_modelo$niv_est,labels=c("Primaria","Secundaria","Bachillerato","Universidad"))
datos_modelo$enf_cron <- factor(datos_modelo$enf_cron,labels=c("No","Sí"))
datos_modelo$fuma <- factor(datos_modelo$fuma,labels=c("A diario","A veces","Antes sí","Nunca"))
datos_modelo$sit_lab <- factor(datos_modelo$sit_lab,labels=c("Trabaja","desempleo","jubilado","estudiante","incapacitado","Hogar","Otros"))
datos_modelo$frec_act_fis <- factor(datos_modelo$frec_act_fis,labels=c("No ejercicio","ocasional","Frec. mensual","Frec. Semanal"))

etiquetas_hogar <- c("mucho","algo","nada")

# Convertimos algunas variables en tipo factor
variables_factor <- c("SAP","sexo","niv_est","enf_cron","fuma","sit_lab","frec_act_fis","estrato",
                     "ruido", "malos","agua","limpieza"  ,
                     "cont_indus", "cont_otras","escasez_verde", "molest_animal", "delincuencia")

datos_modelo[, variables_factor] <- lapply(datos_modelo[, variables_factor], as.factor)
# Comprobamos
class(datos_modelo$SAP)
# Creamos el objeto que contiene los resultados del modelo
modelo_logit <- glm(SAP ~ sexo + edad + niv_est + enf_cron + IMC + fuma + sit_lab +
                       frec_act_fis + estrato  + ingreso_eq + GHQ_12 + m2 , 
                       data = datos_modelo, family = binomial(link = "logit"),na.action = "na.omit")
# Resultados
summary(modelo_logit)
plot(modelo_logit)

# Pseudo R2 (McFadden)
modelo_nulo <- glm(SAP~1, family = "binomial",data = datos_modelo)
summary(modelo_nulo)
pseudo_R <- 1 - logLik(modelo_logit)/logLik(modelo_nulo) # 0.55
pseudo_R
coefs <- c(modelo_logit$coefficients)
coefs

#### M2: ELIMINAMOS LAS VARIABLES MENOS SIGNIFICATIVAS de M1 ####


# Creamos el objeto que contiene los resultados del modelo
modelo_logit_signif <- glm(SAP ~ edad + niv_est + enf_cron + sit_lab +frec_act_fis + ingreso_eq + GHQ_12, 
                    data = datos_modelo, family = binomial(link = "logit"),na.action = "na.omit")
# Resultados
summary(modelo_logit_signif)

# Pseudo R2 (McFadden)
pseudo_R_signif <- 1 - logLik(modelo_logit_signif)/logLik(modelo_nulo) # 0.48
pseudo_R_signif
coefs_signif <- c(modelo_logit_signif$coefficients)
coefs_signif


#### M3: COMO EL M1 PERO AHORA CONSIDERANDO LAS CARACTERISTICAS DE LA VIVIENDA ####


# Creamos el objeto que contiene los resultados del modelo
modelo_logit_viv <- glm(SAP ~ edad  + enf_cron + IMC + fuma + sit_lab +frec_act_fis  +GHQ_12 + m2 +
                        ruido + malos + agua + limpieza + cont_indus + cont_otras + escasez_verde + molest_animal + delincuencia, 
                    data = datos_modelo, family = binomial(link = "logit"),na.action = "na.omit")
# Resultados
summary(modelo_logit_viv)

# Pseudo R2 (McFadden)


pseudo_R_viv <- 1 - logLik(modelo_logit_viv)/logLik(modelo_nulo) # 0.55
pseudo_R_viv
coefs_viv <- c(modelo_logit_viv$coefficients)
coefs_viv



#### M4: COMO EL M2 PERO CONSIDERANDO LAS CARACTERISTICAS DE LA VIVIENDA ####


# Creamos el objeto que contiene los resultados del modelo
modelo_logit_viv_signif <- glm( SAP ~sexo + edad + niv_est + enf_cron + IMC + fuma + sit_lab +
                                  frec_act_fis + estrato  + ingreso_eq + GHQ_12 + m2  + 
                          ruido + malos + agua + limpieza + cont_indus + cont_otras + escasez_verde + molest_animal + delincuencia, 
                        data = datos_modelo, family = binomial(link = "logit"),na.action = "na.omit")
# Resultados
summary(modelo_logit_viv_signif)
plot(modelo_logit_signif)

# Pseudo R2 (McFadden)
pseudo_R_viv_signif <- 1 - logLik(modelo_logit_viv_signif)/logLik(modelo_nulo) # 0.49
pseudo_R_viv_signif
coefs_viv_signif <- c(modelo_logit_viv_signif$coefficients)
coefs_viv_signif

#### Análisis descriptivo de M4 ####

library(tidyverse)
library(skimr)
library(broom)

# mtcars %>% 
#   as_tibble() %>% 
#   mutate_at(vars(gear), factor) %>% 
#   select(mpg, wt, gear, cyl) -> limpio
# 
# skim(limpio) %>% 
#   as_tibble() -> descriptivos
# 
# 
# modelo <- glm(gear~mpg+wt+cyl, family = binomial(), data = limpio)
# tidy(modelo) %>% 
#   rename(skim_variable = term) %>% #para que coincida el nombre en el join
#   inner_join(descriptivos)  %>% 
#   select(variable = skim_variable, 
#          coeficiente = estimate, 
#          error = std.error, 
#          media = numeric.mean,
#          desviación = numeric.sd
#   )




library(tidyverse)
library(skimr)
library(broom)


library(broom)

datos_modelo %>%
  as_tibble() %>%
  mutate_at(vars(SAP), factor) %>%
  select(SAP, sexo, edad, niv_est, enf_cron, IMC, fuma, sit_lab, frec_act_fis, estrato, ingreso_eq, GHQ_12, m2,
         ruido, malos, agua, limpieza, cont_indus, cont_otras, escasez_verde, molest_animal, delincuencia) -> limpio

skim(limpio) %>%
  as_tibble() -> descriptivos


modelo <-glm( SAP ~sexo + edad + niv_est + enf_cron + IMC + fuma + sit_lab +
                frec_act_fis + estrato  + ingreso_eq + GHQ_12 + m2  + 
                ruido + malos + agua + limpieza + cont_indus + cont_otras + escasez_verde + molest_animal + delincuencia, 
              data = datos_modelo, family = binomial(link = "logit"),na.action = "na.omit")
tidy(modelo) %>%
  rename(skim_variable = term) %>% #para que coincida el nombre en el join
  inner_join(descriptivos)  %>%
  select(variable = skim_variable,
         coeficiente = estimate,
         error = std.error,
         media = numeric.mean,
         desviación = numeric.sd,
         mínimo = numeric.p0,
         máximo = numeric.p100
  )





















