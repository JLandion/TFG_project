##### PAQUETES USADOS ##### 
#install.packages(readxl)
#install.packages(dplyr)
#install.packages(openxlsx)
#install.packages(mlogit)

library(readxl)
library(dplyr)
library(openxlsx)
library(mlogit)
library(stringr)




##### 1. IMPORTAMOS LOS DATOS Y GUARDAMOS EN UN EXCEL NUESTRAS VARIABLES DE INTERES ####


MICRODAT_EA_2017 <- read_excel("MICRODAT_EA_2017.xlsx")
MICRODAT_EH_2017 <- read_excel("MICRODAT_EH_2017.xlsx")




# Seleccionamos las variables que nos interesan de cada encuesta

# Encuesta de adultos
variables_adulto <- c(
  "CCAA","IDENTHOGAR","A7_2a", "FACTORADULTO",
  # Endogena, sexo, edad, enfermedades crónicas
  "G21","NIVEST","EDADa","SEXOa","G22",
  # Tabaco, actividad fisica y laboral
  "V121","T111","T112","ACTIVa",
  # Salud mental
  "M47a","M47b","M47_1","M47_2","M47_3","M47_4","M47_5",
  "M47_6","M47_7","M47_8","M47_9","M47_10","M47_11","M47_12",
  # Alimentación y clase social
  "U120_1","U120_1a","U120_2","U120_3","U120_4","U120_5","U120_6","U120_7",
  "U120_7a","U120_8","U120_9","U120_10","U120_11","U120_12","U120_13","U120_14",
  "U120_15","U120_15a","U120FZ","U120CANTFZ","CLASE_PR",
  # Peso, altura e IMC
  "S109","S110","IMCa")
  
# Encuesta de hogar  
variables_hogar <- c("FACTORHOGAR",
  "IDENTHOGAR","NORDEN_A","A7_1","NORDEN_Pref","A7_2a",
                     
  # Número de adultos y menores y estrato 
  "NADULTOS","NMENORES","D29", "ESTRATO",
  # Caracteristicas de la vivienda e ingreso mensual
  "C22","C23","C24_1","C24_2","C24_3","C24_4","C24_5",
  "C24_6","C24_7","C24_8","C24_9")


# Guardamos las variables de cada encuesta en dos data frame, los uniremos posteriormente 
datos_adultos <- data.frame(MICRODAT_EA_2017[,variables_adulto])
datos_hogares <- data.frame(MICRODAT_EH_2017[,variables_hogar])
# Comprobamos
class(datos_adultos);class(datos_hogares);head(datos_adultos)


# Renombramos las variables
nombre_vars_adultos <- c(
  SAP="G21",enf_cron="G22",edad="EDADa",sexo="SEXOa",
  altura="S109", peso="S110", IMC="IMCa",
  clase_pr="CLASE_PR", fuma="V121",tipo_act_fis="T111",
  frec_act_fis="T112",niv_est="NIVEST",sit_lab="ACTIVa",
  
  SM_frec_concentr="M47_1",SM_frec_preoc="M47_2",SM_frec_desemp="M47_3",
  SM_frec_decis="M47_4",SM_frec_agob="M47_5",SM_frec_super="M47_6",
  SM_frec_disfr="M47_7",SM_frec_frente="M47_8",SM_frec_deprim="M47_9",
  SM_frec_confi="M47_10",SM_frec_vale="M47_11",SM_frec_feliz="M47_12",
  SM_estres="M47a",SM_satis="M47b",
  
  al_frec_fruta="U120_1",al_cant_fruta="U120_1a",al_frec_carne="U120_2",
  al_frec_huevos="U120_3",al_frec_pescado="U120_4",al_frec_pasta="U120_5",
  al_frec_cereal="U120_6",al_frec_verdura="U120_7",al_cant_verdura="U120_7a",
  al_frec_legumbre="U120_8",al_frec_fiambre="U120_9",al_frec_lacteo="U120_10",
  al_frec_dulces="U120_11",al_frec_refresco="U120_12",al_frec_rapid="U120_13",
  al_frec_snack="U120_14",al_frec_zumo="U120_15",al_cant_zumo="U120_15a",
  al_frec_diaria_fruta_zumo="U120FZ",al_cant_fruta_zumo="U120CANTFZ")
  
nombre_vars_hogares <- c(
  n_adultos="NADULTOS",n_menores="NMENORES", estrato="ESTRATO",
  ingreso="D29",n_dormitorios="C22",m2="C23",ruido="C24_1",malos="C24_2",
  agua="C24_3",limpieza="C24_4",cont_indus="C24_5",cont_otras="C24_6",
  escasez_verde="C24_7",molest_animal="C24_8",delincuencia="C24_9"
)

datos_adultos <- rename(datos_adultos,!!nombre_vars_adultos)
datos_hogares <- rename(datos_hogares,!!nombre_vars_hogares)
# Comprobamos
names(datos_adultos);names(datos_hogares);head(datos_adultos)
summary(datos_adultos);summary(datos_hogares)


# Vemos que los datos son de tipo caracter y los queremos del tipo numerico, asi que:
class(datos_hogares$ingreso)
datos_adultos <- as.data.frame(sapply(datos_adultos,as.numeric))
datos_hogares <- as.data.frame(sapply(datos_hogares,as.numeric))

# Comprobamos
class(datos_hogares$ingreso);summary(datos_hogares$ingreso)
names(datos_adultos);head(datos_hogares)
summary(datos_adultos);summary(datos_hogares)


# Tratamiento de valores perdidos y NS/NC

# Veamos primero el número de NA que tenemos para cada vaariable
summarise_all(datos_adultos, funs(sum(is.na(.))))
summarise_all(datos_hogares, funs(sum(is.na(.))))


# Cambiamos los NS/NC por NA
lista_reemplazo_A <- list(
  list(cols = c("enf_cron","SM_estres","SM_satis","SM_frec_concentr","SM_frec_preoc", "SM_frec_desemp", 
                "SM_frec_decis", "SM_frec_agob", "SM_frec_super" ,"SM_frec_disfr" , 
                "SM_frec_frente" , "SM_frec_deprim","SM_frec_confi","SM_frec_vale",
                "SM_frec_feliz","tipo_act_fis","frec_act_fis","fuma"
              ),na_vals = c(8,9)),

  list(cols = c("sit_lab"),na_vals = c(8)),
  list(cols = c("clase_pr","IMC"),na_vals = c(9)),
  list(cols = c("niv_est"),na_vals = c(98,99)),
  list(cols = c("altura","peso"),na_vals = c(998,999))
  )

for (reemplazo_A in lista_reemplazo_A) {
  for (col in reemplazo_A$cols){
    datos_adultos[datos_adultos[, col] %in% reemplazo_A$na_vals,col] <- NA
  }
}

# Comprobamos 
summarise_all(datos_adultos, funs(sum(is.na(.))))



# Ahora con la encuesta de hogares
lista_reemplazo_H <- list(
  list(cols = c("ruido","malos","agua","limpieza", "cont_indus","cont_otras",
                  "escasez_verde","molest_animal" ,"delincuencia"),na_vals = c(8,9)),
  list(cols =  c("n_dormitorios", "ingreso"),na_vals = c(98,99)),
  list(cols = c("m2"),na_vals = c(998,999))
  )

for (reemplazo_H in lista_reemplazo_H) {
  for(col in reemplazo_H$cols) {
    datos_hogares[datos_hogares[, col] %in% reemplazo_H$na_vals,col] <- NA
  }
}

# Comprobamos
summarise_all(datos_hogares, funs(sum(is.na(.))))




##### 2. ADECUAMOS LA BASE DE DATOS PARA PODER TRABAJAR CON ELLA #####
attach(datos_adultos);attach(datos_hogares)

# * Cambiamos Los valores binarios a 1 y 0 en algunas variables
col_2 <- c("sexo","enf_cron","al_frec_diaria_fruta_zumo")
datos_adultos[, col_2] <- datos_adultos[, col_2] * !datos_adultos[, col_2] == 2
#   Comprobamos
table(datos_adultos$sexo); table(datos_adultos$enf_cron)
table(datos_adultos$al_frec_diaria_fruta_zumo)
#   Descubrimos que hay muy pocas observaciones de la variable al_frec_diaria_fruta_zumo


# * Convertimos la variable endogena (SAP) en binaria
table(datos_adultos$SAP)
datos_adultos$SAP[SAP == 1 | SAP == 2] <- 0
datos_adultos$SAP[SAP == 3 | SAP == 4 | SAP == 5] <- 1
#   Comprobamos
summary(datos_adultos$SAP)


# * Agrupamos los valores de la variable nivel de estudios
table(datos_adultos$niv_est)
#   Queremos agruparlos de 2 en 2
datos_adultos$niv_est[niv_est == 2 | niv_est == 3] <- 1
datos_adultos$niv_est[niv_est == 4 | niv_est == 5] <- 2
datos_adultos$niv_est[niv_est == 6 | niv_est == 7] <- 3
datos_adultos$niv_est[niv_est == 8 | niv_est == 9] <- 4
#   Comprobamos
table(datos_adultos$niv_est)


# * Obtenemos el GHQ-12, que es la suma de los valores de las variables sobre la salud mental

name_vars_GHQ_12 <- c("SM_frec_concentr","SM_frec_preoc","SM_frec_desemp",
                      "SM_frec_decis","SM_frec_agob","SM_frec_super",
                      "SM_frec_disfr","SM_frec_frente","SM_frec_deprim",
                      "SM_frec_confi","SM_frec_vale","SM_frec_feliz")

vars_GHQ_12 <- datos_adultos[name_vars_GHQ_12]
datos_adultos$GHQ_12 <- rowSums(vars_GHQ_12)

# Comprobamos

names(datos_adultos)
summary(datos_adultos$GHQ_12)


# * Ahora cambiamos los valores de la variable ingreso para poder  posteriormente
#   dividirla entre la escala de Oxford


# * Escala Oxford

datos_hogares$uds_consumo <- 1 + (datos_hogares$n_adultos - 1) * 0.7 + (datos_hogares$n_menores) * 0.5
summary(datos_hogares$uds_consumo)
datos_hogares$uds_consumo
names(datos_hogares)
head(datos_hogares$uds_consumo)
summary(datos_hogares$uds_consumo)

# * Ahora cambiamos los valores de la variable ingreso para poder  posteriormente
#   dividirla entre la escala Oxford


datos_hogares$ingreso[ingreso == 01] <-  570/2
datos_hogares$ingreso[ingreso == 02] <- (570 + 800)/2
datos_hogares$ingreso[ingreso == 03] <- (800 + 1050)/2
datos_hogares$ingreso[ingreso == 04] <- (1050 + 1300)/2
datos_hogares$ingreso[ingreso == 05] <- (1300 + 1550)/2
datos_hogares$ingreso[ingreso == 06] <- (1550 + 1800)/2
datos_hogares$ingreso[ingreso == 07] <- (1800 + 2200)/2
datos_hogares$ingreso[ingreso == 08] <- (2200 + 2700)/2
datos_hogares$ingreso[ingreso == 09] <- (2700 + 3600)/2
datos_hogares$ingreso[ingreso == 10] <- (3600 + 4500)/2
datos_hogares$ingreso[ingreso == 11] <- (4500 + 6000)/2
datos_hogares$ingreso[ingreso == 12] <-  9000

summary(datos_hogares$ingreso)


# Por ultimo, dividimos las unidades de consumo entre el ingreso
datos_hogares$ingreso_eq <- datos_hogares$ingreso / datos_hogares$uds_consumo

summary(datos_hogares$ingreso_eq);head(datos_adultos)


# Ahora exportamos los datos para que cuando los volvamos a cargar no tarde tanto en ejecutar
write.xlsx(datos_adultos,"datos_adultos_excel")
write.xlsx(datos_hogares,"datos_hogares_excel")

# Para fusionar ambos dataframes
datos_adultos %>% 
  left_join(datos_hogares, by = c("IDENTHOGAR", "A7_2a" = "NORDEN_A")) -> datos_def

colnames(datos_def)
head(datos_def)
summary(datos_def)
write.xlsx(datos_def,"datos_def_excel")


##### 3. USAMOS EL PAQUETE MLOGIT PARA EFECTUAR LAS ESTIMACIONES ####

# Se dejan como comentario las siguientes lineas de codigo, ya que dichas 
# estimaciones se efectuarán en otro sript y con otro paquete


# logdata_datos <- mlogit.data(datos_def,choice = "SAP",shape = "wide",varying = c("niv_est","edad","sexo"))
# 
# summary(mlogit(SAP~  niv_est + edad + sexo  , data = logdata_datos))
# # summary(mlogit(SAP~ sexo + edad + peso + altura + niv_est + enf_cron + fuma + sit_lab +
# #                  frec_act_fis + estrato  + ingreso_eq + GHQ_12 ,
# #                data = logdata_datos))
# glm_datos<-glm(SAP~ sexo +  niv_est,family=quasipoisson(),data = datos_merge)
# 
# counts <- c(18,17,15,20,10,20,25,13,12)
# outcome <- gl(3,1,9)
# treatment <- gl(3,3)
# d.AD <- data.frame(treatment, outcome, counts)
# glm.qD93 <- glm(counts ~ outcome + treatment, family = quasipoisson())


























# rm(datos_adultos)
# rm(datos_hogares)
# rm(datos_merge)
# rm(values)
# rm(col_2)
# rm(media_ingresos)
# rm(name_vars_GHQ_12)
# rm(nombre_vars_adulto)
# rm(nombre_vars_adultos)
# rm(nombre_vars_hogares)
# rm(variables_adulto)
# rm(variables_hogar)
# rm(vars_GHQ_12)
# rm(partones)
# rm(lista_reemplazo_A)
# rm(lista_reemplazo_H)
# rm(lista_reemplazo_A)
# rm(lista_reemplazo_H)
# rm(reemplazo_A)
# rm(reemplazo_H)
# rm(col)
# rm(counts)
# rm(outcome)
# rm(treatment)
# rm(glm.qD93)
# rm(logdata_datos)
# rm(d.AD)
# rm(datos_def)
