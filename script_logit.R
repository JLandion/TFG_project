# Explicación del paquete mlogit
#URL: https://cran.r-project.org/package=mlogit,
#     https://r-forge.r-project.org/projects/mlogit/

#install.packages("mlogit")
library(mlogit)
help(mlogit)

##### Fishing ####################################################
# Cargamos el data frame Fishing que viene incluido en el paquete:
# La eleccion de 4 formas de pesca, su coste, ratio de captura
# e ingreso mensual de cada forma de pesca

data("Fishing", package = "mlogit")
list(Fishing)
# Es necesario introducir los datos en un objeto del tipo mlogit.data
Fish <- mlogit.data(Fishing, #datos

                    varying = c(2:9), # Para indicar las variables exógenas
                    
                    shape = "wide",   # "long" si cada fila es una alternativa
                                      # "wide" si cada fila es una observacion
                    
                    choice = "mode")  # variable endógena

# Ya podemos ejecutar el modelo
summary(mlogit(mode ~ price + catch, data = Fish))

## Un modelo multinomial puro
summary(mlogit(mode ~ 0 | income, data = Fish))

## El cual puede ser estimado también usando el paquete:
library("nnet")
summary(multinom(mode ~ income, data = Fishing))

## a "mixed" model
m <- mlogit(mode ~ price+ catch | income, data = Fish)
summary(m)

## same model with charter as the reference level
m <- mlogit(mode ~ price+ catch | income, data = Fish, reflevel = "charter")

## same model with a subset of alternatives : charter, pier, beach
m <- mlogit(mode ~ price+ catch | income, data = Fish,
            alt.subset = c("charter", "pier", "beach"))

## model on unbalanced data i.e. for some observations, some
## alternatives are missing
# a data.frame in wide format with two missing prices
Fishing2 <- Fishing
Fishing2[1, "price.pier"] <- Fishing2[3, "price.beach"] <- NA
mlogit(mode~price+catch|income, Fishing2, shape="wide", choice="mode", varying = 2:9)


# mFormula
# este argumento es, en ocasiones, necesario para poder utilizar la funcion mlogit
# Sirve para indicar la especificación del modelo. Usamos Fishing para probar

form1_fishing <- mFormula(mode ~ price + catch)
head(model.matrix(form1_fishing,Fish, 2))

# Lo mismo, con una individual specific variable
form2_fishing <- mFormula(mode ~ price + catch | income)
head(model.matrix(form2_fishing, Fish), 2)

# Lo mismo, sin intercepto
form3_fishing <- mFormula(mode ~ price + catch | income + 0)
head(model.matrix(form3_fishing, Fish), 2)

# lo mismo que en form2_fishing, pero ahora, coeficientes de catch son alternativas
# specific

form4_fishing <- mFormula(mode ~ price | income | catch)
head(model.matrix(form4_fishing, Fish), 2)









##### TravelMode #######################################################
# El data.frame esta en formato "long"
#install.packages("AER")
data("TravelMode", package = "AER")
TravelMode
summary(TravelMode)
TM <- mlogit.data(TravelMode, choice = "choice", shape = "long",
                  alt.levels = c("air", "train", "bus", "car"))

# Lo mismo pero ahora llamando a la variable exogena, en vez de sus valores
TM <- mlogit.data(TravelMode ,choice = "choice", shape = "long",
                  alt.var = "mode")

# Lo mismo pero ahora indicando la variable id
TM <- mlogit.data(TravelMode, choice = "choice",
                  shape = "long", id.var = "individual",
                  alt.levels = c("air", "train", "bus", "car"))

# Análogo e idéntico al caso anterior
TM <- mlogit.data(TravelMode, choice = "choice", shape = "long",
                  id.var = "individual", alt.var = "mode")

# Lo mismo pero indicando que queremos eliminar la variable id
TM <- mlogit.data(TravelMode, choice = "choice", shape = "long",
                  id.var = "individual", alt.var = "mode", drop.index = TRUE)





# a data.frame in long format with three missing lines
data("TravelMode", package = "AER")
Tr2 <- TravelMode[-c(2, 7, 9),]
mlogit(choice~wait+gcost|income+size, Tr2, shape = "long",
       chid.var = "individual", alt.var="mode", choice = "choice")

## An heteroscedastic logit model
data("TravelMode", package = "AER")
hl <- mlogit(choice ~ wait + travel + vcost, TravelMode,
             shape = "long", chid.var = "individual", alt.var = "mode",
             method = "bfgs", heterosc = TRUE, tol = 10)

## A nested logit model
TravelMode$avincome <- with(TravelMode, income * (mode == "air"))
TravelMode$time <- with(TravelMode, travel + wait)/60
TravelMode$timeair <- with(TravelMode, time * I(mode == "air"))
TravelMode$income <- with(TravelMode, income / 10)

# Hensher and Greene (2002), table 1 p.8-9 model 5
TravelMode$incomeother <- with(TravelMode, ifelse(mode %in% c('air', 'car'), income, 0))
nl <- mlogit(choice~gcost+wait+incomeother, TravelMode,
             shape='long', alt.var='mode',
             nests=list(public=c('train', 'bus'), other=c('car','air')))

# same with a comon nest elasticity (model 1)
nl2 <- update(nl, un.nest.el = TRUE)





##### Train ##############################################################################
# Train es un data.frame tipo "wide". La variable "choiceid" es el index,
# Las alternativas son llamadas "ch1" y "ch2, el opuesto de la variable es devuelto
data("Train", package = "mlogit")
Train
summary(Train)
Train <- mlogit.data(Train, choice = "choice", shape = "wide",varying = 4:11,
                     alt.levels = c("A", "B"), # Nombre de los valores que toma la endogena
                     sep = "_", # Para indicar el signo que separa las distintas alternativas
                     opposite = c("price", "time", "change", "comfort")) # exógenas






##### HC  GAME ####################################################################################
data("HC", package = "mlogit")
HC <- mlogit.data(HC, choice = "depvar", varying=c(2:8, 10:16), shape="wide")

# Game es un data.frame en formato wide el cual la respuesta es una variable ranking
data("Game", package = "mlogit")
G <- mlogit.data(Game, shape="wide", varying = 1:12, alt.var = 'platform',
                 drop.index = TRUE, # En caso de que las variables de índice se eliminen del data.frame
                 choice="ch", ranked = TRUE)# si la endógena está en un rango
# a ranked ordered model
data("Game", package = "mlogit")
g <- mlogit(ch~own|hours, Game, choice='ch', varying = 1:12,ranked=TRUE, shape="wide", reflevel="PC")



# Los mismos datos pero en formato long
data("Game2", package = "mlogit")
G2 <- mlogit.data(Game2, shape='long', choice="ch", alt.var = 'platform', ranked = TRUE)








# Ahora usamos la función que estima por MV

G_model <- mlogit(ch~own|hours, Game, choice='ch', varying = 1:12,
            ranked=TRUE, shape="wide", reflevel="PC")
summary(G_model)


