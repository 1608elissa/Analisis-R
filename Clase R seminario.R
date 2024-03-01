#### INTRODUCCION ####

# OPERACIONES ARITMETICAS
1+9
9-5
4/2
9*5

###### VECTORES
x <- 1+9
y <- 9-5
 
x+y

z <- c(x,y)
print(z)

numeros <- c(1,2,3,4,5)
palabras <- c("Ana","Juan","Ulises","Alejandra")
mixto <- c(8,"Ana",5,6,"Juan")

# Para saber el tamaño de un vector
length(numeros)
length(palabras) 

#¿Qué tipos/clases de elementos (o datos) tenemos en R?
# Caracter
class(palabras)
# Numéricos
class(numeros)

# Los vectores deben ser del mismo tipo
class(mixto)
# Se puede hacer coercion del vector, pero no codifica bien
as.numeric(mixto)

# Borrar objetos creados previamente
rm(mixto)
remove(numeros)

###### MATRICES
matriz1 <- matrix (data= c(1,2,3,4,5,6), nrow=2, ncol=3)
matriz2 <- matrix (1:10, nrow=5)

###### LISTAS
lista <- list(Miembros = c("Ana","Juan","Ulises","Alejandra"), Edad =c(22,20,33,26))
lista$Miembros
lista$Edad

#### CARGAR BASE DE DATOS ####
install.packages("readxl")
library(readxl)

data <- read_xlsx("Base Ixchel.xlsx",sheet = "TR_RC")%>%
  gather(tip_cond_val, value, -c("ID","DECADA", "SEXO", "EDAD")) %>%
  separate(tip_cond_val, c("COND", "TR_RC","TIPO","VAL"),
           sep = "_")
data$DECADA <- as.factor(data$DECADA)
data$COND <- as.factor(data$COND)
data$TR_RC <- as.factor(data$TR_RC)
data$TIPO <- as.factor(data$TIPO)
data$VAL <- as.factor(data$VAL)

data3 <- read_xlsx("Junto.xlsx",sheet = "Demo")

data3$DECADA <- as.factor(data3$DECADA)
data3$ESCOLARIDAD <-as.numeric(data3$ESCOLARIDAD)
data3$IDARE_R_PUNTAJE <-as.numeric(data3$IDARE_R_PUNTAJE)
data3$IDERE_R_PUNTAJE <-as.numeric(data3$IDERE_R_PUNTAJE)
data3$IDARE_E_PUNTAJE <-as.numeric(data3$IDARE_E_PUNTAJE)
data3$IDERE_E_PUNTAJE <-as.numeric(data3$IDERE_E_PUNTAJE)
data3$SHIPLEY <-as.numeric(data3$SHIPLEY)
data3$MoCA <-as.numeric(data3$MoCA)
data3$CRI_Total <- as.numeric(data3$CRI_Total)


#### GRAFICAS BASICAS ####
hist(data3$ESCOLARIDAD)
plot(data3$ESCOLARIDAD)
plot(data3$DECADA, data3$ESCOLARIDAD)

#### GRAFICAS BONITAS ####
install.packages("tidyverse")
library(tidyverse) 
source("summarySE.R")


ggplot(data3, aes(x = DECADA, y = ESCOLARIDAD, color=DECADA)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "purple")

filter(data, TR_RC == "TR", !VAL=="TOT") %>%
  summarySE(measurevar = "value", groupvars = c("COND", "DECADA")) %>%
  ggplot(aes(y=value, x=COND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")

filter(data, TR_RC == "TR", !VAL=="TOT") %>%
  ggplot(aes(x = DECADA, y = value, fill= DECADA)) + 
  geom_boxplot() + theme_classic()+ theme(legend.position = "none") +
  scale_fill_brewer(palette="Pastel1")


filter(data, TR_RC == "TR", !VAL=="TOT") %>%
  ggplot(aes(y=value, x=COND, fill= DECADA)) + theme_minimal() +
  scale_fill_brewer(palette="PuRd") +
  geom_boxplot() +
  geom_jitter(size=.4)


filter(data, TR_RC == "TR", !VAL=="TOT") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + geom_smooth(method = "lm", color = "#9F79EE", lwd = 1.5) +
  labs(x = "EDAD", y = "TIEMPOS DE REACCIÓN") +  
  theme_classic()+ 
  theme(text = element_text(size=15))


ggplot(data3, aes(x = EDAD, y = CRI_Total)) +
  geom_point() + geom_smooth(method = "lm", color = "#8B668B", lwd = 1.5) +
  labs(x = "EDAD", y = "CRI_Total") +  
  theme_classic()

filter(data, TR_RC == "TR", !VAL=="TOT", DECADA=="20")%>%
  ggplot(aes(x = EDAD, y = value, colour= VAL)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "EDAD", y = "TR", color = "VALENCIA") +
  theme_classic()+ 
  theme(text = element_text(size = 17))
