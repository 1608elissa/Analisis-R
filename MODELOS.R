library(readxl)
library(rstatix)
library(tidyverse)
library(magrittr)


#### BASE DE DATOS CORRELACIONES ####
data <- read_xlsx("Junto.xlsx",sheet = "Correlaciones")%>%
  gather(cond_tip_est_val, value, -c("ID","DECADA", "SEXO", "EDAD", "MoCA", "ESCOLARIDAD", 
                                     "CRI_Total", "IDARE_R_PUNTAJE", "IDERE_R_PUNTAJE",
                                     "COVID_CAT", "SUEÑO_NOR", "AMP_DUR_ROS", "AMP_DUR_ESC",
                                     "SUP_DUR_ROS", "SUP_DUR_ESC", "AMP_1DU_ROS",
                                     "AMP_1DU_ESC", "SUP_1DU_ROS", "SUP_1DU_ESC")) %>%
  separate(cond_tip_est_val, c("COND","VD","TIPO", "VAL"), sep = "_")

data$DECADA <- as.factor(data$DECADA)
data$SEXO <- as.factor(data$SEXO)
data$VD <- as.factor(data$VD)
data$COND <- as.factor(data$COND)
data$VAL <- as.factor(data$VAL)
data$TIPO <- as.factor(data$TIPO)


#### INDICE DE EFICIENCIA ####
a <- filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG")

b <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = a)

step(object = b, direction = "both", trace = 1)

modeloEIROS <- (lm(formula = value ~ EDAD + MoCA + CRI_Total + EDAD:CRI_Total + 
                MoCA:CRI_Total, data = a))
summary(modeloEIROS)


c <- filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG")

d <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = c)

step(object = d, direction = "both", trace = 1)

modeloEIESC <- (lm(formula = value ~ EDAD * ESCOLARIDAD * MoCA * CRI_Total, data = c))
summary(modeloEIESC)


#### INDICES DE AMPLIFICACION ####
e <- filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")

f <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = e)

step(object = f, direction = "both", trace = 1)

modeloAMPDUR <- (lm(formula = value ~ ESCOLARIDAD + MoCA + ESCOLARIDAD:MoCA, data = e))
summary(modeloAMPDUR)


#### INDICES DE SUPRESION ####
g <- filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")

h <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = g)

step(object = h, direction = "both", trace = 1)

modeloSUPDUR <- (lm(formula = value ~ EDAD + CRI_Total + EDAD:CRI_Total, data = g))
summary(modeloSUPDUR)

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %$%
  cor.test( value, EDAD,
            method="pearson")
filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %$%
  cor.test( value, CRI_Total,
            method="spearman")

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  ggplot(aes(x = EDAD , y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "SUP") +  
  geom_smooth(method = "lm", se = FALSE, color= "violet") +
  theme_classic()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  ggplot(aes(x = CRI_Total , y = value)) +
  geom_point() + 
  labs(x = "CRI_Total", y = "SUP")


#### D PRIMA ####
i <- filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")

j <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = i)

step(object = j, direction = "both", trace = 1)

modeloDPROS <- (lm(formula = value ~ EDAD + MoCA + CRI_Total + EDAD:MoCA + MoCA:CRI_Total, 
                   data = i))
summary(modeloDPROS)



k <- filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")

l <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = k)

step(object = l, direction = "both", trace = 1)

modeloDPESC <- (lm(formula = value ~ EDAD * ESCOLARIDAD * MoCA * CRI_Total, data = k))
summary(modeloDPESC)
