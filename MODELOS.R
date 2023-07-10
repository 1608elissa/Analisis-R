library(readxl)
library(rstatix)
library(tidyverse)
library(magrittr)
library(gridExtra)

#### BASE DE DATOS CORRELACIONES ####
data <- read_xlsx("Junto.xlsx",sheet = "Correlaciones")%>%
  gather(cond_tip_est_val, value, -c("ID","DECADA", "SEXO", "EDAD","EDAD_CAT", "MoCA", 
                                     "MoCA_CAT","ESCOLARIDAD", "ESCOLARIDAD_CAT",
                                     "CRI_Total","CRI_Total_CAT","IDARE_R_PUNTAJE", "IDARE_R_CAT",  
                                     "IDARE_E_PUNTAJE", "IDARE_E_CAT", "IDERE_R_PUNTAJE",
                                     "IDERE_R_CAT", "IDERE_E_PUNTAJE", "IDERE_E_CAT",
                                     "COVID_CAT", "SHIPLEY","SHIPLEY_CAT","SUEÑO_NOR","SUEÑO_2DA",
                                     "OCUPACION","OCUPA_CAT","OCUPACION_CAT",
                                     "AMP_ROS","SUP_ROS")) %>%
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

plot1 <- ggplot(data = a, aes(CRI_Total, modeloEIROS$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = a, aes(MoCA, modeloEIROS$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

grid.arrange(plot1, plot2)

plot1 <- ggplot(data = c, aes(EDAD, modeloEIESC$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = c, aes(MoCA, modeloEIESC$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = c, aes(ESCOLARIDAD, modeloEIESC$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

grid.arrange(plot1, plot2, plot3)


#### INDICES DE AMPLIFICACION ####
e <- filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")

f <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = e)


filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  lm(formula= value ~ ESCOLARIDAD, data = .)%>%
  summary()
filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  lm(formula= value ~ MoCA, data = .)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  ggplot(aes(x = ESCOLARIDAD , y = value)) +
  geom_point() + 
  labs(x = "ESCOLARIDAD", y = "AMP") +  
  geom_smooth(method = "lm", se = FALSE, color= "violet") +
  theme_classic()

step(object = f, direction = "both", trace = 1)

modeloAMPDUR <- (lm(formula = value ~ ESCOLARIDAD + MoCA + ESCOLARIDAD:MoCA, data = e))
summary(modeloAMPDUR)

ggplot(data = e, aes(ESCOLARIDAD, modeloAMPDUR$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

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

ggplot(data = j, aes(CRI_Total, modeloDPROS$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()


filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() + 
  labs(x = "CRI_Total", y = "DPR") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()

ggplot(data = j, aes(x = CRI_Total, modeloDPROS$residuals)) +
  geom_point() + 
  geom_smooth(method = "lm", color= "firebrick") +
  geom_abline(yintercept = 0) +
  theme_classic()

k <- filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")

l <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = k)

step(object = l, direction = "both", trace = 1)

modeloDPESC <- (lm(formula = value ~ EDAD * ESCOLARIDAD * MoCA * CRI_Total, data = k))
summary(modeloDPESC)

plot1 <- ggplot(data = c, aes(EDAD, modeloDPESC$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = c, aes(ESCOLARIDAD, modeloDPESC$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

grid.arrange(plot1, plot2)



m <- filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")

n <- lm(formula= value ~ EDAD+ESCOLARIDAD+MoCA+CRI_Total, data = m)

step(object = n, direction = "both", trace = 1)

modeloEIDPROS <- (lm(formula = value ~ EDAD + MoCA + CRI_Total, data = m))
summary(modeloEIDPROS)


filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "EIDP") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() + 
  labs(x = "MoCA", y = "EIDP") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() + 
  labs(x = "CRI_Total", y = "EIDP") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()


o <- filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")

p <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = o)

step(object = p, direction = "both", trace = 1)

modeloEIDPESC <- (lm(formula = value ~ EDAD * ESCOLARIDAD * MoCA * CRI_Total, data = o))
summary(modeloEIDPESC)


filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "EIDP") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() + 
  labs(x = "ESCOLARIDAD", y = "EIDP") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()
