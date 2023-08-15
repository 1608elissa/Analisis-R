library(readxl)
library(rstatix)
library(tidyverse)
library(car)

#### BASE DE DATOS ####
data <- read_xlsx("Junto.xlsx",sheet = "IXCHEL")%>%
  gather(cond_tip_est_val, value, -c("ID","DECADA","SEXO", "EDAD", "MoCA", 
                                     "ESCOLARIDAD", "CRI_Total","EDIMBURGO","IDARE_R_PUNTAJE", "IDARE_R_NIVEL",  
                                     "IDARE_E_PUNTAJE", "IDARE_E_NIVEL", "IDERE_R_PUNTAJE","IDERE_R_NIVEL", 
                                     "IDERE_E_PUNTAJE", "IDERE_E_NIVEL","COVID_CAT", "SHIPLEY",
                                     "SUEÑO_NOR","SUEÑO_2DA","OCUPA_CAT")) %>%
  separate(cond_tip_est_val, c("TIPO","COND","VAL"), sep = "_")

data %>%
  mutate(DECADA = as.factor(DECADA),
         SEXO = as.factor(SEXO),
         COND = as.factor(COND),
         VAL = as.factor(VAL),
         TIPO = as.factor(TIPO))


### CREACION DE FILTROS PARA VARIBALES QUE SE USARAN ####
a<- filter(data, COND=="ROSTROS", VAL=="TOT", DECADA=="20")
b<- filter(data, COND=="ROSTROS", VAL=="TOT", DECADA=="60")
c<- filter(data, COND=="ROSTROS", VAL=="TOT", DECADA %in% c("20", "60"))
d<- filter(data, COND=="ROSTROS", !VAL=="TOT", DECADA %in% c("20", "60"))

### PRUEBAS DE NORMALIDAD Y HOMOGENEIDAD PARA SABER SI SE PUEDE USAR PARAMETRICA ####
shapiro.test(c$value)
leveneTest(y = c$value , group = c$DECADA, center = "median")

### EVALUA LAS DIFERENCIAS EN MEMORIA DE TRABAJO PARA TUS GRUPOS ####
wilcox.test(a$value, b$value, paired = FALSE)

### EVALUA LAS DIFERENCIAS EN ANSIEDAD PARA TUS GRUPOS ####
wilcox.test(a$IDARE_R_NIVEL, b$IDARE_R_NIVEL, paired = FALSE)
wilcox.test(a$IDARE_E_NIVEL, b$IDARE_E_NIVEL, paired = FALSE)

### EVALUA LAS DIFERENCIAS EN DEPRESION PARA TUS GRUPOS ####
wilcox.test(a$IDERE_R_NIVEL, b$IDERE_R_NIVEL, paired = FALSE)
wilcox.test(a$IDERE_E_NIVEL, b$IDERE_E_NIVEL, paired = FALSE)

### REGRESIONES LINEALES ####
lm(formula = value ~ IDARE_R_PUNTAJE, data = c)%>%
  summary()
lm(formula = value ~ IDARE_E_PUNTAJE, data = c)%>%
  summary()
lm(formula = value ~ IDERE_R_PUNTAJE, data = c)%>%
  summary()
lm(formula = value ~ IDERE_E_PUNTAJE, data = c)%>%
  summary()

### CORRELACIONES ####
cor.test(c$value, c$IDARE_R_PUNTAJE, method="spearman")
cor.test(c$value, c$IDARE_E_PUNTAJE, method="spearman")
cor.test(c$value, c$IDERE_R_PUNTAJE, method="spearman")
cor.test(c$value, c$IDERE_E_PUNTAJE, method="spearman")

### ANOVA ####
aov(value ~ VAL*DECADA, data=d)%>%
  summary()

aov(value ~ VAL*DECADA, data=d)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()


### MEDIAS ####
library(psych)
library(magrittr)

filter(data, COND=="ROSTROS", !VAL=="TOT", DECADA %in% c("20", "60")) %$%
  describeBy(value, VAL)

filter(data, COND=="ROSTROS", !VAL=="TOT", DECADA %in% c("20", "60")) %$%
  describeBy(value, DECADA)

filter(data, COND=="ROSTROS", !VAL=="TOT", DECADA=="20") %$%
  describeBy(value, VAL)

filter(data, COND=="ROSTROS", !VAL=="TOT", DECADA=="60") %$%
  describeBy(value, VAL)
