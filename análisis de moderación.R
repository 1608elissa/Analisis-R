library(readxl)
library(rstatix)
library(tidyverse)

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


#### AMPLIFICACION ANALISIS CON ATENDER ROSTROS ####
### EDAD ###
filter(data, EDAD_CAT=="ALTO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.) -> AMPaltoEDAD

filter(data, EDAD_CAT=="BAJO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.) -> AMPbajoEDAD

stargazer::stargazer(AMPaltoEDAD, AMPbajoEDAD, type="text", df=FALSE, column.labels=c("Alto","Bajo"), report = "vct*p")

filter(data, !EDAD_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.)%>%
  summary()

### ESCOLARIDAD_CAT ###
filter(data, ESCOLARIDAD_CAT=="ALTO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.) -> AMPaltoESCO

filter(data, ESCOLARIDAD_CAT=="BAJO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.) -> AMPbajoESCO

stargazer::stargazer(AMPaltoESCO, AMPbajoESCO, type="text", df=FALSE, column.labels=c("Alto","Bajo"), report = "vct*p")

filter(data, !EDAD_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.)%>%
  summary()

### MoCA_CAT ###
filter(data, MoCA_CAT=="ALTO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.) -> AMPaltoMOCA

filter(data, MoCA_CAT=="BAJO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.) -> AMPbajoMOCA

stargazer::stargazer(AMPaltoMOCA, AMPbajoMOCA, type="text", df=FALSE, column.labels=c("Alto","Bajo"), report = "vct*p")

filter(data, !MoCA_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.)%>%
  summary()

### CRI_Total_CAT ###
filter(data, CRI_Total_CAT=="ALTO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.) -> AMPaltoCRI

filter(data, CRI_Total_CAT=="BAJO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.) -> AMPbajoCRI

stargazer::stargazer(AMPaltoCRI, AMPbajoCRI, type="text", df=FALSE, column.labels=c("Alto","Bajo"), report = "vct*p")

filter(data, !CRI_Total_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.)%>%
  summary()

### IDARE_R_CAT ###
filter(data, IDARE_R_CAT=="ALTO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.) -> AMPaltoIDARE

filter(data, IDARE_R_CAT=="BAJO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.) -> AMPbajoIDARE

stargazer::stargazer(AMPaltoIDARE, AMPbajoIDARE, type="text", df=FALSE, column.labels=c("Alto","Bajo"), report = "vct*p")

filter(data, !IDARE_R_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.)%>%
  summary()

### IDERE_R_CAT ###
filter(data, IDERE_R_CAT=="ALTO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.) -> AMPaltoIDERE

filter(data, IDERE_R_CAT=="BAJO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.) -> AMPbajoIDERE

stargazer::stargazer(AMPaltoIDERE, AMPbajoIDERE, type="text", df=FALSE, column.labels=c("Alto","Bajo"), report = "vct*p")

filter(data, !IDERE_R_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.)%>%
  summary()

### OCUPACION_CAT ###
filter(data, OCUPACION_CAT=="ALTO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.) -> AMPaltoOCUP

filter(data, OCUPACION_CAT=="BAJO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.) -> AMPbajoOCUP

stargazer::stargazer(AMPaltoOCUP, AMPbajoOCUP, type="text", df=FALSE, column.labels=c("Alto","Bajo"), report = "vct*p")

filter(data, !OCUPACION_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS , data =.)%>%
  summary()

#### SUPRESION ANALISIS CON ATENDER ESCENAS ####
### EDAD ###
filter(data, EDAD_CAT=="ALTO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.) -> SUPaltoEDAD

filter(data, EDAD_CAT=="BAJO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.) -> SUPbajoEDAD

stargazer::stargazer(SUPaltoEDAD, SUPbajoEDAD, type="text", df=FALSE, column.labels=c("Alto","Bajo"), report = "vct*p")

filter(data, !EDAD_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.)%>%
  summary()

### ESCOLARIDAD_CAT ###
filter(data, ESCOLARIDAD_CAT=="ALTO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.) -> SUPaltoESCO

filter(data, ESCOLARIDAD_CAT=="BAJO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.) -> SUPbajoESCO

stargazer::stargazer(SUPaltoESCO, SUPbajoESCO, type="text", df=FALSE, column.labels=c("Alto","Bajo"), report = "vct*p")

filter(data, !ESCOLARIDAD_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.)%>%
  summary()

### MoCA_CAT ###
filter(data, MoCA_CAT=="ALTO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.) -> SUPaltoMOCA

filter(data, MoCA_CAT=="BAJO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.) -> SUPbajoMOCA

stargazer::stargazer(SUPaltoMOCA, SUPbajoMOCA, type="text", df=FALSE, column.labels=c("Alto","Bajo"), report = "vct*p")

filter(data, !MoCA_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.)%>%
  summary()

### CRI_Total_CAT ###
filter(data, CRI_Total_CAT=="ALTO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.) -> SUPaltoCRI

filter(data, CRI_Total_CAT=="BAJO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.) -> SUPbajoCRI

stargazer::stargazer(SUPaltoCRI, SUPbajoCRI, type="text", df=FALSE, column.labels=c("Alto","Bajo"), report = "vct*p")

filter(data, !CRI_Total_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.)%>%
  summary()

a<-filter(data, CRI_Total_CAT=="ALTO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")
b<-filter(data, CRI_Total_CAT=="BAJO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")

wilcox.test(a$value,b$value)

c<-filter(data, CRI_Total_CAT=="ALTO", VD == "DUR", COND== "SUP", VAL=="TOT", TIPO=="ESC")
d<-filter(data, CRI_Total_CAT=="BAJO", VD == "DUR", COND== "SUP", VAL=="TOT", TIPO=="ESC")

wilcox.test(c$value,d$value)


filter(data, !CRI_Total_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %>%
  ggplot(aes(y = value, x = CRI_Total_CAT, color=CRI_Total_CAT)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "#473C8B")

filter(data, !CRI_Total_CAT=="MEDIO", VD == "DUR", COND== "SUP", VAL=="TOT", TIPO=="ESC") %>%
  ggplot(aes(y = value, x = CRI_Total_CAT, color=CRI_Total_CAT)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "#473C8B")


### IDARE_R_CAT ###
filter(data, IDARE_R_CAT=="ALTO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.) -> SUPaltoIDARE

filter(data, IDARE_R_CAT=="BAJO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.) -> SUPbajoIDARE

stargazer::stargazer(SUPaltoIDARE, SUPbajoIDARE, type="text", df=FALSE, column.labels=c("Alto","Bajo"), report = "vct*p")

filter(data, !IDARE_R_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.)%>%
  summary()

### IDERE_R_CAT ###
filter(data, IDERE_R_CAT=="ALTO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.) -> SUPaltoIDERE

filter(data, IDERE_R_CAT=="BAJO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.) -> SUPbajoIDERE

stargazer::stargazer(SUPaltoIDERE, SUPbajoIDERE, type="text", df=FALSE, column.labels=c("Alto","Bajo"), report = "vct*p")

filter(data, !IDERE_R_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.)%>%
  summary()

### OCUPACION_CAT ###
filter(data, OCUPACION_CAT=="ALTO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.) -> SUPaltoOCUP

filter(data, OCUPACION_CAT=="BAJO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.) -> SUPbajoOCUP

stargazer::stargazer(SUPaltoOCUP, SUPbajoOCUP, type="text", df=FALSE, column.labels=c("Alto","Bajo"), report = "vct*p")

filter(data, !OCUPACION_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS , data =.)%>%
  summary()

#### GRAFICAS PARA AMPLIFICACION ROSTROS ####
data %>%
  mutate(IDERE_R_CAT = as.factor(IDERE_R_CAT),
         IDARE_R_CAT = as.factor(IDARE_R_CAT),
         CRI_Total_CAT = as.factor(CRI_Total_CAT),
         EDAD_CAT = as.factor(EDAD_CAT),
         ESCOLARIDAD_CAT = as.factor(ESCOLARIDAD_CAT),
         MoCA_CAT = as.factor(MoCA_CAT))

### EDAD ###
filter(data, !EDAD_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = AMP_ROS, y = value, colour = EDAD_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AMPLIFICACION", y = "EI") +  
  theme_classic()
  
### ESCOLARIDAD_CAT ###
filter(data, !ESCOLARIDAD_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = AMP_ROS, y = value, colour = ESCOLARIDAD_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AMPLIFICACION", y = "EI") +  
  theme_classic()

### MoCA_CAT ###
filter(data, !MoCA_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = AMP_ROS, y = value, colour = MoCA_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AMPLIFICACION", y = "EI") +  
  theme_classic()
  
### CRI_Total_CAT ###
filter(data, !CRI_Total_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = AMP_ROS, y = value, colour = CRI_Total_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AMPLIFICACION", y = "EI") +  
  theme_classic()
  
### IDARE_R_CAT ###
filter(data, !IDARE_R_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = AMP_ROS, y = value, colour = IDARE_R_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AMPLIFICACION", y = "EI") +  
  theme_classic()
  
### IDERE_R_CAT ###
filter(data, !IDERE_R_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = AMP_ROS, y = value, colour = IDERE_R_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AMPLIFICACION", y = "EI") +  
  theme_classic()

### OCUPACION_CAT ###
filter(data, !OCUPACION_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = AMP_ROS, y = value, colour = OCUPACION_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AMPLIFICACION", y = "EI") +  
  theme_classic()

#### GRAFICAS PARA SUPRESION ESCENAS ####

### EDAD ###
filter(data, !EDAD_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  ggplot(aes(x = SUP_ROS, y = value, colour = EDAD_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "SUPRESION", y = "EI") +  
  theme_classic()

### ESCOLARIDAD_CAT ###
filter(data, !ESCOLARIDAD_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  ggplot(aes(x = SUP_ROS, y = value, colour = ESCOLARIDAD_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "SUPRESION", y = "EI") +  
  theme_classic()

### MoCA_CAT ###
filter(data, !MoCA_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  ggplot(aes(x = SUP_ROS, y = value, colour = MoCA_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "SUPRESION", y = "EI") +  
  theme_classic()

### CRI_Total_CAT ###
filter(data, !CRI_Total_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  ggplot(aes(x = SUP_ROS, y = value, colour = CRI_Total_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "SUPRESION", y = "EI") +  
  theme_classic()

### IDARE_R_CAT ###
filter(data, !IDARE_R_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  ggplot(aes(x = SUP_ROS, y = value, colour = IDARE_R_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "SUPRESION", y = "EI") +  
  theme_classic()

### IDERE_R_CAT ###
filter(data, !IDERE_R_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  ggplot(aes(x = SUP_ROS, y = value, colour = IDERE_R_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "SUPRESION", y = "EI") +  
  theme_classic()

### OCUPACION_CAT ###
filter(data, !OCUPACION_CAT=="MEDIO", VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  ggplot(aes(x = SUP_ROS, y = value, colour = OCUPACION_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "SUPRESION", y = "EI") +  
  theme_classic()