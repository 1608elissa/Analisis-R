library(readxl)
library(rstatix)
library(tidyverse)
library(magrittr)


#### BASE DE DATOS CORRELACIONES ####
data <- read_xlsx("Junto.xlsx",sheet = "Correlaciones")%>%
  gather(cond_tip_est_val, value, -c("ID","DECADA", "SEXO", "EDAD", "MoCA", "ESCOLARIDAD", "ESCOL_CAT", 
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

# PORCENTAJE DE RESPUESTAS CORRECTAS ####

filter(data, VD == "RC", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_DUR_ROS,
            method="pearson")
filter(data, VD == "RC", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_DUR_ESC,
            method="pearson")
filter(data, VD == "RC", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_DUR_ROS,
            method="pearson")
filter(data, VD == "RC", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_DUR_ESC,
            method="pearson")
filter(data, VD == "RC", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_1DU_ROS,
            method="pearson")
filter(data, VD == "RC", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_1DU_ESC,
            method="pearson")
filter(data, VD == "RC", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_1DU_ROS,
            method="pearson")
filter(data, VD == "RC", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_1DU_ESC,
            method="pearson")
filter(data, VD == "RC", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, ESCOLARIDAD,
            method="pearson")
filter(data, VD == "RC", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, ESCOLARIDAD,
           method="pearson")
filter(data, VD == "RC", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, EDAD,
            method="pearson")
filter(data, VD == "RC", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, MoCA,
           method="spearman")
filter(data, VD == "RC", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, CRI_Total,
            method="spearman")
filter(data, VD == "RC", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, IDARE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "RC", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, IDERE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "RC", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, COVID_CAT,
            method="spearman")

filter(data, VD == "RC", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_DUR_ROS,
            method="pearson")
filter(data, VD == "RC", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_DUR_ESC,
            method="pearson")
filter(data, VD == "RC", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_DUR_ROS,
            method="pearson")
filter(data, VD == "RC", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_DUR_ESC,
            method="pearson")
filter(data, VD == "RC", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_1DU_ROS,
            method="pearson")
filter(data, VD == "RC", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_1DU_ESC,
            method="pearson")
filter(data, VD == "RC", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_1DU_ROS,
            method="pearson")
filter(data, VD == "RC", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_1DU_ESC,
            method="pearson")
filter(data, VD == "RC", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, ESCOLARIDAD,
            method="pearson")
filter(data, VD == "RC", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, EDAD,
            method="pearson")
filter(data, VD == "RC", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, MoCA,
            method="spearman")
filter(data, VD == "RC", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, CRI_Total,
            method="spearman")
filter(data, VD == "RC", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, IDARE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "RC", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, IDERE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "RC", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, COVID_CAT,
            method="spearman")

# TIEMPOS DE REACCIÓN ####

filter(data, VD == "TR", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_DUR_ROS,
            method="pearson")
filter(data, VD == "TR", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_DUR_ESC,
            method="pearson")
filter(data, VD == "TR", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_DUR_ROS,
            method="pearson")
filter(data, VD == "TR", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_DUR_ESC,
            method="pearson")
filter(data, VD == "TR", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_1DU_ROS,
            method="pearson")
filter(data, VD == "TR", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_1DU_ESC,
            method="pearson")
filter(data, VD == "TR", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_1DU_ROS,
            method="pearson")
filter(data, VD == "TR", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_1DU_ESC,
            method="pearson")
filter(data, VD == "TR", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, ESCOLARIDAD,
            method="pearson")
filter(data, VD == "TR", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, ESCOLARIDAD,
            method="pearson")
filter(data, VD == "TR", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, EDAD,
            method="pearson")
filter(data, VD == "TR", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, MoCA,
            method="spearman")
filter(data, VD == "TR", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, CRI_Total,
            method="spearman")
filter(data, VD == "TR", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, IDARE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "TR", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, IDERE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "TR", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, COVID_CAT,
            method="spearman")

filter(data, VD == "TR", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_DUR_ROS,
            method="pearson")
filter(data, VD == "TR", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_DUR_ESC,
            method="pearson")
filter(data, VD == "TR", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_DUR_ROS,
            method="pearson")
filter(data, VD == "TR", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_DUR_ESC,
            method="pearson")
filter(data, VD == "TR", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_1DU_ROS,
            method="pearson")
filter(data, VD == "TR", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_1DU_ESC,
            method="pearson")
filter(data, VD == "TR", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_1DU_ROS,
            method="pearson")
filter(data, VD == "TR", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_1DU_ESC,
            method="pearson")
filter(data, VD == "TR", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, ESCOLARIDAD,
            method="pearson")
filter(data, VD == "TR", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, EDAD,
            method="pearson")
filter(data, VD == "TR", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, MoCA,
            method="spearman")
filter(data, VD == "TR", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, CRI_Total,
            method="spearman")
filter(data, VD == "TR", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, IDARE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "TR", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, IDERE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "TR", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, COVID_CAT,
            method="spearman")

# INDICE DE EFICIENCIA INVERSA ####

a <- filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG")

b <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = a)

step(object = b, direction = "both", trace = 1)

modelo <- (lm(formula = value ~ EDAD + MoCA + CRI_Total + EDAD:CRI_Total + 
                MoCA:CRI_Total, data = a))
summary(modelo)

confint(lm(formula = value ~ EDAD + MoCA + CRI_Total + EDAD:CRI_Total + 
             MoCA:CRI_Total, data = a))

filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %>%
  lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data =.)%>%
  summary()


plot1 <- ggplot(data = a, aes(EDAD, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = a, aes(MoCA, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = a, aes(CRI_Total, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

grid.arrange(plot1, plot2, plot3)


filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_DUR_ROS,
            method="pearson")
filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_DUR_ESC,
            method="pearson")
filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_DUR_ROS,
            method="pearson")
filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_DUR_ESC,
            method="pearson")
filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_1DU_ROS,
            method="pearson")
filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_1DU_ESC,
            method="pearson")
filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_1DU_ROS,
            method="pearson")
filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_1DU_ESC,
            method="pearson")
filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, ESCOLARIDAD,
            method="pearson")
filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, ESCOLARIDAD,
            method="pearson")
filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, EDAD,
            method="pearson")
filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %>%
  lm(formula= value ~ EDAD, data =.)%>%
  summary()

filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %>%
  lm(formula= value ~ EDAD*ESCOLARIDAD, data =.)%>%
  summary()

filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "EI") +  
  geom_smooth(method = "lm", se = FALSE, color= "violet") +
  theme_classic()

filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, MoCA,
            method="spearman")
filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, CRI_Total,
            method="spearman")
filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, IDARE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, IDERE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, COVID_CAT,
            method="spearman")

filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_DUR_ROS,
            method="pearson")
filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_DUR_ESC,
            method="pearson")
filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_DUR_ROS,
            method="pearson")
filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_DUR_ESC,
            method="pearson")
filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_1DU_ROS,
            method="pearson")
filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, AMP_1DU_ESC,
            method="pearson")
filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_1DU_ROS,
            method="pearson")
filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, SUP_1DU_ESC,
            method="pearson")
filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, ESCOLARIDAD,
            method="pearson")
filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, EDAD,
            method="pearson")
filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, MoCA,
            method="spearman")
filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, CRI_Total,
            method="spearman")
filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, IDARE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, IDERE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %$%
  cor.test( value, COVID_CAT,
            method="spearman")

# D PRIMA ####

filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, AMP_DUR_ROS,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, AMP_DUR_ESC,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, SUP_DUR_ROS,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, SUP_DUR_ESC,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, AMP_1DU_ROS,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, AMP_1DU_ESC,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, SUP_1DU_ROS,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, SUP_1DU_ESC,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, ESCOLARIDAD,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, ESCOLARIDAD,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, EDAD,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, MoCA,
            method="spearman")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, CRI_Total,
            method="spearman")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, IDARE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, IDERE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, COVID_CAT,
            method="spearman")

filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, AMP_DUR_ROS,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, AMP_DUR_ESC,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, SUP_DUR_ROS,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, SUP_DUR_ESC,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, AMP_1DU_ROS,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, AMP_1DU_ESC,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, SUP_1DU_ROS,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, SUP_1DU_ESC,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, ESCOLARIDAD,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, EDAD,
            method="pearson")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, MoCA,
            method="spearman")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, CRI_Total,
            method="spearman")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, IDARE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, IDERE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, COVID_CAT,
            method="spearman")

# EI NUEVO CON D PRIMA ####

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, AMP_DUR_ROS,
            method="spearman")
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, SUP_DUR_ROS,
            method="spearman")
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, ESCOLARIDAD,
            method="pearson")
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, ESCOLARIDAD,
            method="spearman")
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, EDAD,
            method="pearson")
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, MoCA,
            method="spearman")
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, CRI_Total,
            method="spearman")
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, IDARE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, IDERE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  cor.test( value, COVID_CAT,
            method="spearman")


filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, AMP_DUR_ROS,
            method="spearman")
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, SUP_DUR_ROS,
            method="spearman")
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, ESCOLARIDAD,
            method="pearson")
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, EDAD,
            method="pearson")
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, MoCA,
            method="spearman")
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, CRI_Total,
            method="spearman")
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, IDARE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, IDERE_R_PUNTAJE,
            method="spearman")
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  cor.test( value, COVID_CAT,
            method="spearman")

##### EDAD Y CRIQ ####
cor.test(data$EDAD, data$CRI_Total, method="pearson")
cor.test(data$EDAD, data$ESCOLARIDAD, method="pearson")

