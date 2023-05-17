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
