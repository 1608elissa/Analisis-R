library(readxl)
library(rstatix)
library(tidyverse)
library(purrr)
library(writexl)

data <- read_xlsx("Junto.xlsx",sheet = "GpoZ")


EDAD <- data$EDAD
MoCA <- data$MoCA
ESCOLARIDAD <- data$ESCOLARIDAD
CRI_Total	<- data$CRI_Total
IDARE_R	<- data$IDARE_R
IDARE_E	<- data$IDARE_E
IDERE_R	<- data$IDERE_R
IDERE_E	<- data$IDERE_E
SHIPLEY	<- data$SHIPLEY
SUEÑO_NOR	<- data$SUEÑO_NOR
SUEÑO_2DA	<- data$SUEÑO_2DA
OCUPA_Z <- data$OCUPA_Z


EDAD_CAT <- cut(EDAD, breaks = c(-Inf, mean(EDAD) - sd(EDAD), 
                                   mean(EDAD) + sd(EDAD), Inf), 
                  labels = c("LOW", "MEDIUM", "HIGH"))

MoCA_CAT <- cut(MoCA, breaks = c(-Inf, mean(MoCA) - sd(MoCA), 
                                 mean(MoCA) + sd(MoCA), Inf), 
                labels = c("LOW", "MEDIUM", "HIGH"))

ESCOLARIDAD_CAT <- cut(ESCOLARIDAD, breaks = c(-Inf, mean(ESCOLARIDAD) - sd(ESCOLARIDAD), 
                                 mean(ESCOLARIDAD) + sd(ESCOLARIDAD), Inf), 
                labels = c("LOW", "MEDIUM", "HIGH"))

CRI_Total_CAT <- cut(CRI_Total, breaks = c(-Inf, mean(CRI_Total) - sd(CRI_Total), 
                                 mean(CRI_Total) + sd(CRI_Total), Inf), 
                labels = c("LOW", "MEDIUM", "HIGH"))

IDARE_R_CAT <- cut(IDARE_R, breaks = c(-Inf, mean(IDARE_R) - sd(IDARE_R), 
                                 mean(IDARE_R) + sd(IDARE_R), Inf), 
                labels = c("LOW", "MEDIUM", "HIGH"))

IDARE_E_CAT <- cut(IDARE_E, breaks = c(-Inf, mean(IDARE_E) - sd(IDARE_E), 
                                       mean(IDARE_E) + sd(IDARE_E), Inf), 
                   labels = c("LOW", "MEDIUM", "HIGH"))

IDERE_R_CAT <- cut(IDERE_R, breaks = c(-Inf, mean(IDERE_R) - sd(IDERE_R), 
                                       mean(IDERE_R) + sd(IDERE_R), Inf), 
                   labels = c("LOW", "MEDIUM", "HIGH"))

IDERE_E_CAT <- cut(IDERE_E, breaks = c(-Inf, mean(IDERE_E) - sd(IDERE_E), 
                                       mean(IDERE_E) + sd(IDERE_E), Inf), 
                   labels = c("LOW", "MEDIUM", "HIGH"))

SHIPLEY_CAT <- cut(SHIPLEY, breaks = c(-Inf, mean(SHIPLEY) - sd(SHIPLEY), 
                                 mean(SHIPLEY) + sd(SHIPLEY), Inf), 
                labels = c("LOW", "MEDIUM", "HIGH"))

SUEÑO_NOR_CAT <- cut(SUEÑO_NOR, breaks = c(-Inf, mean(SUEÑO_NOR) - sd(SUEÑO_NOR), 
                                 mean(SUEÑO_NOR) + sd(SUEÑO_NOR), Inf), 
                labels = c("LOW", "MEDIUM", "HIGH"))

SUEÑO_2DA_CAT <- cut(SUEÑO_2DA, breaks = c(-Inf, mean(SUEÑO_2DA) - sd(SUEÑO_2DA), 
                                           mean(SUEÑO_2DA) + sd(SUEÑO_2DA), Inf), 
                     labels = c("LOW", "MEDIUM", "HIGH"))

OCUPA_Z_CAT <- cut(OCUPA_Z, breaks = c(-Inf, mean(OCUPA_Z) - sd(OCUPA_Z), 
                                 mean(OCUPA_Z) + sd(OCUPA_Z), Inf), 
                labels = c("LOW", "MEDIUM", "HIGH"))



Prueba <- data.frame(EDAD_CAT, MoCA_CAT, ESCOLARIDAD_CAT, CRI_Total_CAT, 
                     IDARE_R_CAT, IDARE_E_CAT, IDERE_R_CAT, IDERE_E_CAT, 
                     SHIPLEY_CAT, SUEÑO_NOR_CAT, SUEÑO_2DA_CAT,OCUPA_Z_CAT)

write_xlsx(Prueba, "Prueba.xlsx")
