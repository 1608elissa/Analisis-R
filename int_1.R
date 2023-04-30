#librerias####
library(tidyverse)


#######preconstucto de tabla####
library(readxl)
dat <- read_excel("mineria de datos/conjunto_datos25abril2023.xlsx")
a<-dat$AB17 %>%
  cut(15)

levels(a)
levels(a) <- seq(length(levels(a)))
tab<-tibble("AB17"=a,
            "2"=dat$AB17)
view(tab)           


table(cut(a, 15) )

library(openxlsx)

wb<-createWorkbook("cosito")
addWorksheet(wb,"labels")






#####guardar en excel####
writeDataTable(wb, "labels", tab)
openXL(wb)
saveWorkbook(wb, "labes.xlsx", overwrite = TRUE)




#Tarea de Eli pequeña####

#1. hacer lista con variables a modificar o lista de variables a no modificar
#2. modificar las variables en el excel para poder generar rangos
#   de manera automatica y no queden lejos del ultimo valor perdido

