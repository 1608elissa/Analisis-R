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


#pruebas####



library(readxl)
dat <- read_excel("mineria de datos/conjunto_datos25abril2023.xlsx")
dat$AC24est  %<>%  as.numeric()
a<-dat$AC24est %>%
  cut(10)
b<-a

levels(a)
levels(b) <- seq(length(levels(a)))
tab<-tibble("AC24est"=a,
            "1"=b,
            "2"=dat$AC24est)
view(tab)           
