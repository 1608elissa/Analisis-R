library(plotrix)
library(readxl)
library(rstatix)
library(tidyverse)


data3 <- read_xlsx("Demograficos.xlsx",sheet = "All")


data3$DECADA <- as.factor(data3$DECADA)
data3$ESCOLARIDAD <-as.numeric(data3$ESCOLARIDAD)
data3$IDARE_R_PUNTAJE <-as.numeric(data3$IDARE_R_PUNTAJE)
data3$IDERE_R_PUNTAJE <-as.numeric(data3$IDERE_R_PUNTAJE)
data3$IDARE_E_PUNTAJE <-as.numeric(data3$IDARE_E_PUNTAJE)
data3$IDERE_E_PUNTAJE <-as.numeric(data3$IDERE_E_PUNTAJE)
data3$SHIPLEY_TOT <-as.numeric(data3$SHIPLEY_TOT)
data3$MoCA <-as.numeric(data3$MoCA)


anova_test(SHIPLEY_TOT~DECADA, data=data3)
anova_test(ESCOLARIDAD~DECADA, data=data3)
anova_test(EDAD~DECADA, data=data3)

std.error(data3$SHIPLEY_TOT)
std.error(data3$ESCOLARIDAD)
std.error(data3$MoCA)
std.error(data3$EDAD)

kruskal_test(MoCA~DECADA, data=data3)
kruskal_test(IDARE_R_PUNTAJE~DECADA, data=data3)
kruskal_test(IDERE_R_PUNTAJE~DECADA, data=data3)
kruskal_test(IDARE_E_PUNTAJE~DECADA, data=data3)
kruskal_test(IDERE_E_PUNTAJE~DECADA, data=data3)

range(data3$IDAREESTADO, na.rm = T)
range(data3$IDARERASGO, na.rm = T)
range(data3$IDEREESTADO, na.rm = T)
range(data3$IDERERASGO, na.rm = T)



decadas<-c("20","30","40","50","60")


for (i in decadas) {
  bases<- filter(data3,DECADA== i)
  print(i) 
  std.error(bases$SHIPLEY_TOT)  %>%
  print()
  std.error(bases$ESCOLARIDAD)  %>%
  print()
  range(bases$MoCA) %>%
  print()
  std.error(bases$EDAD) %>%
  print()
  range(bases$IDARE_R_PUNTAJE, na.rm = T) %>%
  print()
  range(bases$IDERE_R_PUNTAJE, na.rm = T) %>%
  print()
  range(bases$IDARE_E_PUNTAJE, na.rm = T) %>%
  print()
  range(bases$IDERE_E_PUNTAJE, na.rm = T) %>%
  print()
 #print(bases) 
}
 
for (i in decadas) {
  bases<- filter(data3,DECADA== i)
  print(i) 
  mean(bases$SHIPLEY_TOT)  %>%
    print()
  mean(bases$ESCOLARIDAD)  %>%
    print()
  median(bases$MoCA) %>%
    print()
  mean(bases$EDAD) %>%
    print()
  median(bases$IDARE_R_PUNTAJE, na.rm = T) %>%
    print()
  median(bases$IDERE_R_PUNTAJE, na.rm = T) %>%
    print()
  median(bases$IDARE_E_PUNTAJE, na.rm = T) %>%
    print()
  median(bases$IDERE_E_PUNTAJE, na.rm = T) %>%
    print()
  #print(bases) 
}





