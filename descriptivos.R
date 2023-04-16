library(plotrix)
library(readxl)

ruta_archivo <- readxl_example("Demograficos.xlsx")
data3 <- read_excel("Demograficos.xlsx",sheet = "All")


data3$DECADA <- as.factor(data3$DECADA)
data3$ESCOLARIDAD <-as.numeric(data3$ESCOLARIDAD)
data3$IDEREESTADO <-as.numeric(data3$IDEREESTADO)
data3$IDERERASGO <-as.numeric(data3$IDERERASGO)


anova_test(SHIPLEY~DECADA, data=data3)
anova_test(ESCOLARIDAD~DECADA, data=data3)
anova_test(MOCA~DECADA, data=data3)
anova_test(EDAD~DECADA, data=data3)

std.error(data3$SHIPLEY)
std.error(data3$ESCOLARIDAD)
std.error(data3$MOCA)
std.error(data3$EDAD)

kruskal_test(IDAREESTADO~DECADA, data=data3)
kruskal_test(IDARERASGO~DECADA, data=data3)
kruskal_test(IDEREESTADO~DECADA, data=data3)
kruskal_test(IDERERASGO~DECADA, data=data3)

range(data3$IDAREESTADO, na.rm = T)
range(data3$IDARERASGO, na.rm = T)
range(data3$IDEREESTADO, na.rm = T)
range(data3$IDERERASGO, na.rm = T)



decadas<-c("20","30","40","50","60")


for (i in decadas) {
  bases<- filter(data3,DECADA== i)
  print(i) 
  std.error(bases$SHIPLEY)  %>%
  print()
  std.error(bases$ESCOLARIDAD)  %>%
  print()
  std.error(bases$MOCA) %>%
  print()
  std.error(bases$EDAD) %>%
  print()
  range(bases$IDAREESTADO, na.rm = T) %>%
  print()
  range(bases$IDARERASGO, na.rm = T) %>%
  print()
  range(bases$IDEREESTADO, na.rm = T) %>%
  print()
  range(bases$IDERERASGO, na.rm = T) %>%
  print()
 #print(bases) 
}
 





