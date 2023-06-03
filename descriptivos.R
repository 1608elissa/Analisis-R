library(plotrix)
library(readxl)
library(rstatix)
library(tidyverse)
library(pgirmess)


data3 <- read_xlsx("Junto.xlsx",sheet = "Demo")


data3$DECADA <- as.factor(data3$DECADA)
data3$ESCOLARIDAD <-as.numeric(data3$ESCOLARIDAD)
data3$IDARE_R_PUNTAJE <-as.numeric(data3$IDARE_R_PUNTAJE)
data3$IDERE_R_PUNTAJE <-as.numeric(data3$IDERE_R_PUNTAJE)
data3$IDARE_E_PUNTAJE <-as.numeric(data3$IDARE_E_PUNTAJE)
data3$IDERE_E_PUNTAJE <-as.numeric(data3$IDERE_E_PUNTAJE)
data3$IDARE_R_NIVEL <-as.numeric(data3$IDARE_R_NIVEL)
data3$IDERE_R_NIVEL <-as.numeric(data3$IDERE_R_NIVEL)
data3$IDARE_E_NIVEL <-as.numeric(data3$IDARE_E_NIVEL)
data3$IDERE_E_NIVEL <-as.numeric(data3$IDERE_E_NIVEL)
data3$SHIPLEY <-as.numeric(data3$SHIPLEY)
data3$MoCA <-as.numeric(data3$MoCA)
data3$CRI_Total <- as.numeric(data3$CRI_Total)


anova_test(SHIPLEY~DECADA, data=data3)
anova_test(CRI_Total~DECADA, data=data3)
aov(CRI_Total~DECADA, data=data3)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()
anova_test(ESCOLARIDAD~DECADA, data=data3)
aov(ESCOLARIDAD~DECADA, data=data3)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()
anova_test(EDAD~DECADA, data=data3)

kruskal_test(MoCA~DECADA, data=data3)
kruskalmc(MoCA~DECADA, data=data3)
kruskal_test(IDARE_R_PUNTAJE~DECADA, data=data3)
kruskalmc(IDARE_R_PUNTAJE~DECADA, data=data3)
kruskal_test(IDERE_R_PUNTAJE~DECADA, data=data3)
kruskalmc(IDERE_R_PUNTAJE~DECADA, data=data3)
kruskal_test(IDARE_E_PUNTAJE~DECADA, data=data3)
kruskal_test(IDERE_E_PUNTAJE~DECADA, data=data3)
kruskalmc(IDERE_E_PUNTAJE~DECADA, data=data3)
kruskal_test(IDARE_R_NIVEL~DECADA, data=data3)
kruskal_test(IDERE_R_NIVEL~DECADA, data=data3)
kruskalmc(IDERE_R_NIVEL~DECADA, data=data3)
kruskal_test(IDARE_E_NIVEL~DECADA, data=data3)
kruskal_test(IDERE_E_NIVEL~DECADA, data=data3)
kruskalmc(IDERE_E_NIVEL~DECADA, data=data3)


decadas<-c("20","30","40","50","60")


for (i in decadas) {
  bases<- filter(data3,DECADA== i)
  print(i) 
  std.error(bases$SHIPLEY)  %>%
  print()
  std.error(bases$ESCOLARIDAD)  %>%
  print()
  std.error(bases$CRI_Total)  %>%
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
  range(bases$IDARE_R_NIVEL, na.rm = T) %>%
  print()
  range(bases$IDERE_R_NIVEL, na.rm = T) %>%
  print()
  range(bases$IDARE_E_NIVEL, na.rm = T) %>%
  print()
  range(bases$IDERE_E_NIVEL, na.rm = T) %>%
  print()
 #print(bases) 
  }
 
for (i in decadas) {
  bases<- filter(data3,DECADA== i)
  print(i) 
  mean(bases$SHIPLEY)  %>%
    print()
  mean(bases$ESCOLARIDAD)  %>%
    print()
  mean(bases$CRI_Total)  %>%
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
  median(bases$IDARE_R_NIVEL, na.rm = T) %>%
    print()
  median(bases$IDERE_R_NIVEL, na.rm = T) %>%
    print()
  median(bases$IDARE_E_NIVEL, na.rm = T) %>%
    print()
  median(bases$IDERE_E_NIVEL, na.rm = T) %>%
    print()
  #print(bases) 
}


ggplot(data3, aes(x = DECADA, y = MoCA, color=DECADA)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "purple")

ggplot(data3, aes(x = DECADA, y = CRI_Total, color=DECADA)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "purple")

ggplot(data3, aes(x = DECADA, y = SHIPLEY, color=DECADA)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "purple")

ggplot(data3, aes(x = DECADA, y = ESCOLARIDAD, color=DECADA)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "purple")

ggplot(data3, aes(x = DECADA, y = EDAD, color=DECADA)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "purple")

ggplot(data3, aes(x = DECADA, y = IDARE_R_PUNTAJE, color=DECADA)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "purple") 

ggplot(data3, aes(x = DECADA, y = IDARE_E_PUNTAJE, color=DECADA)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "purple")

ggplot(data3, aes(x = DECADA, y = IDERE_R_PUNTAJE, color=DECADA)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "purple")

ggplot(data3, aes(x = DECADA, y = IDERE_E_PUNTAJE, color=DECADA)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "purple")

ggplot(data3, aes(x = DECADA, y = IDARE_R_NIVEL, color=DECADA)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "purple") 

ggplot(data3, aes(x = DECADA, y = IDARE_E_NIVEL, color=DECADA)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "purple")

ggplot(data3, aes(x = DECADA, y = IDERE_R_NIVEL, color=DECADA)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "purple")

ggplot(data3, aes(x = DECADA, y = IDERE_E_NIVEL, color=DECADA)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "purple")
