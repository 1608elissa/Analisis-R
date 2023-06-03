library(readxl)
library(rstatix)
library(tidyverse)
library(magrittr)

data <- read_xlsx("Resultados escalas.xlsx")

cor.test(data$F1, data$Urg_Neg,
          method="spearman")

cor.test(data$F2, data$Urg_Neg,
         method="spearman")

cor.test(data$Total...4, data$Urg_Neg,
         method="spearman")


cor.test(data$F1, data$Urg_Pos,
         method="spearman")

cor.test(data$F2, data$Urg_Pos,
         method="spearman")

cor.test(data$Total...4, data$Urg_Pos,
         method="spearman")


cor.test(data$F1, data$Perseverancia,
         method="spearman")

cor.test(data$F2, data$Perseverancia,
         method="spearman")

cor.test(data$Total...4, data$Perseverancia,
         method="spearman")


cor.test(data$F1, data$Premeditacion,
         method="spearman")

cor.test(data$F2, data$Premeditacion,
         method="spearman")

cor.test(data$Total...4, data$Premeditacion,
         method="spearman")


cor.test(data$F1, data$Busq_sen,
         method="spearman")

cor.test(data$F2, data$Busq_sen,
         method="spearman")

cor.test(data$Total...4, data$Busq_sen,
         method="spearman")



cor.test(data$Total...4, data$Total...10,
         method="spearman")




ggplot(data, aes(x= Urg_Neg , y= F2)) +
  geom_point() + 
  labs(x = "Urgencia Negativa", y = "Postergación de actividades") + 
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

ggplot(data, aes(x= Urg_Pos , y= F2)) +
  geom_point() + 
  labs(x = "Urgencia Positiva", y = "Postergación de actividades") + 
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

ggplot(data, aes(x= Premeditacion , y= F1)) +
  geom_point() + 
  labs(x = "Falta de Premeditación", y = "Autorregulación académica") + 
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

ggplot(data, aes(x= Perseverancia , y= F1)) +
  geom_point() + 
  labs(x = "Falta de Perseverancia", y = "Autorregulación académica") + 
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()

ggplot(data, aes(x= Total...10 , y= Total...4)) +
  geom_point() + 
  labs(x = "Impulsividad", y = "Procrastinación") + 
  geom_smooth(method = "lm", color= "firebrick") +
  theme_classic()
