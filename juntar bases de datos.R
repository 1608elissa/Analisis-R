junto <- data1 %>% 
  full_join(data3, by="ID") 

junto2 <- data3 %>% 
  full_join(data, by="ID")

junto3 <- data2 %>% 
  full_join(data3, by="ID")



junto$DECADA <- as.factor(junto$DECADA)

junto2$DECADA.x <- as.factor(junto2$DECADA.x)

junto3$DECADA.x <- as.factor(junto3$DECADA.x)



data3 <- read_xlsx("data.xlsx",sheet = "Hoja3")

data3$DECADA <- as.factor(data3$DECADA)
data3$ESCOLARIDAD <-as.numeric(data3$ESCOLARIDAD)
data3$IDEREESTADO <-as.numeric(data3$IDEREESTADO)
data3$IDERERASGO <-as.numeric(data3$IDERERASGO)


anova_test(SHIPLEY~DECADA, data=data3)
anova_test(ESCOLARIDAD~DECADA, data=data3)
anova_test(MOCA~DECADA, data=data3)

std.error(data3$SHIPLEY)
std.error(data3$ESCOLARIDAD)
std.error(data3$MOCA)

kruskal_test(IDAREESTADO~DECADA, data=data3)
kruskal_test(IDARERASGO~DECADA, data=data3)
kruskal_test(IDEREESTADO~DECADA, data=data3)
kruskal_test(IDERERASGO~DECADA, data=data3)

range(data3$IDAREESTADO, na.rm = T)
range(data3$IDARERASGO, na.rm = T)
range(data3$IDEREESTADO, na.rm = T)
range(data3$IDERERASGO, na.rm = T)



filter(junto, TR_RC == "RC", VAL=="NEU") %>%
  aov(value~DECADA.x + ESCOLARIDAD, data = .) %>%
  summary()

filter(junto, TR_RC == "RC", VAL=="NEU") %>%
  aov(value~DECADA.x + SHIPLEY, data = .) %>%
  summary()

filter(junto, TR_RC == "RC", VAL=="NEU") %>%
  aov(value~DECADA.x + MOCA, data = .) %>%
  summary()

filter(junto, TR_RC == "RC", VAL=="NEU") %>%
  aov(value~DECADA.x + IDARERASGO, data = .) %>%
  summary()



filter(junto, TR_RC == "TR", VAL=="NEU") %>%
  aov(value~DECADA.x + ESCOLARIDAD, data = .) %>%
  summary()

filter(junto, TR_RC == "TR", VAL=="NEU") %>%
  aov(value~DECADA.x + SHIPLEY, data = .) %>%
  summary()

filter(junto, TR_RC == "TR", VAL=="NEU") %>%
  aov(value~DECADA.x + MOCA, data = .) %>%
  summary()

filter(junto, TR_RC == "TR", VAL=="NEU") %>%
  aov(value~DECADA.x + IDARERASGO, data = .) %>%
  summary()



filter(junto2, EI == "EI", VAL=="NEU") %>%
  aov(value~DECADA.y + ESCOLARIDAD, data = .) %>%
  summary()

filter(junto2, EI == "EI", VAL=="NEU") %>%
  aov(value~DECADA.x + SHIPLEY, data = .) %>%
  summary()

filter(junto2, EI == "EI", VAL=="NEU") %>%
  aov(value~DECADA.x + MOCA, data = .) %>%
  summary()

filter(junto2, EI == "EI", VAL=="NEU") %>%
  aov(value~DECADA.x + IDARERASGO, data = .) %>%
  summary()



####MECANISMOS####

filter(junto3, TR_RC == "INRC", IND == "SUP", VAL=="NEU") %>%
  aov(value~DECADA.y + ESCOLARIDAD, data = .) %>%
  summary()

filter(junto3, TR_RC == "INRC", IND == "SUP", VAL=="NEU") %>%
  aov(value~DECADA.x + SHIPLEY, data = .) %>%
  summary()

filter(junto3, TR_RC == "INRC", IND == "SUP", VAL=="NEU") %>%
  aov(value~DECADA.x + MOCA, data = .) %>%
  summary()

filter(junto3, TR_RC == "INRC", IND == "SUP", VAL=="NEU") %>%
  aov(value~DECADA.x + IDARERASGO + ESCOLARIDAD, data = .) %>%
  summary()

filter(junto3, TR_RC == "INRC", IND == "AMP", VAL=="NEU") %>%
  aov(value~DECADA.y + ESCOLARIDAD, data = .) %>%
  summary()

filter(junto3, TR_RC == "INRC", IND == "AMP", VAL=="NEU") %>%
  aov(value~DECADA.x + SHIPLEY, data = .) %>%
  summary()

filter(junto3, TR_RC == "INRC", IND == "AMP", VAL=="NEU") %>%
  aov(value~DECADA.x + MOCA, data = .) %>%
  summary()

filter(junto3, TR_RC == "INRC", IND == "AMP", VAL=="NEU") %>%
  aov(value~DECADA.x + IDARERASGO + ESCOLARIDAD, data = .) %>%
  summary()



filter(junto3, TR_RC == "INTR", IND == "SUP", VAL=="NEU") %>%
  aov(value~DECADA.y + ESCOLARIDAD, data = .) %>%
  summary()

filter(junto3, TR_RC == "INTR", IND == "SUP", VAL=="NEU") %>%
  aov(value~DECADA.x + SHIPLEY, data = .) %>%
  summary()

filter(junto3, TR_RC == "INTR", IND == "SUP", VAL=="NEU") %>%
  aov(value~DECADA.x + MOCA, data = .) %>%
  summary()

filter(junto3, TR_RC == "INTR", IND == "SUP", VAL=="NEU") %>%
  aov(value~DECADA.x + IDARERASGO + ESCOLARIDAD, data = .) %>%
  summary()

filter(junto3, TR_RC == "INTR", IND == "AMP", VAL=="NEU") %>%
  aov(value~DECADA.y + ESCOLARIDAD, data = .) %>%
  summary()

filter(junto3, TR_RC == "INTR", IND == "AMP", VAL=="NEU") %>%
  aov(value~DECADA.x + SHIPLEY, data = .) %>%
  summary()

filter(junto3, TR_RC == "INTR", IND == "AMP", VAL=="NEU") %>%
  aov(value~DECADA.x + MOCA, data = .) %>%
  summary()

filter(junto3, TR_RC == "INTR", IND == "AMP", VAL=="NEU") %>%
  aov(value~DECADA.x + IDARERASGO + ESCOLARIDAD, data = .) %>%
  summary()



filter(junto3, TR_RC == "INEI", IND == "SUP", VAL=="NEU") %>%
  aov(value~DECADA.y + ESCOLARIDAD, data = .) %>%
  summary()

filter(junto3, TR_RC == "INEI", IND == "SUP", VAL=="NEU") %>%
  aov(value~DECADA.x + SHIPLEY, data = .) %>%
  summary()

filter(junto3, TR_RC == "INEI", IND == "SUP", VAL=="NEU") %>%
  aov(value~DECADA.x + MOCA, data = .) %>%
  summary()

filter(junto3, TR_RC == "INEI", IND == "SUP", VAL=="NEU") %>%
  aov(value~DECADA.x + IDARERASGO + ESCOLARIDAD, data = .) %>%
  summary()

filter(junto3, TR_RC == "INEI", IND == "AMP", VAL=="NEU") %>%
  aov(value~DECADA.y + ESCOLARIDAD, data = .) %>%
  summary()

filter(junto3, TR_RC == "INEI", IND == "AMP", VAL=="NEU") %>%
  aov(value~DECADA.x + SHIPLEY, data = .) %>%
  summary()

filter(junto3, TR_RC == "INEI", IND == "AMP", VAL=="NEU") %>%
  aov(value~DECADA.x + MOCA, data = .) %>%
  summary()

filter(junto3, TR_RC == "INEI", IND == "AMP", VAL=="NEU") %>%
  aov(value~DECADA.x + IDARERASGO + ESCOLARIDAD, data = .) %>%
  summary()
