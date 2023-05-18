library(plotrix)
library(readxl)
library(rstatix)
library(tidyverse)

###### BASES DE DATOS #####
data <- read_xlsx("QiGong/QiGong.xlsx",sheet = "Nback")%>%
  gather(cond_ses_tipo_VD, value, -c("Sj","SEXO","EDAD")) %>%
  separate(cond_ses_tipo_VD, c("COND","SESION","TIPO","VD"), sep = "_")

data2 <- read_xlsx("QiGong/QiGong.xlsx",sheet = "Eriksen")%>%
  gather(ses_VD_dif_cons, value, -c("Sj")) %>%
  separate(ses_VD_dif_cons, c("SESION","VD","DIF","CONS"), sep = "_")

data3 <- read_xlsx("QiGong/QiGong.xlsx",sheet = "DEMO")%>%
  gather(var_sesion, value, -c("Sj","SEXO","EDAD")) %>%
  separate(var_sesion, c("VAR","SESION"), sep = "_")


###### N BACK #####
filter(data, VD == "RC") %>%
  aov(value ~ COND*SESION*TIPO, data=.)%>%
  summary()
filter(data, VD == "RC") %>%
  aov(value ~ COND*SESION*TIPO, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data, VD == "TR") %>%
  aov(value ~ COND*SESION*TIPO, data=.)%>%
  summary()
filter(data, VD == "TR") %>%
  aov(value ~ COND*SESION*TIPO, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data, VD == "DP") %>%
  aov(value ~ COND*SESION, data=.)%>%
  summary()
filter(data, VD == "DP") %>%
  aov(value ~ COND*SESION, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()


###### ERIKSEN #####
filter(data2, VD == "RC", !DIF == "Re") %>%
  aov(value ~ SESION*DIF*CONS, data=.)%>%
  summary()
filter(data2, VD == "RC", !DIF == "Re") %>%
  aov(value ~ SESION*DIF*CONS, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data2, VD == "TR", !DIF == "Re") %>%
  aov(value ~ SESION*DIF*CONS, data=.)%>%
  summary()
filter(data2, VD == "TR", !DIF == "Re") %>%
  aov(value ~ SESION*DIF*CONS, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data2, VD == "RC", DIF == "Re") %>%
  aov(value ~ SESION*CONS, data=.)%>%
  summary()
filter(data2, VD == "RC", DIF == "Re") %>%
  aov(value ~ SESION*CONS, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data2, VD == "TR", DIF == "Re") %>%
  aov(value ~ SESION*CONS, data=.)%>%
  summary()
filter(data2, VD == "TR", DIF == "Re") %>%
  aov(value ~ SESION*CONS, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

###### DEMOGRAFICOS #####
filter(data3, VAR == "IDAREE") %>%
  wilcox.test(value ~ SESION, data=., correct=FALSE)

filter(data3, VAR == "IDARER") %>%
  wilcox.test(value ~ SESION, data=., correct=FALSE)

filter(data3, VAR == "IDEREE") %>%
  wilcox.test(value ~ SESION, data=., correct=FALSE)

filter(data3, VAR == "IDERER") %>%
  wilcox.test(value ~ SESION, data=., correct=FALSE)

filter(data3, VAR == "ACTNO") %>%
  wilcox.test(value ~ SESION, data=., correct=FALSE)

filter(data3, VAR == "ACTFRE") %>%
  wilcox.test(value ~ SESION, data=., correct=FALSE)

filter(data3, VAR == "CFQ") %>%
  wilcox.test(value ~ SESION, data=., correct=FALSE)

