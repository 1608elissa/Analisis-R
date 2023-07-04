library(plotrix)
library(readxl)
library(rstatix)
library(tidyverse)

###### BASES DE DATOS #####
data <- read_xlsx("QiGong/QiGong.xlsx",sheet = "Nback")%>%
  gather(cond_ses_tipo_VD, value, -c("Sj","SEXO","EDAD")) %>%
  separate(cond_ses_tipo_VD, c("COND","SESION","TIPO","VD"), sep = "_")

data2 <- read_xlsx("QiGong/QiGong cambios.xlsx",sheet = "Eriksen")%>%
  gather(ses_VD_dif_cons, value, -c("Sj")) %>%
  separate(ses_VD_dif_cons, c("SESION","VD","DIF","CONS"), sep = "_")

data3 <- read_xlsx("QiGong/QiGong.xlsx",sheet = "DEMO")%>%
  gather(var_sesion, value, -c("Sj","SEXO","EDAD")) %>%
  separate(var_sesion, c("VAR","SESION"), sep = "_")


###### N BACK #####
filter(data, VD == "RC", !COND=="0Back") %>%
  aov(value ~ COND*SESION*TIPO, data=.)%>%
  summary()
filter(data, VD == "RC", !COND=="0Back") %>%
  aov(value ~ COND*SESION*TIPO, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data, VD == "TR", !COND=="0Back") %>%
  aov(value ~ COND*SESION*TIPO, data=.)%>%
  summary()
filter(data, VD == "TR", !COND=="0Back") %>%
  aov(value ~ COND*SESION*TIPO, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

filter(data, VD == "DP", !COND=="0Back") %>%
  aov(value ~ COND*SESION, data=.)%>%
  summary()
filter(data, VD == "DP", !COND=="0Back") %>%
  aov(value ~ COND*SESION, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()

##### GRAFICAS NBACK ####

filter(data, VD == "RC", !COND=="0Back") %>%
  ggplot(aes(y = value, x = COND, color=COND)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "#473C8B")

filter(data, VD == "RC", !COND=="0Back") %>%
  ggplot(aes(y = value, x = TIPO, color=TIPO)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "#473C8B")

filter(data, VD == "RC", !COND=="0Back") %>%
  ggplot(aes(y = value, x = COND, color=TIPO)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "#473C8B")

filter(data, VD == "RC", !COND=="0Back") %>%
  summarySE(measurevar = "value", groupvars = c("COND","TIPO"))%>%
  ggplot(aes(y=value, x=COND, fill= TIPO)) +
  geom_col(position = "dodge") + 
  theme_classic() +
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge")


filter(data, VD == "TR", !COND=="0Back") %>%
  ggplot(aes(y = value, x = SESION, color=SESION)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "#473C8B")

filter(data, VD == "DP", !COND=="0Back") %>%
  ggplot(aes(y = value, x = COND, color=COND)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "#473C8B")


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

filter(data2, VD == "TR", DIF == "Re", !CONS=="FIDI") %>%
  aov(value ~ SESION*CONS, data=.)%>%
  summary()
filter(data2, VD == "TR", DIF == "Re", !CONS=="FIDI") %>%
  aov(value ~ SESION*CONS, data=.)%>%
  tukey_hsd()%>%
  filter(p.adj < 0.05)%>%
  View()


##### GRAFICAS ERIKSEN ####

filter(data2, VD == "RC", !DIF == "Re") %>%
  ggplot(aes(y = value, x = SESION, color=SESION)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "#473C8B")

filter(data2, VD == "TR", !DIF == "Re") %>%
  ggplot(aes(y = value, x = DIF, color=DIF)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "#473C8B")

filter(data2, VD == "TR", DIF == "Re", !CONS=="FIDI") %>%
  ggplot(aes(y = value, x = CONS, color=CONS)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "#473C8B")

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


filter(data3, VAR == "IDAREE") %>%
  ggplot(aes(y = value, x = SESION, color=SESION)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "#473C8B")

filter(data3, VAR == "IDARER") %>%
  ggplot(aes(y = value, x = SESION, color=SESION)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "#473C8B")

filter(data3, VAR == "IDEREE") %>%
  ggplot(aes(y = value, x = SESION, color=SESION)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "#473C8B")

filter(data3, VAR == "IDERER") %>%
  ggplot(aes(y = value, x = SESION, color=SESION)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "#473C8B")

filter(data3, VAR == "ACTNO") %>%
  ggplot(aes(y = value, x = SESION, color=SESION)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "#473C8B")

filter(data3, VAR == "ACTFRE") %>%
  ggplot(aes(y = value, x = SESION, color=SESION)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "#473C8B")

filter(data3, VAR == "CFQ") %>%
  ggplot(aes(y = value, x = SESION, color=SESION)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none") +
  theme_classic() +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.35,
               color = "#473C8B")

