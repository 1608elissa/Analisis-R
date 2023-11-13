ggplot(data = data, aes(x = ESCOLARIDAD, y = AMP_ROS)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, color = "#5F9EA0", lwd = 1.5) +
  labs(x = "ESCOLARIDAD", y = "ÍNDICE DE AMPLIFICACIÓN") +  
  theme_classic()

ggplot(data = data, aes(x = IDARE_E_PUNTAJE, y = AMP_ROS)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, color = "#9F79EE", lwd = 1.5) +
  labs(x = "IDARE ESTADO", y = "ÍNDICE DE AMPLIFICACIÓN") +  
  theme_classic()

ggplot(data = data, aes(x = IDERE_R_PUNTAJE, y = AMP_ROS)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, color = "#6E8B3D", lwd = 1.5) +
  labs(x = "IDERE RASGO", y = "ÍNDICE DE AMPLIFICACIÓN") +  
  theme_classic()

ggplot(data = data, aes(x = SUEÑO_2DA, y = AMP_ROS)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, color = "#1874CD", lwd = 1.5) +
  labs(x = "HORAS DE SUEÑO EL DÍA DE LA SESIÓN", y = "ÍNDICE DE AMPLIFICACIÓN") +  
  theme_classic()

ggplot(data = data, aes(x = OCUPA_CAT, y = AMP_ROS)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, color = "#CD5555", lwd = 1.5) +
  labs(x = "OCUPACIÓN", y = "ÍNDICE DE AMPLIFICACIÓN") +  
  theme_classic()

ggplot(data = data, aes(x = EDAD, y = AMP_ROS)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, color = "#8B636C", lwd = 1.5) +
  labs(x = "EDAD", y = "ÍNDICE DE SUPRESIÓN") +  
  theme_classic()



filter(data, TIPO=="ROSTROS")%>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "#8B636C", lwd = 1.5) +
  labs(x = "EDAD", y = "ÍNDICE DE EFICIENCIA (ROSTROS)") +  
  theme_classic()

filter(data, TIPO=="ROSTROS")%>%
  ggplot(aes(x = IDERE_R_PUNTAJE, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "#6E8B3D", lwd = 1.5) +
  labs(x = "IDERE RASGO", y = "ÍNDICE DE EFICIENCIA (ROSTROS)") +  
  theme_classic()

filter(data, TIPO=="ROSTROS")%>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "#8B668B", lwd = 1.5) +
  labs(x = "MoCA", y = "ÍNDICE DE EFICIENCIA (ROSTROS)") +  
  theme_classic()

filter(data, TIPO=="ROSTROS")%>%
  ggplot(aes(x = OCUPA_CAT, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "#CD5555", lwd = 1.5) +
  labs(x = "OCUPACIÓN", y = "ÍNDICE DE EFICIENCIA (ROSTROS)") +  
  theme_classic()


filter(data, TIPO=="ESCENAS")%>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "#8B636C", lwd = 1.5) +
  labs(x = "EDAD", y = "ÍNDICE DE EFICIENCIA (ESCENAS)") +  
  theme_classic()

filter(data, TIPO=="ESCENAS")%>%
  ggplot(aes(x = IDARE_E_PUNTAJE, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "#9F79EE", lwd = 1.5) +
  labs(x = "IDARE ESTADO", y = "ÍNDICE DE EFICIENCIA (ESCENAS)") +  
  theme_classic()

filter(data, TIPO=="ESCENAS")%>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "#8B668B", lwd = 1.5) +
  labs(x = "MoCA", y = "ÍNDICE DE EFICIENCIA (ESCENAS)") +  
  theme_classic()

filter(data, TIPO=="ESCENAS")%>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "#8B5A00", lwd = 1.5) +
  labs(x = "RESERVA COGNITIVA", y = "ÍNDICE DE EFICIENCIA (ESCENAS)") +  
  theme_classic()



##### GRAFICAS ARTICULO BASICAS ####
ggplot(data = data, aes(x = EDAD, y = AMP_ROS)) +
  geom_point() + geom_smooth(method = "lm", color = "#9F79EE", lwd = 1.5) +
  labs(x = "AGE", y = "ENHANCEMENT INDEX") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=12)) + 
  scale_y_continuous(breaks = seq(-2.5, 2.5))

ggplot(data = data, aes(x = EDAD, y = SUP_ROS)) +
  geom_point() + geom_smooth(method = "lm", color = "#9F79EE", lwd = 1.5) +
  labs(x = "AGE", y = "SUPPRESSION INDEX") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=12)) + 
  scale_y_continuous(breaks = seq(-2.5, 2.5))

filter(data, TIPO=="ROSTROS")%>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#9F79EE", lwd = 1.5) +
  labs(x = "AGE", y = "WORKING MEMORY EFFICIENCY-ATTEND FACES (IEI)") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=13)) + 
  scale_y_continuous(breaks = seq(-2, 5))

filter(data, TIPO=="ESCENAS")%>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#9F79EE", lwd = 1.5) +
  labs(x = "AGE", y = "WORKING MEMORY EFFICIENCY-IGNORE FACES (IEI)") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=13)) + 
  scale_y_continuous(breaks = seq(-2, 5))


filter(data, TIPO=="ROSTROS")%>%
  ggplot(aes(x = value, y = AMP_ROS)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#CD5555", lwd = 1.5) +
  labs(x = "WORKING MEMORY EFFICIENCY-IGNORE FACES (IEI)", y = "ENHANCEMENT INDEX") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=13))+ 
  scale_y_continuous(breaks = seq(-2.5, 2.5)) +
  scale_x_continuous(breaks = seq(-2, 5))

filter(data, TIPO=="ESCENAS")%>%
  ggplot(aes(x = value, y = SUP_ROS)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#CD5555", lwd = 1.5) +
  labs(x = "WORKING MEMORY EFFICIENCY-IGNORE FACES (IEI)", y = "SUPPRESSION INDEX") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=13)) +
  scale_y_continuous(breaks = seq(-2.5, 2.5)) +
  scale_x_continuous(breaks = seq(-2, 5))


#### GRAFICAS ARTICULO REGRESIONES MULTIPLES ####

data2 <- read_xlsx("Junto.xlsx",sheet = "Regresiones")%>%
  gather(var_cond_tipo, value, -c("ID","DECADA","SEXO","EDAD","EDAD_Z","EDAD_CAT","MoCA","MoCA_Z",
                                  "MoCA_CAT","ESCOLARIDAD","ESCOLARIDAD_Z","ESCOLARIDAD_CAT","CRI_Total","CRI_Total_Z","CRI_Total_CAT",
                                  "IDARE_R_PUNTAJE","IDARE_R_Z","IDARE_R_CAT","IDARE_E_PUNTAJE","IDARE_E_Z","IDARE_E_CAT","IDERE_R_PUNTAJE",
                                  "IDERE_R_Z","IDERE_R_CAT","IDERE_E_PUNTAJE","IDERE_E_Z","IDERE_E_CAT","COVID_CAT","SHIPLEY","SHIPLEY_Z",
                                  "SHIPLEY_CAT","SUEÑO_NOR","SUEÑO_NOR_Z","SUEÑO_NOR_CAT","SUEÑO_2DA","SUEÑO_2DA_Z","SUEÑO_2DA_CAT",	
                                  "OCUPACION","OCUPA_CAT","OCUPA_Z","OCUPACION_CAT","AMP_ROS","AMP_ROS_Z","SUP_ROS","SUP_ROS_Z")) %>%
  separate(var_cond_tipo, c("VAR","COND","TIPO"), sep = "_")

### AMPLIFICACION - ROSTROS 

filter(data, TIPO=="ROSTROS")%>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#8B636C", lwd = 1.5) +
  labs(x = "AGE", y = "WORKING MEMORY EFFICIENCY-ATTEND FACES (IEI)") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=13)) 

filter(data, TIPO=="ROSTROS")%>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#8B668B", lwd = 1.5) +
  labs(x = "COGNITIVE FUNCTION", y = "WORKING MEMORY EFFICIENCY-ATTEND FACES (IEI)") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=13)) 

filter(data, TIPO=="ROSTROS")%>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#8B5A00", lwd = 1.5) +
  labs(x = "COGNITIVE RESERVE", y = "WORKING MEMORY EFFICIENCY-ATTEND FACES (IEI)") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=13))




filter(data, TIPO=="ESCENAS")%>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#8B636C", lwd = 1.5) +
  labs(x = "AGE", y = "WORKING MEMORY EFFICIENCY-IGNORE FACES (IEI)") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=13))

filter(data, TIPO=="ESCENAS")%>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#8B5A00", lwd = 1.5) +
  labs(x = "COGNITIVE RESERVE", y = "WORKING MEMORY EFFICIENCY-IGNORE FACES (IEI)") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=13))

filter(data, TIPO=="ESCENAS")%>%
  ggplot(aes(x = IDARE_E_PUNTAJE, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#008B8B", lwd = 1.5) +
  labs(x = "STATE ANXIETY", y = "WORKING MEMORY EFFICIENCY-IGNORE FACES (IEI)") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=13))

filter(data, TIPO=="ESCENAS")%>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#8B668B", lwd = 1.5) +
  labs(x = "COGNITIVE FUNCTION", y = "WORKING MEMORY EFFICIENCY-IGNORE FACES (IEI)") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=13)) 




ggplot(data = data, aes(x = ESCOLARIDAD, y = AMP_ROS)) +
  geom_point() + geom_smooth(method = "lm", color = "#5F9EA0", lwd = 1.5) +
  labs(x = "SCHOOLING", y = "ENHANCEMENT INDEX") +  
  theme_classic()

ggplot(data = data, aes(x = CRI_Total, y = AMP_ROS)) +
  geom_point() + geom_smooth(method = "lm", color = "#8B5A00", lwd = 1.5) +
  labs(x = "COGNITIVE RESERVE", y = "ENHANCEMENT INDEX") +  
  theme_classic()





filter(data2, !CRI_Total_CAT=="MEDIO")%>%
  ggplot(aes(x = EDAD_Z, y = AMP_ROS_Z, colour= CRI_Total_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "ENHANCEMENT INDEX", color = "CRI_CAT") +
  theme_classic()+ 
  theme(axis.text = element_text(size=13))

filter(data2, !CRI_Total_CAT=="MEDIO")%>%
  ggplot(aes(x = EDAD_Z, y = SUP_ROS_Z, colour= CRI_Total_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "SUPPRESSION INDEX", color = "CRI_CAT") +
  theme_classic()+ 
  theme(axis.text = element_text(size=13))



filter(data2, COND== "ESCENAS", TIPO== "Z", !IDERE_E_CAT=="MEDIO")%>%
  ggplot(aes(x = EDAD_Z, y = value, colour= IDERE_E_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "WORKING MEMORY EFFICIENCY-IGNORE FACES (IEI)", color = "IDERE_E_CAT") +
  theme_classic()+ 
  theme(axis.text = element_text(size=13))

filter(data2, COND== "ESCENAS", TIPO== "Z", !SHIPLEY_CAT=="MEDIO")%>%
  ggplot(aes(x = EDAD_Z, y = value, colour= SHIPLEY_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "WORKING MEMORY EFFICIENCY-IGNORE FACES (IEI)", color = "SHIPLEY_CAT") +
  theme_classic()+ 
  theme(axis.text = element_text(size=13))



filter(data2, COND== "ESCENAS", TIPO== "Z", !CRI_Total_CAT=="MEDIO")%>%
  ggplot(aes(x = SUP_ROS_Z, y = value, colour= CRI_Total_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "SUPPRESSION INDEX", y = "WORKING MEMORY EFFICIENCY-IGNORE FACES (IEI)", color = "CRI_Total_CAT") +
  theme_classic()+ 
  theme(axis.text = element_text(size=13))


filter(data2, !SUEÑO_2DA_CAT=="MEDIO")%>%
  ggplot(aes(x = EDAD_Z, y = SUP_ROS_Z, colour= SUEÑO_2DA_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "SUPPRESSION INDEX", color = "SLEEP-SES_CAT") +
  theme_classic()+ 
  theme(axis.text = element_text(size=13))


#### GRAFICAS ARTICULO VIOLIN DIFERENCIAS ENTRE GRUPOS ####

filter(data2, COND== "ROSTROS", TIPO== "Z", !EDAD_CAT=="MEDIO") %>%
  ggplot(aes(y=value, x=EDAD_CAT, fill=EDAD_CAT)) + 
  scale_fill_brewer(palette="BuPu") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "AGE GROUP", y = "WORKING MEMORY EFFICIENCY-ATTEND FACES (IEI)") +
  theme_classic()+ 
  theme(legend.position = "none") +
  theme(axis.text = element_text(size=13))

filter(data2, COND== "ROSTROS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %>%
  ggplot(aes(y=value, x=CRI_Total_CAT, fill=CRI_Total_CAT)) + 
  scale_fill_brewer(palette="RdPu") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "COGNITIVE RESERVE GROUP", y = "WORKING MEMORY EFFICIENCY-ATTEND FACES (IEI)") +
  theme_classic()+ 
  theme(legend.position = "none") +
  theme(axis.text = element_text(size=13))

filter(data2, COND== "ROSTROS", TIPO== "Z", !MoCA_CAT=="MEDIO") %>%
  ggplot(aes(y=value, x=MoCA_CAT, fill=MoCA_CAT)) + 
  scale_fill_brewer(palette="PuRd") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "CURRENT COGNITIVE FUNCTION GROUP", y = "WORKING MEMORY EFFICIENCY-ATTEND FACES (IEI)") +
  theme_classic()+ 
  theme(legend.position = "none") +
  theme(axis.text = element_text(size=13))

filter(data2, COND== "ESCENAS", TIPO== "Z", !EDAD_CAT=="MEDIO") %>%
  ggplot(aes(y=value, x=EDAD_CAT, fill=EDAD_CAT)) + 
  scale_fill_brewer(palette="BuPu") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "AGE GROUP", y = "WORKING MEMORY EFFICIENCY-IGNORE FACES (IEI)") +
  theme_classic()+ 
  theme(legend.position = "none") +
  theme(axis.text = element_text(size=13))

filter(data2, COND== "ESCENAS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %>%
  ggplot(aes(y=value, x=CRI_Total_CAT, fill=CRI_Total_CAT)) + 
  scale_fill_brewer(palette="RdPu") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "COGNITIVE RESERVE GROUP", y = "WORKING MEMORY EFFICIENCY-IGNORE FACES (IEI)") +
  theme_classic()+ 
  theme(legend.position = "none") +
  theme(axis.text = element_text(size=13))

filter(data2, COND== "ESCENAS", TIPO== "Z", !MoCA_CAT=="MEDIO") %>%
  ggplot(aes(y=value, x=MoCA_CAT, fill=MoCA_CAT)) + 
  scale_fill_brewer(palette="PuRd") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "CURRENT COGNITIVE FUNCTION GROUP", y = "WORKING MEMORY EFFICIENCY-IGNORE FACES (IEI)") +
  theme_classic()+ 
  theme(legend.position = "none") +
  theme(axis.text = element_text(size=13))

filter(data2, COND== "ESCENAS", TIPO== "Z", !IDARE_E_CAT=="MEDIO") %>%
  ggplot(aes(y=value, x=IDARE_E_CAT, fill=IDARE_E_CAT)) + 
  scale_fill_brewer(palette="Greens") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "STATE ANXIETY GROUP", y = "WORKING MEMORY EFFICIENCY-IGNORE FACES (IEI)") +
  theme_classic()+ 
  theme(legend.position = "none") +
  theme(axis.text = element_text(size=13))


filter(data2, !ESCOLARIDAD_CAT=="MEDIO") %>%
  ggplot(aes(y=AMP_ROS_Z, x=ESCOLARIDAD_CAT, fill=ESCOLARIDAD_CAT)) +
  scale_fill_brewer(palette="Reds") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "AGE SCHOOLING GROUP", y = "ENHANCEMENT INDEX") +
  theme_classic()+ 
  theme(legend.position = "none") +
  theme(axis.text = element_text(size=13))

filter(data2, !CRI_Total_CAT=="MEDIO") %>%
  ggplot(aes(y=AMP_ROS_Z, x=CRI_Total_CAT, fill=CRI_Total_CAT)) +
  scale_fill_brewer(palette="RdPu") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "COGNITIVE RESERVE GROUP", y = "ENHANCEMENT INDEX") +
  theme_classic()+ 
  theme(legend.position = "none") +
  theme(axis.text = element_text(size=13))





