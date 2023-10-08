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



##### GRAFICAS ARTICULO ####
ggplot(data = data, aes(x = EDAD, y = AMP_ROS)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, color = "#9F79EE", lwd = 1.5) +
  labs(x = "AGE", y = "ENHANCEMENT INDEX") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=12))

ggplot(data = data, aes(x = EDAD, y = SUP_ROS)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, color = "#9F79EE", lwd = 1.5) +
  labs(x = "AGE", y = "SUPPRESSION INDEX") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=12))

filter(data, TIPO=="ROSTROS")%>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "#9F79EE", lwd = 1.5) +
  labs(x = "AGE", y = "WORKING MEMORY EFFICIENCY (ATTEND FACES)") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=12))

filter(data, TIPO=="ESCENAS")%>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "#9F79EE", lwd = 1.5) +
  labs(x = "AGE", y = "WORKING MEMORY EFFICIENCY (IGNORE FACES)") +  
  theme_classic()+ 
  theme(axis.text = element_text(size=12))
