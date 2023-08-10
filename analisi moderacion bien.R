#### EDAD ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS * EDAD, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = EDAD, y = value, colour = EDAD)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "EDAD", y = "EI rostros") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS", EDAD_CAT=="BAJO")%>%
  lm(formula= value ~ AMP_ROS, data =.)%>%
  summary()
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS", EDAD_CAT=="ALTO")%>%
  lm(formula= value ~ AMP_ROS, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS * EDAD, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  ggplot(aes(x = SUP_ROS, y = value, colour = EDAD_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "SUPRESION", y = "EI escenas") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS", EDAD_CAT=="BAJO")%>%
  lm(formula= value ~ AMP_ROS, data =.)%>%
  summary()
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS", EDAD_CAT=="ALTO")%>%
  lm(formula= value ~ AMP_ROS, data =.)%>%
  summary()

#### ESCOLARIDAD ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS * ESCOLARIDAD, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS * ESCOLARIDAD, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = AMP_ROS, y = value, colour = ESCOLARIDAD)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AMPLIFICACION", y = "EI") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  ggplot(aes(x = SUP_ROS, y = value, colour = ESCOLARIDAD)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "SUPRESION", y = "EI") +  
  theme_classic()

#### CRIQ ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS * CRI_Total, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS * CRI_Total, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS", CRI_Total_CAT=="BAJO")%>%
  lm(formula= value ~ SUP_ROS, data =.)%>%
  summary()
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS", CRI_Total_CAT=="ALTO")%>%
  lm(formula= value ~ SUP_ROS, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  ggplot(aes(x = AMP_ROS, y = value, colour = CRI_Total)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AMPLIFICACION", y = "EI") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  ggplot(aes(x = SUP_ROS, y = value, colour = CRI_Total)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color= "PURPLE") +
  labs(x = "SUPRESION", y = "EI") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  ggplot(aes(x = SUP_ROS, y = value, colour = CRI_Total_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "SUPRESION", y = "EI") +  
  theme_classic()

#### IDARE ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS * IDARE_R_PUNTAJE, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS * IDARE_R_PUNTAJE, data =.)%>%
  summary()

#### IDERE ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS * IDERE_R_PUNTAJE, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS * IDERE_R_PUNTAJE, data =.)%>%
  summary()

#### MOCA ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS * MoCA, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS", !MoCA_CAT=="MEDIO")%>%
  ggplot(aes(x = AMP_ROS, y = value, colour = MoCA_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AMPLIFICACION", y = "EI rostros") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS", MoCA_CAT=="BAJO")%>%
  lm(formula= value ~ AMP_ROS, data =.)%>%
  summary()
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS", MoCA_CAT=="ALTO")%>%
  lm(formula= value ~ AMP_ROS, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS * MoCA, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS", !MoCA_CAT=="MEDIO")%>%
  ggplot(aes(x = AMP_ROS, y = value, colour = MoCA_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AMPLIFICACION", y = "EI escenas") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS", MoCA_CAT=="BAJO")%>%
  lm(formula= value ~ AMP_ROS, data =.)%>%
  summary()
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS", MoCA_CAT=="ALTO")%>%
  lm(formula= value ~ AMP_ROS, data =.)%>%
  summary()

#### SUEÑO NORMAL ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS * SUEÑO_NOR, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS * SUEÑO_NOR, data =.)%>%
  summary()

#### SUEÑO SEGUNDA SESION ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS * SUEÑO_2DA, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS * SUEÑO_2DA, data =.)%>%
  summary()

#### OCUPACION ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS * OCUPA_CAT, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS", !OCUPA_CAT=="MEDIO")%>%
  ggplot(aes(x = AMP_ROS, y = value, colour = OCUPACION_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AMPLIFICACION", y = "EI rostros") +  
  theme_classic()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS", OCUPACION_CAT=="BAJO")%>%
  lm(formula= value ~ AMP_ROS, data =.)%>%
  summary()
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS", OCUPACION_CAT=="ALTO")%>%
  lm(formula= value ~ AMP_ROS, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS * OCUPA_CAT, data =.)%>%
  summary()

#### COVID ####
filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")%>%
  lm(formula= value ~ AMP_ROS * COVID_CAT, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")%>%
  lm(formula= value ~ SUP_ROS * COVID_CAT, data =.)%>%
  summary()
