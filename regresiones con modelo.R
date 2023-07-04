filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %>%
  lm(formula= value ~ EDAD+ESCOLARIDAD+MoCA+CRI_Total, data =.)%>%
  summary()

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %>%
  lm(formula= value ~ EDAD+ESCOLARIDAD+MoCA+CRI_Total, data =.)%>%
  summary()

filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  lm(formula= value ~ ESCOLARIDAD+MoCA, data =.)%>%
  summary()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  ggplot(aes(x = ESCOLARIDAD , y = value)) +
  geom_point() + 
  labs(x = "ESCOLARIDAD", y = "SUP") +  
  geom_smooth(method = "lm", se = FALSE, color= "violet") +
  theme_classic()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  lm(formula= value ~ EDAD+ESCOLARIDAD+MoCA+CRI_Total, data =.)%>%
  summary()

