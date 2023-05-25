
filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() + 
  labs(x = "CRI_Total", y = "EI") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()

filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() + 
  labs(x = "MoCA", y = "EI") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()



filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "EI") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()

filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = MoCA, y = value)) +
  geom_point() + 
  labs(x = "MoCA", y = "EI") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()

filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG") %>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() + 
  labs(x = "MoCA", y = "EI") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()




filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() + 
  labs(x = "ESCOLARIDAD", y = "EI") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()




filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %>%
  ggplot(aes(x = CRI_Total, y = value)) +
  geom_point() + 
  labs(x = "CRI_Total", y = "DPR") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()

filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %>%
  ggplot(aes(x = EDAD, y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "DPR") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()

filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() + 
  labs(x = "ESCOLARIDAD", y = "DPR") +  
  geom_smooth(method = "lm", se = FALSE, color= "firebrick") +
  theme_classic()
