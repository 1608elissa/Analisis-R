filter(data2, MECANISM== "AMP", MEDICION=="1DU", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %$%
  cor.test( value, EDAD,
            method="spearman")

filter(data2, MECANISM== "AMP", MEDICION=="1DU", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() + 
  labs(x = "ESCOLARIDAD", y = "AMPLIFICACION") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()

filter(data2, MECANISM== "SUP", MEDICION=="1DU", CALIF=="COR", FASE== "COD", ESTIMULO== "ESC", TIPO=="TG", !VAL=="TOT") %>%
  ggplot(aes(x = ESCOLARIDAD, y = value)) +
  geom_point() + 
  labs(x = "ESCOLARIDAD", y = "SUPRESION") +  
  geom_smooth(method = "lm", se = FALSE, color= "black") +
  theme_classic()
