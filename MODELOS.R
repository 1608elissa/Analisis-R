a <- filter(data, VD == "EI", COND== "ROS", VAL=="TOT", TIPO=="TG")

b <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = a)

step(object = b, direction = "both", trace = 1)

modeloEIROS <- (lm(formula = value ~ EDAD + MoCA + CRI_Total + EDAD:CRI_Total + 
                MoCA:CRI_Total, data = a))
summary(modeloEIROS)


c <- filter(data, VD == "EI", COND== "ESC", VAL=="TOT", TIPO=="TG")

d <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = c)

step(object = d, direction = "both", trace = 1)

modeloEIESC <- (lm(formula = value ~ EDAD * ESCOLARIDAD * MoCA * CRI_Total, data = c))
summary(modeloEIESC)



e <- filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS")

f <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = e)

step(object = f, direction = "both", trace = 1)

modeloAMPDUR <- (lm(formula = value ~ ESCOLARIDAD + MoCA + ESCOLARIDAD:MoCA, data = e))
summary(modeloAMPDUR)



g <- filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS")

h <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = g)

step(object = h, direction = "both", trace = 1)

modeloSUPDUR <- (lm(formula = value ~ EDAD + CRI_Total + EDAD:CRI_Total, data = g))
summary(modeloSUPDUR)

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %$%
  cor.test( value, EDAD,
            method="pearson")
filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %$%
  cor.test( value, CRI_Total,
            method="spearman")

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  ggplot(aes(x = EDAD , y = value)) +
  geom_point() + 
  labs(x = "EDAD", y = "SUP") +  
  geom_smooth(method = "lm", se = FALSE, color= "violet") +
  theme_classic()

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %>%
  ggplot(aes(x = CRI_Total , y = value)) +
  geom_point() + 
  labs(x = "CRI_Total", y = "SUP")


i <- filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ROSTROS")

j <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = i)

step(object = j, direction = "both", trace = 1)

modeloDPROS <- (lm(formula = value ~ EDAD + MoCA + CRI_Total + EDAD:MoCA + MoCA:CRI_Total, 
                   data = i))
summary(modeloDPROS)



k <- filter(data, VD == "DPR", COND== "DP", VAL=="TOT", TIPO=="ESCENAS")

l <- lm(formula= value ~ EDAD*ESCOLARIDAD*MoCA*CRI_Total, data = k)

step(object = l, direction = "both", trace = 1)

modeloDPESC <- (lm(formula = value ~ EDAD * ESCOLARIDAD * MoCA * CRI_Total, data = k))
summary(modeloDPESC)
