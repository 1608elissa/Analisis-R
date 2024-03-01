
modelo <- lm(y ~ x1 + x2 + x1*x2, data = tus_datos)

# Obtener el AIC del modelo
aic_modelo <- AIC(modelo)

# Visualizar el resultado
cat("El AIC del modelo es:", aic_modelo, "\n")




# ROSTROS + AMPLIFICACION + EDAD
ROESCOL <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*ESCOLARIDAD_Z*EDAD_Z, data =.)

aic_ROESCOL <- AIC(ROESCOL)

ROCRI <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*CRI_Total_Z*EDAD_Z, data =.)

aic_ROCRI <- AIC(ROCRI)

ROMOCA <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*MoCA_Z*EDAD_Z, data =.)

aic_ROMOCA <- AIC(ROMOCA)

ROAR <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*IDARE_R_Z*EDAD_Z, data =.)

aic_ROAR <- AIC(ROAR)

ROAE <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*IDARE_E_Z*EDAD_Z, data =.)

aic_ROAE <- AIC(ROAE)

ROER <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*IDERE_R_Z*EDAD_Z, data =.)

aic_ROER <- AIC(ROER)

ROEE <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*IDERE_E_Z*EDAD_Z, data =.)

aic_ROEE <- AIC(ROEE)

ROSN <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*SUEÑO_NOR_Z*EDAD_Z, data =.)

aic_ROSN <- AIC(ROSN)

ROS2 <- data %>%
  filter(COND=="ROSTROS", TIPO=="Z")%>%
  lm(formula= value ~ AMP_ROS_Z*SUEÑO_2DA_Z*EDAD_Z, data =.)

aic_ROS2 <- AIC(ROS2)


# ESCENAS + SUPRESION + EDAD
ESESCOL <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*ESCOLARIDAD_Z*EDAD_Z, data =.)

aic_ESESCOL <- AIC(ESESCOL)

ESCRI <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*CRI_Total_Z*EDAD_Z, data =.)

aic_ESCRI <- AIC(ESCRI)

ESMOCA <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*MoCA_Z*EDAD_Z, data =.)

aic_ESMOCA <- AIC(ESMOCA)

ESAR <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*IDARE_R_Z*EDAD_Z, data =.)

aic_ESAR <- AIC(ESAR)

ESAE <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*IDARE_E_Z*EDAD_Z, data =.)

aic_ESAE <- AIC(ESAE)

ESER <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*IDERE_R_Z*EDAD_Z, data =.)

aic_ESER <- AIC(ESER)

ESEE <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*IDERE_E_Z*EDAD_Z, data =.)

aic_ESEE <- AIC(ESEE)

ESSN <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*SUEÑO_NOR_Z*EDAD_Z, data =.)

aic_ESSN <- AIC(ESSN)

ESS2 <- data %>%
  filter(COND=="ESCENAS", TIPO=="Z")%>%
  lm(formula= value ~ SUP_ROS_Z*SUEÑO_2DA_Z*EDAD_Z, data =.)

aic_ESS2 <- AIC(ESS2)



# ROSTROS + AMPLIFICACION + EDAD
aic_ROESCOL <- AIC(ROESCOL)
aic_ROCRI <- AIC(ROCRI)
aic_ROMOCA <- AIC(ROMOCA)
aic_ROAR <- AIC(ROAR)
aic_ROAE <- AIC(ROAE)
aic_ROER <- AIC(ROER)
aic_ROEE <- AIC(ROEE)
aic_ROSN <- AIC(ROSN)
aic_ROS2 <- AIC(ROS2)

print(paste("El AIC del modelo es:", aic_ROESCOL))
print(paste("El AIC del modelo es:", aic_ROCRI))
print(paste("El AIC del modelo es:", aic_ROMOCA))
print(paste("El AIC del modelo es:", aic_ROAR))
print(paste("El AIC del modelo es:", aic_ROAE))
print(paste("El AIC del modelo es:", aic_ROER))
print(paste("El AIC del modelo es:", aic_ROEE))
print(paste("El AIC del modelo es:", aic_ROSN))
print(paste("El AIC del modelo es:", aic_ROS2))

ROSTROS_AMP_MOD <- data.frame(aic_ROESCOL,aic_ROCRI,aic_ROMOCA,aic_ROAR,aic_ROAE,
                              aic_ROER,aic_ROEE,aic_ROSN,aic_ROS2)

# ESCENAS + SUPRESION + EDAD
aic_ESESCOL <- AIC(ESESCOL)
aic_ESCRI <- AIC(ESCRI)
aic_ESMOCA <- AIC(ESMOCA)
aic_ESAR <- AIC(ESAR)
aic_ESAE <- AIC(ESAE)
aic_ESER <- AIC(ESER)
aic_ESEE <- AIC(ESEE)
aic_ESSN <- AIC(ESSN)
aic_ESS2 <- AIC(ESS2)

print(paste("El AIC del modelo es:", aic_ESESCOL))
print(paste("El AIC del modelo es:", aic_ESCRI))
print(paste("El AIC del modelo es:", aic_ESMOCA))
print(paste("El AIC del modelo es:", aic_ESAR))
print(paste("El AIC del modelo es:", aic_ESAE))
print(paste("El AIC del modelo es:", aic_ESER))
print(paste("El AIC del modelo es:", aic_ESEE))
print(paste("El AIC del modelo es:", aic_ESSN))
print(paste("El AIC del modelo es:", aic_ESS2))

ESCENAS_SUP_MOD <- data.frame(aic_ESESCOL,aic_ESCRI,aic_ESMOCA,aic_ESAR,aic_ESAE,
                              aic_ESER,aic_ESEE,aic_ESSN,aic_ESS2)

write_xlsx(list(ROSTROS_AMP_MOD = ROSTROS_AMP_MOD, 
                ESCENAS_SUP_MOD = ESCENAS_SUP_MOD),"Resultados AIC.xlsx")
