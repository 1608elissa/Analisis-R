filter(data2, COND== "ROSTROS", TIPO== "Z", CRI_Total_CAT=="LOW") %$%
  lm(formula= value ~ AMP_ROS_Z, data =.)%>%
  summary() 

filter(data2, COND== "ROSTROS", TIPO== "Z", CRI_Total_CAT=="HIGH") %$%
  lm(formula= value ~ AMP_ROS_Z, data =.)%>%
  summary() 

filter(data2, COND== "ROSTROS", TIPO== "Z", CRI_Total_CAT=="LOW")%>%
  ggplot(aes(x = AMP_ROS_Z, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#8B636C", lwd = 1.5) +
  labs(x = "ENHANCEMENT INDEX", y = "WME/AF") +  
  theme_classic()+ 
  ylim(-2,2) +
  xlim(-2,2) +
  theme(text = element_text(size = 17))

filter(data2, COND== "ROSTROS", TIPO== "Z", CRI_Total_CAT=="HIGH")%>%
  ggplot(aes(x = AMP_ROS_Z, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#8B936C", lwd = 1.5) +
  labs(x = "ENHANCEMENT INDEX", y = "WME/AF") +  
  theme_classic()+ 
  ylim(-2,2) +
  xlim(-2,2) +
  theme(text = element_text(size = 17))




filter(data2, COND== "ESCENAS", TIPO== "Z", CRI_Total_CAT=="LOW") %$%
  lm(formula= value ~ SUP_ROS_Z, data =.)%>%
  summary() 

filter(data2, COND== "ESCENAS", TIPO== "Z", CRI_Total_CAT=="HIGH") %$%
  lm(formula= value ~ SUP_ROS_Z, data =.)%>%
  summary() 

filter(data2, COND== "ESCENAS", TIPO== "Z", CRI_Total_CAT=="LOW")%>%
  ggplot(aes(x = SUP_ROS_Z, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#8B636C", lwd = 1.5) +
  labs(x = "SUPPRESSION INDEX", y = "WME/IF") +  
  theme_classic()+ 
  ylim(-2,2) +
  xlim(-2,2) +
  theme(text = element_text(size = 17))

filter(data2, COND== "ESCENAS", TIPO== "Z", CRI_Total_CAT=="HIGH")%>%
  ggplot(aes(x = SUP_ROS_Z, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#8B936C", lwd = 1.5) +
  labs(x = "SUPPRESSION INDEX", y = "WME/IF") +  
  theme_classic()+ 
  ylim(-2,2) +
  xlim(-2,2) +
  theme(text = element_text(size = 17))




filter(data2, COND== "ROSTROS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %$%
  wilcox.test(value ~ CRI_Total_CAT, data =.)

filter(data2, COND== "ROSTROS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %>%
  ggplot(aes(y=value, x=CRI_Total_CAT, fill=CRI_Total_CAT)) + 
  scale_fill_brewer(palette="RdPu") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "CogRes Group", y = "WME/AF") +
  theme_classic()+ 
  scale_x_discrete(limits = c("LOW","HIGH")) +
  theme(legend.position = "none") +
  ylim (-2, 3) +
  theme(text = element_text(size = 17))



filter(data2, COND== "ESCENAS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %$%
  wilcox.test(value ~ CRI_Total_CAT, data =.)

filter(data2, COND== "ESCENAS", TIPO== "Z", !CRI_Total_CAT=="MEDIO") %>%
  ggplot(aes(y=value, x=CRI_Total_CAT, fill=CRI_Total_CAT)) + 
  scale_fill_brewer(palette="PiYG") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  labs(x = "CogRes Group", y = "WME/IF") +
  theme_classic()+ 
  scale_x_discrete(limits = c("LOW","HIGH")) +
  theme(legend.position = "none") +
  ylim (-2, 3) +
  theme(text = element_text(size = 17))



