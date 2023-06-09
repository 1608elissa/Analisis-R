source("summarySE.R")

#### RC ####

filter(data, TR_RC == "RC", !VAL=="TOT") %>%
summarySE(measurevar = "value", groupvars = c("COND", "VAL")) %>%
  ggplot(aes(y=value, x=VAL, fill= COND)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge"

  )

filter(data, TR_RC == "RC", !VAL=="TOT") %>%
  summarySE(measurevar = "value", groupvars = c("DECADA", "VAL")) %>%
  ggplot(aes(y=value, x=VAL, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge"
                
  )

filter(data, TR_RC == "RC", !VAL=="TOT") %>%
  summarySE(measurevar = "value", groupvars = c("COND", "DECADA")) %>%
  ggplot(aes(y=value, x=COND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge"
                
  )

filter(data, TR_RC == "RC", !VAL=="TOT") %>%
  summarySE(measurevar = "value", groupvars = c("COND", "DECADA", "VAL")) %>%
  ggplot(aes(y=value, x=COND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge"
                
  )+
  facet_grid(rows= vars(VAL))

###### TR #####


filter(data, TR_RC == "TR", !VAL=="TOT") %>%
  summarySE(measurevar = "value", groupvars = c("COND", "VAL")) %>%
  ggplot(aes(y=value, x=COND, fill= VAL)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge"
                
  )

filter(data, TR_RC == "TR", !VAL=="TOT") %>%
  summarySE(measurevar = "value", groupvars = c("DECADA", "VAL")) %>%
  ggplot(aes(y=value, x=VAL, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge"
                
  )

filter(data, TR_RC == "TR", !VAL=="TOT") %>%
  summarySE(measurevar = "value", groupvars = c("COND", "DECADA")) %>%
  ggplot(aes(y=value, x=COND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge"
                
  )

filter(data, TR_RC == "TR", !VAL=="TOT") %>%
  summarySE(measurevar = "value", groupvars = c("COND", "DECADA", "VAL")) %>%
  ggplot(aes(y=value, x=COND, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge"
                
  )+
  facet_grid(rows= vars(VAL))

filter(data, TR_RC == "TR", !VAL=="TOT") %>%
  summarySE(measurevar = "value", groupvars = c("COND")) %>%
  ggplot(aes(y=value, x=COND, fill= COND)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge"
                
  )

filter(data, TR_RC == "TR", !VAL=="TOT") %>%
  summarySE(measurevar = "value", groupvars = c("DECADA")) %>%
  ggplot(aes(y=value, x=DECADA, fill= DECADA)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = "dodge"
                
  )
