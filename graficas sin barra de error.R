filter(data, TR_RC == "RC", !VAL=="TOT") %>%
ggplot(aes(y=value, x=VAL, fill= COND)) +
  geom_col(position = "dodge")

filter(data, TR_RC == "RC", !VAL=="TOT") %>%
  ggplot(aes(y=value, x=VAL, fill= DECADA)) +
  geom_col(position = "dodge") 

filter(data, TR_RC == "RC", !VAL=="TOT") %>%
  ggplot(aes(y=value, x=VAL, fill= COND)) +
  geom_col(position = "dodge") 
