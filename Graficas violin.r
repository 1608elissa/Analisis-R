####SOLO %RC, TR e EI####
data <- read_xlsx("data.xlsx")%>%
  gather(tip_cond_val, value, -c("ID","DECADA","EDAD","SEXO")) %>%
  separate(tip_cond_val, c("TR_RC", "COND", "VAL"),
           sep = "_")
data$DECADA <- as.factor(data$DECADA)
data$COND <- as.factor(data$COND)
data$VAL <- as.factor(data$VAL)

source("summarySE.R")

#### RC

filter(data, TR_RC == "RC") %>%
  ggplot(aes(y=value, x=COND, fill= DECADA)) + theme_minimal() +
  scale_fill_brewer(palette="PuRd") +
  geom_boxplot() +
  geom_jitter(size=.2)

filter(data, TR_RC == "RC") %>%
  ggplot(aes(y=value, x=COND, fill= COND)) + theme_minimal() +
  scale_fill_brewer(palette="Pastel1") +
  geom_boxplot() +
  geom_jitter(size=.2)

filter(data, TR_RC == "RC") %>%
  ggplot(aes(y=value, x=DECADA, fill= DECADA)) + theme_minimal() +
  scale_fill_brewer(palette="PuRd") +
  geom_boxplot() +
  geom_jitter(size=.2) 

filter(data, TR_RC == "RC") %>%
  ggplot(aes(y=value, x=COND, fill= VAL)) + theme_minimal() +
  scale_fill_brewer(palette="Pastel1") +
  geom_boxplot() +
  geom_jitter(size=.2)


###### TR

filter(data, TR_RC == "TR") %>%
  ggplot(aes(y=value, x=COND, fill= DECADA)) + theme_minimal() +
  scale_fill_brewer(palette="PuRd") +
  geom_boxplot() +
  geom_jitter(size=.2)

filter(data, TR_RC == "TR") %>%
  ggplot(aes(y=value, x=DECADA, fill= DECADA)) + theme_minimal() +
  scale_fill_brewer(palette="PuRd") +
  geom_boxplot() +
  geom_jitter(size=.2)

filter(data, TR_RC == "TR") %>%
  ggplot(aes(y=value, x=COND, fill= COND)) + theme_minimal() +
  scale_fill_brewer(palette="Pastel1") +
  geom_boxplot() +
  geom_jitter(size=.2)


#### EI

filter(data, TR_RC == "EI") %>%
  ggplot(aes(y=value, x=COND, fill= DECADA)) + theme_minimal() +
  scale_fill_brewer(palette="PuRd") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  coord_cartesian(ylim = c(0, 30))

filter(data, TR_RC == "EI") %>%
  ggplot(aes(y=value, x=COND, fill= COND)) + theme_minimal() +
  scale_fill_brewer(palette="Pastel2") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  coord_cartesian(ylim = c(0, 30))

filter(data, TR_RC == "EI") %>%
  ggplot(aes(y=value, x=DECADA, fill= DECADA)) + theme_minimal() +
  scale_fill_brewer(palette="PuRd") +
  geom_boxplot() +
  geom_jitter(size=.2) +
  coord_cartesian(ylim = c(0, 30))



#### D PRIMA ####
data2 <- read_xlsx("data.xlsx", sheet = "dprima")%>%
  gather(tip_cond_val, value, -c("ID","DECADA","EDAD","SEXO")) %>%
  separate(tip_cond_val, c("COND", "VAL"),
           sep = "_")
data2$DECADA %<>% as.factor()
data2$COND %<>% as.factor()
data2$VAL %<>% as.factor()


filter(data2) %>%
  ggplot(aes(y=value, x=COND, fill= DECADA)) + theme_minimal() +
  scale_fill_brewer(palette="PuRd") +
  geom_boxplot() +
  geom_jitter(size=.2)

filter(data2) %>%
  ggplot(aes(y=value, x=COND, fill= COND)) + theme_minimal() +
  scale_fill_brewer(palette="Pastel2") +
  geom_boxplot() +
  geom_jitter(size=.2)

filter(data2) %>%
  ggplot(aes(y=value, x=DECADA, fill= DECADA)) + theme_minimal() +
  scale_fill_brewer(palette="PuRd") +
  geom_boxplot() +
  geom_jitter(size=.2)

