filter(data, TR_RC == "RC", !VAL=="TOT", !TIPO=="TO") %>%
  ggplot(aes(y=value, x=COND, fill= COND)) + theme_minimal() +
  scale_fill_brewer(palette="PuRd") +
  geom_boxplot() +
  geom_jitter(size=.4) + coord_cartesian(ylim = c(80, 100))


filter(data, TR_RC == "RC", !VAL=="TOT", !TIPO=="TO") %>%
  ggplot(aes(y=value, x=COND, fill= DECADA)) + theme_minimal() +
  scale_fill_brewer(palette="PuRd") +
  geom_boxplot() +
  geom_jitter(size=.4) + coord_cartesian(ylim = c(70, 100))