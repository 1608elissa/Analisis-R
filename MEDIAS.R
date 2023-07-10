library(psych)


filter(data, COND== "AMP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %$%
  describeBy(value, DECADA)

filter(data, COND== "SUP", VD == "DUR", VAL=="TOT", TIPO=="ROS") %$%
  describeBy(value, DECADA)

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ROSTROS") %$%
  describeBy(value, DECADA)

filter(data, VD == "EIDP", COND== "DP", VAL=="TOT", TIPO=="ESCENAS") %$%
  describeBy(value, DECADA)
