
RC <-filter(data, TR_RC == "RC", !VAL=="TOT") #%>%
tapply(RC$value, list(RC$DECADA,RC$VAL), mean)
tapply(RC$value, list(RC$DECADA,RC$COND), mean)
tapply(RC$value, list(RC$COND,RC$VAL), mean)
tapply(RC$value, list(RC$DECADA,RC$VAL,RC$COND), mean)

    
    
TR <-filter(data, TR_RC == "TR", !VAL=="TOT") #%>%
tapply(TR$value, list(TR$DECADA,TR$VAL), mean)
tapply(TR$value, list(TR$DECADA,TR$COND), mean)
tapply(TR$value, list(TR$COND,TR$VAL), mean)
tapply(TR$value, list(TR$DECADA,TR$VAL,TR$COND), mean)
    