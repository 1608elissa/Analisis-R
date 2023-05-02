library(tidyverse)
library(readxl)
library(openxlsx)


dat <- read_excel("mineria de datos/conjunto_datos25abril2023.xlsx")
list<- pull(read_excel("mineria de datos/conjunto_datos25abril2023.xlsx",sheet = 2),2)
i <- list[1:33]

wb<-createWorkbook("cosito")


for (n in i) {
  #print(dat[[n]])
  
  a<-dat[[n]]
  
  b <- a %>%
    cut(10)
  c <- b
  
  levels(b)
  levels(b) <- seq(length(levels(b)))
  tab<-tibble( n=as.numeric(a),
              varb = as.numeric(b),
              varc = as.numeric(c))
  
  tab[is.na(tab)] <- 666
  #tab <- tab %>% replace(is.na(.), as.factor(666))

 # view(tab)           
  addWorksheet(wb,n)
  writeDataTable(wb, n, tab)

}

openXL(wb) #abrir sin guardar
saveWorkbook(wb, "labes.xlsx", overwrite = TRUE) #guardar sin abrir




