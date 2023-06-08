library(tidyverse)
library(magrittr)
library(readxl)
library(openxlsx)


dat <- read_excel("mineria de datos/conjunto_datos25abril2023 sin tabla.xlsx",sheet = "conjunto_datos")
list<- pull(read_excel("mineria de datos/conjunto_datos25abril2023.xlsx",sheet = "Sheet1"),2)
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



for (i in 1:length(dat$AC28IMC)) {
  
  if (dat$AC28IMC[i]==666) {
    dat$AC28IMC[i] <- 666
    
  } else if (dat$AC28IMC[i] < 18.5) {
    
    dat$AC28IMC[i] <- 1
    
  }else if (dat$AC28IMC[i] < 24.9) {
    dat$AC28IMC[i] <- 2
  }else if (dat$AC28IMC[i] < 29.9) {
    dat$AC28IMC[i] <- 3
  }else {
    dat$AC28IMC[i] <- 4
  }

  
}


addWorksheet(wb,"AC28IMC")
writeDataTable(wb,"AC28IMC" , as.tibble(dat$AC28IMC))

openXL(wb) #abrir sin guardar
saveWorkbook(wb, "labes.xlsx", overwrite = TRUE) #guardar sin abrir





for (i in 1:length(dat$AC27IMC)) {
  
  if (dat$AC27IMC[i]==666) {
    dat$AC27IMC[i] <- 666
    
  } else if (dat$AC27IMC[i] < 18.5) {
    
    dat$AC27IMC[i] <- 1
    
  }else if (dat$AC27IMC[i] < 24.9) {
    dat$AC27IMC[i] <- 2
  }else if (dat$AC27IMC[i] < 29.9) {
    dat$AC27IMC[i] <- 3
  }else {
    dat$AC27IMC[i] <- 4
  }
  
  
}

addWorksheet(wb,"AC27IMC")
writeDataTable(wb,"AC27IMC" , as.tibble(dat$AC27IMC))

openXL(wb) #abrir sin guardar
