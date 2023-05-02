library(tidyverse)
library(readxl)
library(openxlsx)


dat <- read_excel("mineria de datos/conjunto_datos25abril2023.xlsx")
list<- pull(read_excel("mineria de datos/conjunto_datos25abril2023.xlsx",sheet = 2),2)
i <- list[1:34]


for (n in i) {
  #print(dat[[n]])
  
  a<-dat[[n]] %>%
    cut(10)
  b <- a
  
  levels(a)
  levels(a) <- seq(length(levels(a)))
  tab<-tibble( as.name(n)=a,
              var1 = b,
              var2 =dat[[n]])
  view(tab)           
  
}




