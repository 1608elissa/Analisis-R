library(tidyverse)

a <- rnorm(200,100,1)
a %<>% cut(7)

a
View(a)
#######
library(readxl)
dat <- read_excel("mineria de datos/conjunto_datos25abril2023.xlsx")
a<-dat$AB17

levels(a)
levels(a) <- seq(length(levels(a)))
tab<-tibble("AB17"=a,
            "2"=dat$AB17)
view(tab)           


table(cut(a, 15) )

library(openxlsx)

wb<-createWorkbook("cosito")
addWorksheet(wb,"labels")







writeDataTable(wb, "labels", tab)
openXL(wb)
saveWorkbook(wb, "labes.xlsx", overwrite = TRUE)
