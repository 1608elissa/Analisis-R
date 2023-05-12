mod<- read_excel("mineria de datos/conjunto_datos25abril2023.xlsx",sheet = "cubetas")
filter(mod,AD33==1) %>%
  view()

wb<-createWorkbook("noms")

for (i in names(mod)) {

  tab <- as.data.frame(table(mod[i]))
  addWorksheet(wb, i)
  writeDataTable(wb, i,tab)
    
}

saveWorkbook(wb, "mineria de datos/MagiaDeSirena.xlsx", overwrite = TRUE) #guardar sin abrir





wb<-createWorkbook("noms")

for (i in names(mod)) {
  
  tab <- as.data.frame(table(mod[i]))
  addWorksheet(wb, i)
  writeDataTable(wb, i,tab)
  
}

saveWorkbook(wb, "mineria de datos/MagiaDeSirena.xlsx", overwrite = TRUE) #guardar sin abrir
