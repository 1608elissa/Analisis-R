mod<- read_excel("mineria de datos/conjunto_datos25abril2023.xlsx",sheet = "cubetas")
filter(mod,AD33==1)

####part2####

wb<-createWorkbook("noms")
tab <- tibble()

for (i in names(mod)) {
  
  if (nrow(tab) == 0) {
  tab <-  as_tibble(table(mod[i]))
  names(tab) <- c(i, paste("n_", i))
  } else {
  
  tab2 <- as_tibble(table(mod[i]))
  names(tab2) <- c(i, paste("n_", i))
  tab<- gdata::cbindX(tab,tab2)
  }
  rm(tab2, i)
}
addWorksheet(wb, "prueba1")
writeDataTable(wb, "prueba1",tab)
openXL(wb)
saveWorkbook(wb, "mineria de datos/MagiaDeSirena2.xlsx", overwrite = TRUE) #guardar sin abrir
