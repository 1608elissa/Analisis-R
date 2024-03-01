#### PARA HACER CORRECCION CON FDR #####

dataFDR <- read_xlsx("FDR.xlsx",sheet = "pvaluecalc")
FDR <- dataFDR$pvalueReal

# Calcular ajuste por FDR
adjusted_p_values <- p.adjust(FDR, method = "BH")
FDRCorreg <- data.frame(P_Value = FDR, Adjusted_P_Value = adjusted_p_values)

print(FDRCorreg)

write_xlsx(FDRCorreg, "FDRCOR.xlsx")
