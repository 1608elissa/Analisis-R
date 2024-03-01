
# Graficar la regresión con líneas verticales 
abline(v = c(0, 1, -1), col = "red", lty = 2)

# Sombrar el área entre las líneas de 0 y 1
rect(0, par("usr")[3], 1, par("usr")[4], col = rgb(0.8, 0.8, 0.8, 0.5), border = NA)
