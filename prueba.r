# Vamos a simular el experimento de la urna

# Creamos la urna
u <- c(
  "R", "R", "R", "R",
  "B", "B", "B", "B", "B", "B", "B"
)

# Primero lo hacemos CON repo
una_tirada <- sample(u, 2, replace = TRUE)
sum(una_tirada == "R") # Este comando cuenta # rojas

# Queremos ''calcular'' la probabilidad de que
# haya al menos una Roja (evento A).

# ¿Cómo lo resolvemos? Repetimos muchas veces la extraccion
# y cuento la cantidad de veces que ocurrió el éxito
simular_muestra <- function(n) {
  replicate(n, {
    extraccion <- sample(u, 2, replace = TRUE)
    sum(extraccion == "R") > 0
  })
}
estimador_q1 <- function(muestra) {
  mean(muestra)
}

estimador_q2 <- function(muestra) {
  mean(muestra) / (1 - mean(muestra))
}

plot(estimador_q1(simular_muestra(1000)))



XY <- read.csv("income.data.csv", row.names = 1)
#beta1 <- sum((XY$income - mean(XY$income)) * (XY$happiness - mean(XY$happiness))) / sum((XY$income - mean(XY$income)) ^ 2)

beta1 <- sum(XY$income * XY$happiness - mean(XY$happiness) * XY$income) /  sum(XY$income ^ 2 - mean(XY$income) * XY$income)

#beta1 <- sum(XY$happiness - mean(XY$happiness)) / sum(XY$income - mean(XY$income))

beta0 <- sum(XY$happiness) / nrow(XY) - beta1 * sum(XY$income) / nrow(XY)
plot(XY)
abline(a = beta0, b = beta1, col = "aquamarine3", lw = 3)