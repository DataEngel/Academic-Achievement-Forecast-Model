library('ggplot2')
library('lattice')
library('caret')
library('parallel')
library('nnet')

rmse_fold <- function(pliegue, form, datos, nn_size){
  pliegue_logic <- seq_len(nrow(datos)) %in% pliegue
  prueba <- subset(datos, pliegue_logic)
  entrena <- subset(datos, !pliegue_logic)
  modelo <- nnet(form, data = entrena, size = nn_size, linout = TRUE, trace = FALSE)
  response_name <- setdiff(names(datos), modelo$coefnames)
  Y_pronosticado <- predict(modelo, newdata = prueba)
  rmse <- RMSE(Y_pronosticado, prueba[[response_name]])
  rmse
}

#red neuronal

tamano_muestral <- 2000
neuronas <- 10
n_pliegues <- 10

c(
  'ECON_PERSONAS_HOGAR',
  'ECON_CUARTOS',
  'ECON_SN_LAVADORA',
  'ECON_SN_NEVERA',
  'ECON_SN_HORNO',
  'ECON_SN_DVD',
  'ECON_SN_MICROHONDAS',
  'ECON_SN_AUTOMOVIL',
  'MATEMATICAS_PUNT'
) -> variables

indices_muestra <- seq_len(nrow(SB11_20111)) %in% sample(seq_len(nrow(SB11_20111)), tamano_muestral)

muestra <- subset(SB11_20111, subset = indices_muestra, select = variables)
muestra <- na.omit(muestra)

createFolds(muestra$MATEMATICAS_PUNT, k = n_pliegues) -> pliegues

mclapply(
  pliegues,
  rmse_fold,
  MATEMATICAS_PUNT ~ .,
  muestra,
  nn_size = neuronas,
  mc.cores = 1
) -> rmse_pliegues

rmse_pliegues <- unlist(rmse_pliegues)
mean(rmse_pliegues)

plot(rmse_pliegues, ylim = c(0, 14))
abline(h = mean(rmse_pliegues), col =2, lwd = 2)