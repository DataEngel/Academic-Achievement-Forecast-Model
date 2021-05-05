# Interpretacion ----------------------------------------------------------

tamano_muestral <- 5000

indices_muestra <- seq_len(nrow(SB11_20111)) %in% sample(seq_len(nrow(SB11_20111)), tamano_muestral)

muestra <- subset(SB11_20111, subset = indices_muestra, select = variables)
muestra <- na.omit(muestra)

red_neuronal <- nnet(MATEMATICAS_PUNT ~ ., data=muestra, size=neuronas, linout = TRUE)

predict(red_neuronal, newdata = SB11_20111) -> puntaje_pronosticado

nuevo_puntaje_mat <- SB11_20111$MATEMATICAS_PUNT - puntaje_pronosticado
nuevo_puntaje_mat <- na.omit(nuevo_puntaje_mat)
plot(density(nuevo_puntaje_mat))