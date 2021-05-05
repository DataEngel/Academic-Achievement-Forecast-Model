
library("saber")


data("SB11_20112")


# Intervalos de confianza de la media -------------------------------------

table(SB11_20111$ECON_SN_INTERNET)

# ¿el internet tiene que ver con el puntaje en fisica?

tamano_muestral <- 300
iteraciones <- 50

poblacion_A <- SB11_20112$FISICA_PUNT[SB11_20112$ECON_SN_INTERNET == 0]
media_pob_A <- mean(poblacion_A, na.rm = TRUE)

poblacion_B <- SB11_20112$FISICA_PUNT[SB11_20112$ECON_SN_INTERNET == 1]
media_pob_B <- mean(poblacion_B, na.rm = TRUE)

plot(media_pob_A, media_pob_B, col = 4, pch= 20)
abline(0, 1)

for (i in seq_len(iteraciones)){
  muestra <- sample(seq_len(nrow(SB11_20112)), tamano_muestral)
  
  cuales_A <- seq_len(nrow(SB11_20112)) %in% muestra & SB11_20112$ECON_SN_INTERNET == 0
  muestra_A <- SB11_20112$FISICA_PUNT[cuales_A]
  
  media_muestral_A <- mean(muestra_A, na.rm = TRUE)
  t_test_A <- t.test(muestra_A)
  intervalo_A <- t_test_A$conf.int
  LI_A <- min(intervalo_A)
  LS_A <- max(intervalo_A)
  
  cuales_B <- seq_len(nrow(SB11_20112)) %in% muestra & SB11_20112$ECON_SN_INTERNET == 1
  muestra_B <- SB11_20112$FISICA_PUNT[cuales_B]
  
  media_muestral_B <- mean(muestra_B, na.rm = TRUE)
  t_test_B <- t.test(muestra_B)
  intervalo_B <- t_test_B$conf.int
  LI_B <- min(intervalo_B)
  LS_B <- max(intervalo_B)
  
  rect(LI_A, LI_B, LS_A, LS_B)
  
}

points(media_pob_A, media_pob_B, col = 4, pch = 20, cex = 4)
