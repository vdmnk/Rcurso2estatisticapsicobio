library(haven)
library(nlme)

banco3 <- read_sav("C:/Users/vdomi/Desktop/banco3.sav")


# Modelo resp com matriz simétrica:
modelo_resp_simetrica <- lme(
  fixed = resp ~ tempo + drug + tempo*drug,
  data = banco3, 
  random = resp ~ 1|id, 
  correlation = corCompSymm(form = ~1|id)
)
summary(modelo_resp_simetrica)


# Modelo resp com matriz AR1:
modelo_resp_AR1 <- lme(
  fixed = resp ~ tempo + drug + tempo*drug,
  data = banco3, 
  random = resp ~ 1|id, 
  correlation = corAR1(form = ~1|id)
)
summary(modelo_resp_AR1)


# Modelo resp com matriz diagonal:
modelo_resp_diagonal <- lme(
  fixed = resp ~ tempo + drug + tempo*drug,
  data = banco3, 
  random = resp ~ 1|id, 
  correlation = corSpher(form = ~1|id)
)
summary(modelo_resp_diagonal)


# Modelo resp com matriz não-estruturada:
modelo_resp_nope <- lme(
  fixed = resp ~ tempo + drug + tempo*drug,
  data = banco3, 
  random = resp ~ 1|id, 
  correlation = NULL
)
summary(modelo_resp_nope)



# Modelo pulse com matriz simétrica:
modelo_resp_simetrica <- lme(
  fixed = pulse ~ tempo + drug + tempo*drug,
  data = banco3, 
  random = pulse ~ 1|id, 
  correlation = corCompSymm(form = ~1|id)
)
summary(modelo_resp_simetrica)


# Modelo pulse com matriz AR1:
modelo_resp_AR1 <- lme(
  fixed = pulse ~ tempo + drug + tempo*drug,
  data = banco3, 
  random = pulse ~ 1|id, 
  correlation = corAR1(form = ~1|id)
)
summary(modelo_resp_AR1)


# Modelo resp com matriz diagonal:
modelo_resp_diagonal <- lme(
  fixed = pulse ~ tempo + drug + tempo*drug,
  data = banco3, 
  random = pulse ~ 1|id, 
  correlation = corSpher(form = ~1|id)
)
summary(modelo_resp_diagonal)


# Modelo resp com matriz não-estruturada:
modelo_resp_nope <- lme(
  fixed = pulse ~ tempo + drug + tempo*drug,
  data = banco3, 
  random = pulse ~ 1|id, 
  correlation = NULL
)
summary(modelo_resp_nope)