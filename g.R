#con mis datos
# install.packages("glmmTMB")
library(glmmTMB)
# install.packages("lmerTest")
library(lmerTest)
# install.packages("broom.mixed")
library(broom.mixed)
library(ggplot2)
library(palmerpenguins)

# Un solo grupo de datos

lm(flipper_length_mm ~ body_mass_g, data = penguins)

m_mixto <- glmmTMB(flipper_length_mm ~ body_mass_g + (1|year), data = penguins)
update.packages(ask = "sjPlot")
update.packages(ask = "insight")
summary(m_mixto)
x11()
check_model(m_mixto)

plot(m_mixto)


library(titanic)

titanic <- titanic_train |> 
  mutate(Pclass = factor(Pclass))


m_lin <- lm(Survived ~ Pclass, data = titanic)

hist(resid(m_lin))

m_binomial <- glmmTMB(Survived ~ Pclass, data= titanic, family = "binomial")

summary(m_binomial)

# La estimación de los parámetros (= probabilidad de supervivencia) está en escala logit. logit(p) = ln(p / (1-p))

# Intercepto: probabilidad de supervivencia del nivel de referencia (primera clase)
library(tidyverse)
co_binomial <- fixef(m_binomial)

titanic |> 
  group_by(Pclass) |> 
  mutate(n_class = n()) |> 
  group_by(Pclass, n_class, Survived) |> 
  summarise(n_sur = n()) |> 
  mutate(per_sur = n_sur/n_class)
plogis(co_binomial[[1]][1])


# Para el resto de parámetros:

plogis(co_binomial[[1]][1]+co_binomial[[1]][2])

plot_model(m_binomial, type = "pred")
install.packages("DHARMa")

library(DHARMa)
simulateResiduals(m_binomial, plot = TRUE)
#verificar sobredispersion , en la binomial tambien 