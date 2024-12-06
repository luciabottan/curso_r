library(tidyverse)

cars |> 
  ggplot() +
  geom_point(aes(x = speed, y = dist)) +
  labs(x = "Velocidad (millas/h)", y = "Distancia de frenado (pies)")

m_cars <- lm(dist ~ speed, data = cars)

summary(m_cars)

# Calculamos a mano la bondad de ajuste del modelo:
res <- resid(m_cars)

summary(res)

RSS <- sum(res^2)
RMS <- RSS/(nrow(cars)-2) 
sqrt(RMS)

SSY <- sum((cars$dist - mean(cars$dist))^2)
R2 <- (SSY-RSS)/SSY
R2
anova(m_cars) 

# La tabla ANOVA tiene una serie de columnas que resumen la partición de la suma de cuadrados
# La F se calcula bajo el supuesto de b = 0
# Un p-valor de X quiere decir que X de cada 10 veces obtendremos por azar un F-ratio igual al que hemos obtenido con las observaciones.

#MINIMO DE 10 OBSERVACIONES POR CADA PARAMETRO ESTIMADO!!!!!
install.packages("performance")
library(performance)
install.packages("sjPlot")
install.packages("insight")
library(insight)
library(sjPlot)
# install.packages("broom")
library(broom) # https://broom.tidymodels.org/articles/broom.html
library(GGally)
install.packages("lm")
library(lm)
library(performance)
ozono <- read_delim(file = "Ozone.txt", delim = "\t") 
ozono

ggpairs(ozono |> select(rad, temp, wind, ozone),
        lower = list(continuous = wrap("smooth", method = "loess", color = "darkslategrey", alpha = 0.1)),
        diag = list(continuous = wrap("barDiag")))  

m_ozono <- lm(ozone ~ rad + temp + wind, data = ozono)

summary(m_ozono)
anova(m_ozono)

# Comprobamos residuos
x11()

check_model(m_ozono)

# Comprobamos residuos parciales
plot_model(m_ozono, terms = c("rad", "temp", "wind"), show.data = TRUE, type = "resid")

# Comprobamos colinealidad
check_collinearity(m_ozono)

# El factor de inflación de la varianza (VIF) proporciona una estimación cuantitativa de la multicolinealidad entre covariables en un análisis de regresión. Este índice mide hasta qué punto la varianza de un coeficiente de regresión se incrementa a causa de la colinealidad. En cuanto a los valores de VIF a partir de los cuales hay que tomar en serio la colinealidad no existe un consenso claro (5 o 10). En caso de alta colinealidad se puede eliminar la variable con un VIF superior y volver a calcular el VIF.

# Modelo con interacciones 

ozono <- ozono %>% 
  mutate(rads = as.vector(scale(rad)),
         temps = as.vector(scale(temp)),
         winds = as.vector(scale(wind)))

m_ozono_int <- lm(ozone ~ rad*temp + wind*temp, data = ozono)
summary(m_ozono_int)
anova(m_ozono_int)
confint(m_ozono_int)

# comprobar supuestos
check_model(m_ozono_int)

plot_model(m_ozono_int, terms = c("rad", "temp", "wind"), show.data = TRUE, type = "resid")

# Para representar las predicciones del gráfico estandarizar variablbes cuadno hay interacciones, estimadores de las variables
graficar las interacciones, valor de bondad de ajuste r2 por ejemplo, graficos de los residuos (suplementario)
f del modelo, error del modelo 
plot_model(m_ozono_int, type = "pred",
           terms = c("temp","rad[90, 270]")) +
  labs(title = "", 
       x = "Temperature", y = "Ozone concentration", color = "Radiation") + 
  theme_bw()

plot_model(m_ozono_int, type = "pred",
           terms = c("temp","wind[7, 13]")) +
  labs(title = "", 
       x = "Temperature", y = "Ozone concentration", color = "Wind") +
  theme_bw()

# Tablas de resultados
#estandarizar las variables para las interacciones!! siempre 
tidy(m_ozono_int)
tidy(anova(m_ozono_int))
glance(m_ozono_int)



# Ajustamos un modelo lineal


# install.packages("emmeans")
library(emmeans)
# Guia de emmeans: https://rvlenth.github.io/emmeans/articles/AQuickStart.html

pinos <- read_delim(file = "RxF_growth.csv", delim = ",",
                    col_types = list(Irrig = "f",
                                     Fert = "f"))

# pinos contiene datos de un experimento en el que se plantaron 144 plántulas de Pinus pinea en un campo abandonado y se sometieron a un experimento factorial con 4 niveles de riego (0, 150, 300 y 600 mm año⁻¹) y 3 tratamientos de fertilización nitrogenada (0, 150 y 300 kg N ha⁻¹) distribuidos aleatoriamente.

# Queremos saber si el incremento en diamétro (DI) estuvo condicionado por la irrigación
# install.packages("emmeans")
library(emmeans)
# Guia de emmeans: https://rvlenth.github.io/emmeans/articles/AQuickStart.html

pinos <- read_delim(file = "RxF_growth.csv", delim = ",",
                    col_types = list(Irrig = "f",
                                     Fert = "f"))

# pinos contiene datos de un experimento en el que se plantaron 144 plántulas de Pinus pinea en un campo abandonado y se sometieron a un experimento factorial con 4 niveles de riego (0, 150, 300 y 600 mm año⁻¹) y 3 tratamientos de fertilización nitrogenada (0, 150 y 300 kg N ha⁻¹) distribuidos aleatoriamente.

# Queremos saber si el incremento en diamétro (DI) estuvo condicionado por la irrigación

pinos
pinos
m_pinos <- lm(DI ~ Irrig, data = pinos)
summary(m_pinos)


# ¿Qué significa cada parámetro?
# intercept = media del nivel de referencia, variables dummy

pinos <- pinos |> 
  mutate(f1 = fitted(m_pinos))

ggplot(pinos) +
  geom_point(aes(x = f1, y = DI)) +
  labs(y = "Observados", x = "Predichos")


# Check residuals
x11()
check_model(m_pinos)

# Pero... no hemos acabado
# Una de las principales razones de usar factores es conocer las diferencias entre los niveles del factor

paircomp <- emmeans::emmeans(m_pinos, specs = pairwise ~ Irrig)
paircomp

# Representación del modelo

plot_model(m_pinos, type = "pred", terms = "Irrig", show.data = TRUE, jitter = 0.5) +
  labs(title = "", 
       x = "Tratamiento de riego", y = "Incremento en diámetro") +
  theme_bw()

plot(paircomp, comparison = TRUE) +
  labs(title = "", 
       x = "Tratamiento de riego", y = "Incremento en diámetro") +
  theme_bw()

# Tablas de resultados

tidy(m_pinos)

