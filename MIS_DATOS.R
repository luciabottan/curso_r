library(readxl)
mis_datos <- read_excel("mis.datos.xlsx")
View(mis_datos)

mis_datos

plot(data=mis_datos, riq.mon)

ggplot(data = mis_datos) + aes(x = riq.mon) + geom_density()

ggplot(data = mis_datos) + aes(x = cob.mono) + geom_density()

ggplot(data = mis_datos) + aes(x = riq.mon) + geom_boxplot()

ggplot(data = mis_datos) + aes(x = cob.mono, y = gdd.prim.ver) + geom_jitter()

ggplot(data = mis_datos) + aes(x = riq.mon, y = vsw.prim, ) + geom_jitter()

m_riq.mon <- glmmTMB(riq.mon ~ vsw.prim + pp.inv.prim.ver + vsw.prim*pp.inv.prim.ver  + (1|chapa), data= mis_datos, family = "gaussian")

summary(m_riq.mon)
AIC(m_riq.mon)

na.omit(mis_datos)
mis_datos_limpios <- na.omit(mis_datos[, c("riq.mon", "vsw.prim", "pp.inv.prim.ver", "chapa")])
m_riq.mon.poisson <- glmmTMB(riq.mon ~ vsw.prim + pp.inv.prim.ver + vsw.prim * pp.inv.prim.ver + (1 | chapa), 
                             data = mis_datos_limpios, 
                             family = "poisson")

summary(m_riq.mon.poisson.poisson)
AIC(m_riq.mon)
