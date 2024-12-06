library(tidyverse)

install.packages("titanic") 
library(titanic)

intall.packages("palmerpenguins")
library(palmerpenguins)
library(ggplot2)
#cargamos los datos
titanic <- titanic::titanic_train # :: dentro del paquete quiere solo una parte del paquete especifico (sin library, yo hice las dos)

head(titanic)

ggplot(data = titanic)

ggplot(data = titanic, aes(x = Age, y = Fare ))

ggplot() + geom_point(data = titanic, aes(x = Age, y = Fare ))

ggplot() + geom_jitter(data = titanic, aes(x = Pclass, y = Fare )) #geom_jitter mete aleatoreidad los puntos 
#regomendacion guardar graficos desde r con ggsave 

# Como guardar plots satisfactorios

plotqmegusta <- ggplot() + 
  geom_point(data = titanic, aes(x = Age, y = Fare))

ggsave(filename = "farebyage.jpg", plot = plotqmegusta, width = 12, height = 9, units = "cm", dpi = 300)

ggsave(filename = "farebyage.pdf", plot = plotqmegusta, width = 12, height = 9, units = "cm")  

titanic <- titanic

#Representa un gráfico para ver la relación entre el sexo de los pasajeros y la clase con la base de datos titanic.  

grafico1 <- ggplot() + geom_jitter(data = titanic,  aes(x= Sex, y = Pclass, color = Sex))

grafico1

#¿Cómo modificarías el siguiente código para representar la puerta de embarque con diferentes formas pero los puntos de color rosa?

plot.ejer<-  ggplot(data = titanic) + 
  geom_point(aes(x = Age, y = Fare, shape = Embarked),  color = "deeppink", ) +
  theme_classic()
plot.ejer + labs()

titanic <- titanic |> 
  mutate(Pclass = factor(Pclass, levels = c(3, 2, 1), 
                         labels = c("Tercera", "Segunda", "Primera")))

summary(titanic$Pclass)
#Cambia la posición de los ejes X e Y en el sistema de coordenadas de pnumcat.

pnumcat <- ggplot(data = titanic) +    
  geom_boxplot(aes(x = Pclass, y = Age))  

pnumcat  


pnumcat +    
  labs(title = "Edad de los pasajeros según su clase",      
       x = "Clase",      
       y = "Edad (años)") #title, subtitle, x, y, caption

pnumcat + coord_flip()

miplot <- ggplot(data = titanic,    
                 aes(x = Age, y = Fare, color = Sex)) +    
  geom_point()  
miplot  

miplot +    
  facet_grid(Pclass~., scales = "free")  #filas ~ columna, en este caso columnas no. si saco scales me escala ttodos los graficos igual

miplot +    
  facet_grid(Pclass~Embarked) 


miplot +    
  facet_wrap(Embarked~., ncol=3)

#posicion
ggplot(data = titanic) +    
  geom_bar(aes(x = Pclass, fill = Sex))  

ggplot(data = titanic) +    
  geom_bar(aes(x = Pclass, fill = Sex), 
           position = "dodge") #esquivar  

ggplot(data = titanic) +    
  geom_bar(aes(x = Pclass, fill = Sex), 
           position = "fill") #rellenar 

#escalas

miplot +
  scale_color_manual(values = c("darkgreen", "chartreuse")) +
  scale_y_sqrt(breaks = c(9, 16, 25), labels = c("a", "b", "c"))  
#¿Qué harías para cambiar la escala de la edad a un 
#degradado de colores de azul a amarillo en el siguiente gráfico?

plot2 = ggplot(data = titanic, aes(x = Age, y = Fare, color = Age)) +   
  geom_point(size = 3)  

# control+ shift +c = para poner numeral a varias lineas

plot2 + scale_color_gradient(low="blue", high = "yellow")+
  theme_classic()

# Dibuja una linea negra que representa los ejes de miplot y quita el fondo del gráfico.
# Existen temas configurados por defecto y un asistente que te ayuda a personalizar el gráfico.

miplot + theme_classic()

miplot + theme_light()
miplot + theme_void()
miplot +    
  theme(axis.line  = element_line(color = "black"))  +
  theme(panel.background = element_blank()) 
install.packages("ggThemeAssist")  
library(ggThemeAssist)


miplot + theme(panel.background = element_rect(fill = NA))


# Boxplots
ggplot(data = titanic) + 
  geom_boxplot(aes(y = Age))

# Jitter
ggplot(data = titanic) + 
  geom_jitter(aes(x = 1, y = Age))

# group = 1 hace que considere toda la variable como un todo
install.packages("palmerpenguins")
library(palmerpenguins)

# Cleveland plot
library(tidyverse)
penguins= pal
penguins |> 
  mutate(orden = 1:nrow(penguins)) |> 
  ggplot() + 
  geom_point(aes(x = body_mass_g, y = orden)) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(size = 0.1, color = "grey90"), 
        panel.background = element_rect(fill = "white", color = "black")) 

# De variables continuas codicionados por variables categóricas

# Cleveland plot por grupos
penguins |> 
  group_by(island) |> 
  mutate(orden = 1:n()) |> 
  ggplot() + 
  geom_point(aes(x = body_mass_g, y = orden, col = island)) +
  facet_grid(island ~ ., scales = "free_y", space = "free_y") + 
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(size = 0.1, color = "grey90"), 
        panel.background = element_rect(fill = "white", color = "black"),
        legend.position = "none")


# Identificar outliers: ¿a qué observación pertenecen?

# install.packages("plotly")
library(plotly)

ggplotly(miplot) 

titanic |> 
  mutate(orden = 1:nrow(titanic)) |> 
  ggplot() + 
  geom_point(aes(x = Age, y = Fare)) +
  geom_text(aes(x = Age, y = Fare, label = orden), col = "red", size = 2) 


# Tras su identificación podemos: ¿quitarlos?, ¿ignorarlos?, comprobar los residuos del modelo que ajustemos para estas observaciones, ajustar el modelo con y sin estos datos, transformar la variable.

# Gestionar outliers con transformaciones

pellets <- read_delim(file = "pellets.txt") 
# Longitud y peso de heces de un gusano marino

ggplot(data = pellets) +
  geom_boxplot(aes(y = Length)) 

# Una variable respuesta en las categorias de otra explicativa

ggplot(data = titanic) + 
  geom_boxplot(aes(y = Fare, x = Pclass), alpha = 0.7)


# La relación de dos variables en las categorias de otra

ggplot(data = penguins) + 
  geom_point(aes(y = body_mass_g, x = flipper_length_mm)) +
  facet_wrap(.~species)

# Describe la distribución de las tarifas pagadas por los pasajeros con la base de datos titanic.
# Haz un histograma de las tarifas y cambia el número de intervalos para ver como cambia la distribución de la variable.
#no es muy normal
ggplot(titanic) +
  geom_density(aes(x= Fare))
plot3 <- ggplot(titanic) +
  geom_histogram(aes(x= Fare), bins = 30) 

ggplot(titanic) +
  geom_boxplot(aes(x= Fare)) 
#+  facet_grid(Pclass~.) 
#+  
  
plot3

titanic <- titanic |> 
  mutate(Survived = factor(Survived, levels = c(0, 1), labels = c("Muerto", "Vivo"))) #le digo que la variable es un factor

ggplot(data = titanic) + 
  geom_count(aes(x = Sex, y = Survived))
table(titanic$Sex, titanic$Survived)


#Haz un gráfico para explorar la relación entre la clase del billete y la supervivencia con la base de datos titanic.

ggplot(titanic) + geom_count(aes(x= Pclass, y = Survived))


ggplot(titanic) + geom_bar(aes(x= Pclass, y = Survived) , position = "fill")

#~ "alt gr" + "+" 
pnum <- ggplot(data = titanic, aes(x = Age, y = Fare)) + 
  geom_point(alpha = 0.5)

pnum

pnum + 
  
  #ggdist para combinacion de boxplot con violin plot , distribucion etc...
  
  geom_smooth()
