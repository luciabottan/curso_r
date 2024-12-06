library(tidyverse)

install.packages("titanic") 
library(titanic)

intall.packages("palmerpenguins")
library(palmerpenguins)
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
        f