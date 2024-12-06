library(tidyverse)
#%>% # control+ shift + m = %>% 
  
taludes <- read_delim(file="taludes.csv", delim=".")

taludes

taludes_bien <- read_delim(file = "taludes.csv", 
                           delim = ",", col_types = list(Luz = "f", Agua = "f"))

taludes_bien
view(taludes_bien)
summary(taludes_bien)
glimpse(taludes_bien)

taludes_bien <- taludes_bien |>    
  rename(biomasa = Biomasa, # nombre nuevo = nombre viejo          
         nivel_luz = Luz, 
         agua_estival = Agua) 
taludes_bien



macrobenthos <- read_delim(file="Macrobenthos.txt", delim="\t")
macrobenthos
view(macrobenthos)

#Crea un subconjunto de datos que contiene las filas de la 1 a la 10 y de la 390 a la 400.
macro2 |> macrobenthos |> slice (c(1:10,390:400)) # filtrar por filas que cumplen un patrón 

#Crea un subconjunto que NO contiene el Taxón número 1. Pista: revisa los operadores de R .
macrobenthos |> filter (TaxonID>1)

#Crea un subconjunto con las observaciones del Taxón 2 donde se haya registrado una abundancia mayor de 50 o menor o igual a 5. Pista: necesitarás paréntesis para filtrar.
macro3 <- macrobenthos |> filter (TaxonID==2  & (Abundance > 50 | Abundance <=5))
macro3
#¿Cuántas filas han quedado?

#Con el data.frame “Macrobenthos.txt”, crea un nuevo data.frame que contiene las variables relacionadas con el medio (de materia orgánica y temperatura ). Pista: mira la ayuda de seleccionar para ahorrar caracteres.
macro_ambiente <- macrobenthos |>
  select(temperature: o_matter)

#Crea un nuevo objeto con el taxón al principio y que incluye las demás columnas excepto el esfuerzo de muestreo .
macro_taxon <- macrobenthos |> select(TaxonID, everything(), -effort) # , - para quitar una columna
view(macro_taxon)

#Con el data.frame macrobenthos genera una nueva columna con la relación entre la turbidez del agua y la materia orgánica.
macro_newcol <- macrobenthos |> 
  mutate(tur.mo= turbidity/o_matter)
view(macro_newcol)

#Con el macrobentos data.frame, cuenta el número de casos que hay en cada período de muestreo.
macrobenthos |> 
  group_by(period) |>  
  summarise ( cont = n())

#Cuenta el número de casos distintos que hay de esfuerzo de muestreo.
macrobenthos |> 
  summarise ( ndif = n_distinct(effort))

#Calcula la media de la turbidez para cada taxón.

macrobenthos |> 
  group_by(TaxonID) |>  
  summarise ( m.turbidity= mean(turbidity, na.rm=TRUE))
# fun + tab me arroga ya el formato de la funcion:name <- function(variables) {
funcion1 <- function(x) {x/100
}

funcion1(c(100:400,700:1000)) #funcion aplicada a un vector 


