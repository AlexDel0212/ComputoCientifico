require(pacman)
p_load(haven,dplyr, factoextra, FactoMineR, readr, rgl, fpc, psych, readxl)

base <- read.csv('data_pca.csv', sep = ';', dec = ',')
pob_usa <-read_excel('PoblacionUSA.xlsx')

###### DATA PCA ==============
#Normalizar datos
base <- base[,-16]
base1 <- scale(base)

# Realizar pca

# Diagnostico para el PCA
det(cor(base1))
# Es posible hacerlo porque el determinante es cercano a 0
#El determinante de la matriz de correlación es muy bajo, por lo que el grado de asociación es muy alto.

# Usar la función PCA
pca1 <- PCA(base1, graph = F)
summary(pca1)


# hastal el momento se observa que los principales componentes que aportan mayor
# varianza son 1 del 1 al 6 ya que tienen una varianza acumulada mayor al 70%.

# revisar la varianza y eigenvalores
fviz_eig(pca1, choice = 'variance')
# Efectivamente los 6 primeros componentes aportan más varianzas
fviz_eig(pca1,choice = 'eigenvalue')
# solo 6 componentes tienen un eigenvalor mayor a la unidad
# lo adecuado es extraer unicamente seis factores


# Gráfico de las cargas
fviz_pca_var(pca1, col.var = 'contrib', gradient.cols =c('red', 'yellow','green'),
             repel = T)
# En lo anterior se observa que la variable x15 es la más reprensentativa para la dimensión1



#Resultado del pca por factores: 
#VARIMAX
pca12 <- psych::principal(base1,nfactors = 6, residuals = FALSE, rotate = "varimax", 
                         scores =TRUE, oblique.scores=FALSE, method="regression", use= "pairwise", 
                         cor="cor", weight=NULL)

#Matriz de coeficientes para las puntuaciones de los componentes. 
pca12$weights[,1]
pca12$weights[,2]
pca12$weights[,3]
pca12$weights[,4]
pca12$weights[,5]
pca12$weights[,6]

# Los scores son los siguientes
pca12$scores
# Podemos notar que 

##### Poblacion USA =========
# Pob 2020 
pob2020 <- pob_usa[,c(2,3,5,7,9,11,13,15,17,19)]

#Normalizar datos
data1 <- scale(pob2020)


# Realizar pca

# Diagnostico para el PCA
det(cor(data1))
# Es posible hacerlo porque el determinante es cercano a 0
# El determinante de la matriz de correlación es muy bajo, por lo que el grado de asociación 
# es muy alto.


# Calcular factor de adecuación muestral de Kaiser-Meyer-Olkin
psych::KMO(data1)
# Como es de 0.5, podemos seguir continuando, pero no es seguro 

#Todas las variables poseen una msa igual a 0.5, por lo que es pertinente el PCA

pca2 <- PCA(data1,graph = F)
summary(pca2)

# hastal el momento se observa que los principales componentes que aportan mayor
# varianza son 1 y 2, pero más el primero

# revisar la varianza y eigenvalores
fviz_eig(pca2, choice = 'variance')
# Efectivamente el componente uno y dos aportan más varianzas
fviz_eig(pca2,choice = 'eigenvalue')
# solo dos componentes tienen un eigenvalor mayor a la unidad
# lo adecuado es extraer unicamente dos factores


# Análisis gráfico
# grafico de las puntuaciones factoriales y su representacion 
fviz_pca_ind(pca2, col.ind='cos2', gradient.cols =c('red', 'yellow','green'),
             repel = T)

# Las observaciones que menos puntiación tienen son 21, 43, 26, 48

# Gráfico de las cargas
fviz_pca_var(pca2, col.var = 'contrib', gradient.cols =c('red', 'yellow','green'),
             repel = T)
# En lo anterior se ve que solo hay dos variables que explican más la dimensión 2.


psych::cor.plot(data1)
# Casi todas las variables tienen una alta correlación
# Y como vimos antes, su determinante es cercano a 0.

#Resultado del pca por factores: 
#la rotación más omún es VARIMAX
#En un PCA, los componentes principales iniciales pueden ser díficiles de interpretar porque cada variable puede tener cargas
#de manera que cada variable tenga una carga alta en un solo componente, haciendo que la estructura sea más simple y cara. 
pca22 <- psych::principal(data1,nfactors = 2, residuals = FALSE, rotate = "varimax", 
                         scores =TRUE, oblique.scores=FALSE, method="regression", 
                         use= "pairwise", cor="cor", weight=NULL)

#Matriz de coeficientes para las puntuaciones de los componentes. 
pca22$weights[,1]
pca22$weights[,2]

#Nuevas variables obtenidas cuya principal caracteristica es que son ortogonales, es decir, linealmente independientes.
pca22$scores


## Pob 2021
pob2021 <- pob_usa[,c(4,6,8,10,12,14,16,18,20)]

#Normalizar datos
data2 <- scale(pob2021)


# Realizar pca

# Diagnostico para el PCA
det(cor(data2))
# Es posible hacerlo porque el determinante es cercano a 0
# El determinante de la matriz de correlación es muy bajo, por lo que el grado de asociación 
# es muy alto.


# Calcular factor de adecuación muestral de Kaiser-Meyer-Olkin
psych::KMO(data2)
# Como es de 0.5, podemos seguir continuando, pero no es seguro 

#Todas las variables poseen una msa igual a 0.5, por lo que es pertinente el PCA

pca3 <- PCA(data2,graph = F)
summary(pca3)

# hastal el momento se observa que los principales componentes que aportan mayor
# varianza son 1 y 2, pero más el primero

# revisar la varianza y eigenvalores
fviz_eig(pca3, choice = 'variance')
# Efectivamente el componente uno y dos aportan más varianzas
fviz_eig(pca3,choice = 'eigenvalue')
# solo dos componentes tienen un eigenvalor mayor a la unidad
# lo adecuado es extraer unicamente dos factores


# Análisis gráfico
# grafico de las puntuaciones factoriales y su representacion 
fviz_pca_ind(pca3, col.ind='cos2', gradient.cols =c('red', 'yellow','green'),
             repel = T)

# Las observaciones que menos puntiación tienen son 21 y 43

# Gráfico de las cargas
fviz_pca_var(pca3, col.var = 'contrib', gradient.cols =c('red', 'yellow','green'),
             repel = T)
# En lo anterior se ve que solo hay dos variables que explican más la dimensión 2.


psych::cor.plot(data2)
# Casi todas las variables tienen una alta correlación
# Y como vimos antes, su determinante es cercano a 0.

#Resultado del pca por factores: 
#la rotación más omún es VARIMAX
#En un PCA, los componentes principales iniciales pueden ser díficiles de interpretar porque cada variable puede tener cargas
#de manera que cada variable tenga una carga alta en un solo componente, haciendo que la estructura sea más simple y cara. 
pca32 <- psych::principal(data2,nfactors = 2, residuals = FALSE, rotate = "varimax", 
                          scores =TRUE, oblique.scores=FALSE, method="regression", 
                          use= "pairwise", cor="cor", weight=NULL)

#Matriz de coeficientes para las puntuaciones de los componentes. 
pca32$weights[,1]
pca32$weights[,2]

#Nuevas variables obtenidas cuya principal caracteristica es que son ortogonales, es decir, linealmente independientes.
pca32$scores
