################################################################################
# RESUMEN
# En este documento logramos aplicar la matriz del modelo DM, por primera vez
################################################################################
################################################################################


################################################################################
# PAQUETES
################################################################################
library(magrittr)

library(tidyr)
library(dplyr)
library(purrr)
library(tibble)

# Partimos de un tibble con multitud de datos, como mtcars
iris %>% glimpse
# Notemos que ya tenemos el último como factor

## Variable objetivo
data = iris %>%
  mutate(Grande = Sepal.Length > 5.8)

# 1. Conversión del dataset en una matriz cuántica

# 1.1. Cortes con la función cut en cada columna para obtener intervalos
n_breaks = 3
data %<>%
  map_if(is.numeric, cut, breaks = n_breaks) %>%
  as_tibble()

factor_like_cols = data %>% select(where(is.factor)) %>% names()
logical_cols = data %>% select(where(is.logical)) %>% names()

# 1.2. Conversión de cada factor en un valor numérico
data %<>%
  map_if(is.factor, as.numeric) %>%
  as_tibble()

# n_data = dim(data)[1]
# n_var = dim(data)[2]

v_0 = rep(0, n_breaks)
l_0 = rep(0, 2)
FT = c("F", "T")

# 1.3. Para cada variable lógica, convertirla en un vector de dimensión 2 con un 1 en la posición correspondiente
new_data = data %>%
  map_if(is.logical, function(l) {
    l %>%
      map(function(x) {
        l_0 %>% replace(x + 1, 1) %>% set_names(FT)
      })
  }) %>%
  as_tibble()

# 1.4. Conversión de cada vector numérico en una columna de la matriz cuántica con 1s y 0s, recortando previamente
new_data %<>%
  map_if(is.numeric, function(v) {
    v %>%
      map(function(x) {
        v_0 %>% replace(x, 1) %>% set_names(1:n_breaks)
      })
  }) %>%
  as_tibble()

# 1.2. Generación de una matriz cuántica con 1s y 0s, variable objetivo incluida
for (f in c(factor_like_cols, logical_cols)) {
  # f = factor_like_cols[1]
  new_data %<>%
    unnest_wider(starts_with(f), names_sep = "_")
}

data = new_data
original_data = data
N = dim(data)[1]

data %>% glimpse

e_y = data %>% select(Grande_T, Grande_F)
# data %<>% select(-Grande_T, -Grande_F)

# 2. Separamos los datos en los conjuntos según la variable objetivo
# obj = "Grande"
# obj_names = paste0(obj, "_", FT)
C_1 = data %>% filter(Grande_T == 1) %>% select(-starts_with("Grande")) # Para la clase 1
C_2 = data %>% filter(Grande_F == 1) %>% select(-starts_with("Grande")) # Para la clase 2

new_data = bind_rows(C_1[1:2, ], C_2[1:2, ])
C_1 %<>% magrittr::extract(-c(1:2), )
C_2 %<>% magrittr::extract(-c(1:2), )

C_1 %<>% colSums() %>% as.matrix()
C_2 %<>% colSums() %>% as.matrix()

D = C_1 %>% bind_cols(C_2) %>% as.matrix()
X = sqrt(D)

## 3. Matrices m2, 2x2, y rho_d, NxN
# N = dim(data)[1]
m2 = t(X) %*% X / N # Matriz de densidad cuántica (no clásica)
# rho_d = X %*% t(X) / N # Matriz de variación o de respuesta
xxp = X %*% t(X)
my_trace = function(mat) {
  mat %>% diag() %>% sum()
}
rho_d = xxp / my_trace(xxp) # Matriz de variación o de respuesta

# This matrix is called a density matrix and it is a measure of quantum probability (also called non-classical probability).
# That's because it has the following properties:
# 1. It is a positive semi-definite matrix.
all(eigen(rho_d)$values >= 0 | eigen(rho_d)$values < 1e-15) # Eigen devuelve los autovalores de la matriz
# 2. It has trace 1.
my_trace(rho_d) == 1
# 3. It is Hermitian (symmetric in this case).
all(rho_d == t(rho_d))

# Obtenemos el rango de rho_d
lambda = rho_d %>% eigen() %>% .$values %>% sort() %>% round(2)
(rank_rho = sum(lambda != 0))
r = rank_rho
# Como obtenemos solo 2 autovalores distintos de 0, el rango de rho_d es 2

# Ambas matrices de arriba contienen la misma información teóricamente, pues tienen rango 2 y provienen de la misma

## 4. SVD con rho_d
svd_rho_d = svd(rho_d) # rho_d = U D V', con D los autovalores y en U y V' los autovectores
n_nonzero = svd_rho_d$d %>% round(2) %>% equals(0) %>% not %>% sum
n_nonzero == rank_rho # BIEN

U = svd_rho_d$u[, 1:rank_rho] # Autovectores de rho_d

## 2.3. Constructing a surrogate training set from quantum probabilities
X_tilde = original_data %>% select(-Grande_T, -Grande_F) %>% as.matrix() %>% t()
X_tilde[1:4, 1:4]

# 2.3.1. Normalización de cada vector de X tilde
q = X_tilde %*% t(X_tilde) %>% diag

length(q) == dim(X_tilde)[1] # BIEN

## CUIDADO: ¿Estará haciendo correctamente la división?
X_tilde = X_tilde / sqrt(q) # Pertenece a R^d, con d = 15 en este caso

X_tilde %>% dim
t(U) %>% dim

# X_tilde_r es la proyección sobre U, que siendo un conjunto ortonormal:
X_tilde_r = t(U) %*% X_tilde # Pertenece a R^r x R^N, con r = 2 en este caso
# No podemos multiplicar por U directamente debido a que necesitaríamos realmente una copia de U para cada vector por el que multiplicásemos. Como eso es máximamente ineficiente, optamos por simplemente pasar a la base generada por U, que es más o menos lo que se hacía originalmente. #Trivial

X_tilde_r %>% dim
# Tiene sentido que tenga solo 2 filas, pues son las coordenadas respecto de U

# X_tilde_r_e_y
# Buscamos generar el conjunto D_tilde, formado por los productos de X_tilde_r por e_y'
# Recuperamos ahora el siguiente objeto:
(e_y %<>% as.matrix %>% t()) # AGH

X_tilde_r[1:r, 1:4]; e_y[1:r, 1:4]
# En cada columna de X_tilde_r tenemos la codificación de un vector de X_tilde en la base generada por U, la representación como lambda_1, lambda_2
# En cada columna de e_y tenemos la respuesta del vector correspondiente de X_tilde
n_data = dim(X_tilde_r)[2]

# Ahora simplemente convertimos en tibble

X_tilde_r_tib = X_tilde_r %>% t %>% set_colnames(paste0("X_tilde_r_", 1:2)) %>% as_tibble
e_y_tib = e_y %>% t %>% set_colnames(paste0("e_y_", 1:2)) %>% as_tibble
# X_tilde_r_e_y = X_tilde_r %*% t(e_y) # No es tan inmediato, pues busco un producto bloque a bloque
# Queremos, para cada columna de X_tilde_r, multiplicarla por la correspondiente columna de e_y, pero obteniendo bloques:

# Solo para generalizar, pues podría haber más clases
e_y_tib$e_y_3 = 0

D_tilde = X_tilde_r_tib %>%
  bind_cols(e_y_tib) %>%
  unite(e_y, starts_with("e_y"), sep = "") %>%
  nest(.by = e_y) %>%
  tibble::deframe()

str(D_tilde) # Lista con todas las representaciones X_tilde_r para cada clase
typeof(D_tilde)

D_tilde$`010` # Esto representaría la clase 010


## Coordenadas esféricas en R^r
# Pasamos ahora cada vector de D_tilde a coordenadas esféricas
D_tilde[[1]] %>% dim

D_tilde_polar <- D_tilde %>% map(
  ~ .x %>%
    mutate(
      r = sqrt(X_tilde_r_1^2 + X_tilde_r_2^2),
      phi_1 = atan(X_tilde_r_2 / X_tilde_r_1)
    ) %>%
    select(r, phi_1)
)

D_tilde_polar # Mismo espíritu q D_tilde pero en polares

### 2.3.1. A surrogate training set in polar coordinates to be used in a binary classification model
k = 2 # El número de clases
# El resto de pasos ya se han dado

### 2.4. On the empirical conditional density functions for the dependent variables and the classification map
# En D_tilde tenemos en cada índice de la lista un X_tilde_r_e_y, habiéndonos ahorrado así un par de pasos

# Miramos si las distribuciones son diferentes con un test de Kolmogorov-Smirnov multivariante
# ks.test(D_tilde[[1]], D_tilde[[2]]) # No funciona, pues no es univariante
ks.test(D_tilde[[1]] %>% pull(1) %>% unlist, D_tilde[[2]] %>% pull(1) %>% unlist) # Funciona, pues es univariante
ks.test(D_tilde[[1]] %>% pull(2) %>% unlist, D_tilde[[2]] %>% pull(2) %>% unlist) # Funciona, pues es univariante

library(fasano.franceschini.test)
fasano.franceschini.test(D_tilde[[1]], D_tilde[[2]]) # Funciona, pues es multivariante

# Parece que podemos afirmar que las distribuciones son diferentes

# c_r = 1 # El volumen de la esfera unidad
K <- function(..., r = 2) {
  x <- c(...)
  sxx <- sum(x^2)

  if (sxx < 1) {
    return(1/2 * (r + 2) * (1 - sxx))
  } else {
    return(0)
  }
}

# Apply K function using pmap in a parallel way
require(ggplot2)
expand.grid(v_1 = -30:30/30, v_2 = -30:30/30) %>%
  mutate(k_value = pmap_dbl(select(., v_1, v_2), ~ K(..., r = 2))) %>%
  # 2-dimensional graph with ggplot2
  # install.packages("ggplot2")
  ggplot2::ggplot() +
  # ggplot2::geom_point(ggplot2::aes(x = v_1, y = v_2, color = k_value)) +
  ggplot2::geom_raster(ggplot2::aes(x = v_1, y = v_2, fill = k_value)) +
  ggplot2::scale_color_gradient(low = "blue", high = "red") +
  ggplot2::theme_bw() +
  # Mejoramos los valores de leyenda
  ggplot2::labs(
    title = "K function",
    subtitle = "r = 2",
    x = "v_1",
    y = "v_2",
    color = "k_value"
  )

# Vale, tiene pinta de núcleo, okey

#######################################################
#######################################################
#######################################################
######### AQUÍ ESTA EL PROBLEMA
# La siguiente función no hace el cálculo correctamente, a pesar
# de que K se pueda aplicar vectorialmente gracias a pmap_dbl.
# No deja de ser una media, así que no debería suponer mucho problema
# Teóricamente.
#######################################################
#######################################################
#######################################################

f_gorro_h <- function(x, clase, h_window = 0.1) {
  # Para cada valor x esta función debe calcular la probabilidad estimada de
  # pertenencia a la clase "clase", considerando además una ventana h_window.

  # x = c(v_1 = 0.01, v_2 = 0.02)
  # x = D_tilde[[clase]] %>% summarize(mean(X_tilde_r_1), mean(X_tilde_r_2)) %>% unlist() # el promedio para la clase considerada
  # clase = 1; h_window = 0.1
  x_1 <- x[1]; x_2 <- x[2]

  D_tilde[[clase]] %>% # Esto identifica los valores de X_tilde_r recogidos hasta el momento para la clase "clase"
    mutate(
     v_1 = (x_1 - X_tilde_r_1) / h_window, # En este caso x_1 es un ÚNICO valor concreto
     v_2 = (x_2 - X_tilde_r_2) / h_window,
     .keep = "unused"
   ) %>%
   mutate(
     kernel = pmap_dbl(select(., v_1, v_2), K)
   ) %>%
   pull(kernel) %>%
   sum() %>%
   divide_by(n_data * h_window^2)
}

clase = 1
x = D_tilde[[clase]] %>% summarize(mean(X_tilde_r_1), mean(X_tilde_r_2)) %>% unlist()
f_gorro_h(x, clase)
clase = 2
f_gorro_h(x, clase)
## OKEY, FUNCIONA, ahora una prueba extra, al revés
clase = 2
x = D_tilde[[clase]] %>% summarize(mean(X_tilde_r_1), mean(X_tilde_r_2)) %>% unlist()
f_gorro_h(x, clase)
clase = 1
f_gorro_h(x, clase)
# Se porta mejor, chachi

# Ahora, para cada nuevo punto de la bola, denominado x, le asignamos la clase en la que obtenga el valor máximo
new_value = c(0.01, 0.02)
clases = 1:2
new_class = clases %>%
  map_dbl(~
    f_gorro_h(new_value, clase = .x)
  ) %>%
  which.max()

cat("The value", new_value, "is assigned to class", new_class, "\n")



##### NUEVO
# Buscamos ahora, a partir de lo anterior, obtener la clasificación de cada uno de los individuos, para ver cómo de bien se porta, incluso con los datos con los que "se ha entrenado"
# Para ello, vamos a crear una función que nos devuelva la clase a la que pertenece cada individuo
classify = function(x) {
  # x = c(0.01, 0.02)
  clases = 1:2
  new_class = clases %>%
    map_dbl(~
      f_gorro_h(x, clase = .x)
    ) %>%
    which.max()
  return(new_class)
}
classify(c(0.01, 0.02))
# Hecho esto, vamos a aplicar la función a cada uno de los individuos de las dos clases de D_tilde, a ver cuál es la precisión del procedimiento
D_tilde[[1]] %>%
  mutate(
    class = map_dbl(
      .x = pmap_dbl(select(., X_tilde_r_1, X_tilde_r_2), ~ c(...) %>% classify()),
      .f = ~ .x
    )
  ) %>%
  group_by(class) %>%
  summarize(n = n()) %>%
  mutate(
    prop = n / sum(n)
  )

# Y ahora lo mismo con la clase 2
D_tilde[[2]] %>%
  mutate(
    class = map_dbl(
      .x = pmap_dbl(select(., X_tilde_r_1, X_tilde_r_2), ~ c(...) %>% classify()),
      .f = ~ .x
    )
  ) %>%
  group_by(class) %>%
  summarize(n = n()) %>%
  mutate(
    prop = n / sum(n)
  )
# Parece que con la clase 2 la clasificación es completamente perfecta, cosa ligeramente sospechosa

