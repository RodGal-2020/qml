#' @title Quantum Matrix Method for Classification
#'
#' @description This function runs the Falcó algorithm for a given dataset. In the `falco.R` file we can find the kernel function, `K`; the `f_gorro_h` function, for the estimation of the probability of belonging to a class; and the `falco` function, which is the main function that runs the Falcó algorithm.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param my_param Yep, it's a parameter.
#'
#' @return It returns...
#'
#' @examples
#' print("TODO:")
#'
#' @export
testify <- function(Data, objective_variable, test_prob) {
  # For a given tibble with an objective variable, create a column named "test" with a given probability, considering a stratification over the objective variable
  # Data = iris %>% group_by(Species); objective_variable = "Species"; test_prob = 0.25

  Data %<>%
    group_by(!!sym(objective_variable)) %>%
    # Reorder in a random manner
    slice_sample(prop = 1) %>%
    # iris %>% group_by(Species) %>% slice(2) %>% mutate(rn = row_number(), mn = n()) # Una prueba auxiliar
    mutate(test = ifelse(row_number() < test_prob * n(), TRUE, FALSE)) %>%
    ungroup()

  # Data %>% select(test, Species) %>% table

  return(Data)
}

K <- function(..., r = 2) {
  x <- c(...)
  sxx <- sum(x^2)

  if (sxx < 1) {
    return(1/2 * (r + 2) * (1 - sxx))
  } else {
    return(0)
  }
}

f_gorro_h <- function(D_tilde, x, clase, h_window = 0.1) {
  # La D_tilde es la del entrenamiento

  # Para cada vector x esta función calcula la probabilidad estimada de pertenencia a la clase "clase", considerando además una ventana h_window.
  # $\hat{f}_h(x|e_y) = \sum_{\tilde{X} \in \tilde{\chi}_{r, e_y}} \frac{1}{nh^r} K(\frac{x - \tilde{X}_r}{h})$

  # x = c(v_1 = 0.01, v_2 = 0.02)
  # x = D_tilde[[clase]] %>% summarize(mean(X_tilde_r_1), mean(X_tilde_r_2)) %>% unlist() # el promedio para la clase considerada
  # clase = 1; h_window = 0.1
  x_1 <- x[1]; x_2 <- x[2]
  n_Data <- nrow(D_tilde %>% bind_rows()) # n es el tamaño muestral, confirmado por Falcó

  D_tilde[[clase]] %>% # Esto identifica los valores de X_tilde_r recogidos hasta el momento para la clase "clase"
    mutate(
      v_1 = (x_1 - r) / h_window, # En este caso x_1 es un ÚNICO valor concreto
      v_2 = (x_2 - phi_1) / h_window,
      .keep = "unused"
    ) %>%
    mutate(
      kernel = pmap_dbl(select(., v_1, v_2), K)
    ) %>%
    pull(kernel) %>%
    sum() %>%
    divide_by(n_Data * h_window^2)
}

get_rho_d = function(Data, objective_var = NULL, n_breaks = 3, verbose = 0, test_var = "test") {
  # Examples
  # Data = Data_list[[1]]; objective_var = "y"; n_breaks = 3; verbose = 3; test_var = "test"

  Data %<>% as_tibble()
  # Data[[objective_var]] %<>% as.logical() # No necesariamente, pues DEBE SER un factor
  Data[[objective_var]] %<>% as.factor()

  # 1.1. Cortes con la función cut en cada columna numérica para obtener intervalos, convirtiéndolos en factores
  Data %<>% map_if(is.numeric, cut, breaks = n_breaks) %>% as_tibble()

  # Definimos los siguientes como auxiliares, para no tener que repetirlos
  v_0 = rep(0, n_breaks) # Para las variables numéricas
  l_0 = rep(0, 2) # Para las variables lógicas
  # Para los factores habrá que hacerlo manualmente
  FT = c("F", "T")

  # 1.3. Para cada variable lógica, convertirla en un vector de dimensión 2 con un 1 en la posición correspondiente
  # SIN CONTAR LA VARIABLE TEST
  test = Data[test_var]
  new_Data = Data %>%
    select(-all_of(test_var)) %>%
    map_if(is.logical, function(l) {
      l %>%
        map(function(x) {
          l_0 %>% replace(x + 1, 1) %>% set_names(FT)
        })
    }) %>%
    as_tibble() %>%
    bind_cols(test) %>%
    as_tibble()

  # 1.4. Conversión de cada vector numérico en una columna de la matriz cuántica con 1s y 0s, recortando previamente
  # NOTA: OMITIMOS la variable objetivo, que al haber sido un factor sería transformada en este paso
  y = new_Data %>% select(all_of(objective_var), test)
  new_Data %<>%
  # new_Data %>%
  # select(6, y) %>%
    select(-all_of(objective_var), -test) %>%
    map_if(is.factor, function(v) { # Ya no contamos con variables numéricas, sino que tenemos solo factores
      l_v = levels(v) %>% length
      v %>% map(function(x) {
        # v_0 %>% replace(x, 1) %>% set_names(1:n_breaks) # Los nombres son un añadido para luego usar unnest_wider
        # Tenemos que hacer como lo anterior pero sabiendo que puede haber varios niveles
        rep(0, l_v) %>% replace(x, 1) %>% set_names(levels(v))
      })
    }) %>%
    as_tibble() %>% # Lo hacemos debido a que map_if devuelve una lista
    bind_cols(y)

  # 1.2. Conversión de cada factor en un valor numérico, INCLUSO la variable objetivo
  # NOTA: Importante hacerlo después de lo anterior para evitar cortes raros
  # Data %<>% map_if(is.factor, as.numeric) %>% as_tibble()

  # 1.4' Conversión del vector de salida factor->numérico en un vector de dimensión apropiada con un 1 en la posición correspondiente
  # Se pone esto aparte al no tener necesariamente el mismo número de clases que n_breaks
  n_y_classes = new_Data[[objective_var]] %>% unique %>% length
  # Ahora repetimos lo mismo que antes, pero para un vector de ceros de un tamaño apropiado
  v_0_aux = rep(0, n_y_classes)
  new_Data[[objective_var]] %<>%
    map(function(x) {
      v_0_aux %>% replace(x, 1) %>% set_names(1:n_y_classes)
    })

  # 1.2. Generación de una matriz cuántica con 1s y 0s, variable objetivo incluida
  new_Data %<>%
    select(-all_of(test_var)) %>%
    unnest_wider(everything(), names_sep = "_") %>%
    bind_cols(test)

  original_Data = Data
  Data = new_Data
  N = dim(Data)[1]

  if (verbose > 2) {
    Data %>% glimpse
  }

  new_objective = paste0(objective_var, "_", 1:n_y_classes)
  e_y = Data[new_objective]
  # Data %<>% select(-Grande_T, -Grande_F) # Example

  # 2. Separamos los datos en los conjuntos según la variable objetivo
  C <- list()
  for (i in 1:n_y_classes) {
    C[[i]] = Data %>% filter(!!sym(new_objective[i]) == 1) %>% select(-starts_with(objective_var))
  }

  if(nrow(original_Data) != C %>% map(nrow) %>% unlist %>% sum) {
    stop("Error en la separación de los datos")
  }

  # Para el conjunto test podemos tenerlo todo en un único tibble
  test_C = C %>% map(~ .x %>% filter(test == 1) %>% select(-all_of(test_var)))

  train_C = C %>% map(~ .x %>% filter(test == 0) %>% select(-all_of(test_var)))
    # bind_rows() %>%

  # D = (f_1|...|f_k)
  D = train_C %>%
    map(~ .x %>% colSums() %>% as.matrix()) %>%
    bind_cols() %>%
    as.matrix()

  X = sqrt(D) # Elemento a elemento

  ## 3. Matrices m2, 2x2, y rho_d, NxN
  # N = dim(Data)[1]
  m2 = t(X) %*% X / N # Matriz de densidad cuántica (no clásica)
  # rho_d = X %*% t(X) / N # Matriz de variación o de respuesta
  xxp = X %*% t(X)
  my_trace = function(mat) {
    mat %>% diag() %>% sum()
  }
  rho_d = xxp / my_trace(xxp) # Matriz de variación o de respuesta

  # This matrix is called a density matrix and it is a measure of quantum probability (also called non-classical probability).
  # That's because it has the following properties:
  if (!(
    # 1. It is a positive semi-definite matrix.
    all(eigen(rho_d)$values >= 0 | eigen(rho_d)$values < 1e-15) & # Eigen devuelve los autovalores de la matriz
    # 2. It has trace 1.
    my_trace(rho_d) == 1 &
    # 3. It is Hermitian (symmetric in this case).
    all(rho_d == t(rho_d))
  )) {
    warning("La matriz rho_d no cumple con las propiedades de una matriz de densidad cuántica")
  }

  return(list(rho_d = rho_d, train_C = train_C, test_C = test_C))
}

get_C_tilde_polar = function(Data, objective_var = NULL, n_breaks = 3, verbose = 0, test_var = "test", zero_limit = 4) {
  # Example
  # Data = Data_list[[1]]; n_breaks = 3; objective_var = "y"; verbose = 3; test_var = "test"; zero_limit = 4
  rho_d_info <- get_rho_d(Data, objective_var, n_breaks = 3, verbose = 0, test_var = test_var)

  rho_d = rho_d_info$rho_d
  train_C = rho_d_info$train_C
  test_C = rho_d_info$test_C

  # Obtenemos el rango de rho_d
  lambda = rho_d %>% eigen() %>% .$values %>% sort() %>% round(zero_limit)
  rank_rho = sum(lambda != 0)

  # Ambas matrices de arriba contienen la misma información teóricamente, pues tienen rango 2 y provienen de la misma matriz original

  ## 4. SVD con rho_d
  # rho_d = U S U', con S matriz con diagonal de autovalores y en U los autovectores
  svd_rho_d = svd(rho_d) # rho_d = U D V', con D los autovalores y en U y V' los autovectores
  n_nonzero = svd_rho_d$d %>% round(zero_limit) %>% equals(0) %>% not %>% sum
  if (n_nonzero != rank_rho) {
    stop("El rango de rho_d no coincide con el número de autovalores no nulos")
  }

  U = svd_rho_d$u[, 1:rank_rho] # Los r autovectores de rho_d, por columnas

  if (verbose > 2) {
    cat("U:\n")
    U %>% round(2) %>% print
  }

  ## 2.3. Constructing a surrogate training set from quantum probabilities
  # X_tilde = original_Data %>% select(-Grande_T, -Grande_F) %>% as.matrix() %>% t() # Original
  get_X_tilde = function(X) {
    # X es una fila
    # X = runif(100) %>% matrix(nrow = 1)
    # X = train_C[[1]][1,]
    X_tilde = X %>% as.matrix
    q = X_tilde %*% t(X_tilde) # Transpuesta al trabajar con una fila
    # Esto último simplemente cuenta el número de entradas no nulas en X_tilde
    q %<>% diag %>% magrittr::extract(1)

    if(abs(q) < 1e-10) {
      warning("q is close to zero, returning original X_tilde")
      return(X_tilde)
    }

    X_tilde = X_tilde / sqrt(q)
    return(X_tilde)
  }

  get_U_coords = function(X_tilde, U) {
    # U es una matriz con los autovectores de rho_d como columnas
    # X_tilde es una fila
    return(X_tilde %*% U)
  }

  # 2.3.1. Normalización de cada vector de X tilde (digamos, train_C en mi caso)

  # Map to EACH ROW in each train_C the get_X_tilde and then the get_U_coords:
  # train_C[[1]][1,] %>% get_X_tilde %>% get_U_coords(U = U) # Ejemplo de uso
  # train_C[[1]] %>% apply(get_X_tilde, MARGIN = 1) # Debería funcionar pero no lo hace, cosa que se puede identificar al ver el número de unos
  train_C_tilde <- train_C %>% map(
   ~ {
     xp <- matrix(NA, nrow = 0, ncol = rank_rho)
     for (r in 1:nrow(.x)) {
       xp %<>% rbind(
         .x[r, ] %>% get_X_tilde() %>% get_U_coords(U = U)
       )
     }
     xp
   }
 )

  if (verbose > 2) {
    cat("train_C_tilde:\n")
    train_C_tilde # Mismo espíritu q D_tilde pero en polares
  }

  test_C_tilde <- test_C %>% map(
   ~ {
     xp <- matrix(NA, nrow = 0, ncol = rank_rho)
     for (r in 1:nrow(.x)) {
       xp %<>% rbind(
         .x[r, ] %>% get_X_tilde() %>% get_U_coords(U = U)
       )
     }
     xp
   }
 )

  # X_tilde_r_e_y = \hat{X}_{r, e_y} = (theta|...|X_tilde_r|...|theta)
  # Como podemos ver, realmente esta información ya la tenemos guardada en el objeto train_C_tilde,
  # así que podemos ahorrarnos algunos pasos e ir directamente al grano

  if (rank_rho > 2) {
    stop("El rango de rho_d es mayor a 2, y actualmente admitimos clasificación binaria únicamente")
  }

  ## Coordenadas esféricas en R^r
  # Pasamos ahora cada vector de D_tilde a coordenadas esféricas
  train_C_tilde_polar <- train_C_tilde %>% map(
    ~ .x %>%
      as_tibble() %>%
      set_names(paste0("V_", 1:ncol(.))) %>% # Esta V realmente representa X_tilde_r
      mutate(
        r = sqrt(V_1^2 + V_2^2),
        phi_1 = atan(V_2 / V_1)
      ) %>%
      select(r, phi_1)
  )

  if (verbose > 2) {
    cat("train_C_tilde_polar:\n")
    train_C_tilde_polar
  }

  test_C_tilde_polar <- test_C_tilde %>% map(
    ~ .x %>%
      as_tibble() %>%
      set_names(paste0("V_", 1:ncol(.))) %>% # Esta V realmente representa X_tilde_r
      mutate(
        r = sqrt(V_1^2 + V_2^2),
        phi_1 = atan(V_2 / V_1)
      ) %>%
      select(r, phi_1)
  )

  ###### CASO DE CLASIFICACIÓN BINARIA

  ### 2.4. On the empirical conditional density functions for the dependent variables and the classification map
  # En train_C_tilde_polar tenemos en cada índice de la lista un X_tilde_r_e_y, habiéndonos ahorrado así un par de pasos

  # Miramos si las distribuciones son diferentes con un test de Kolmogorov-Smirnov multivariante
  # ks.test(D_tilde[[1]], D_tilde[[2]]) # No funciona, pues no es univariante
  # ks.test_1 = ks.test(train_C_tilde_polar[[1]] %>% pull(1) %>% unlist, train_C_tilde_polar[[2]] %>% pull(1) %>% unlist) # Funciona, pues es univariante
  # ks.test_2 = ks.test(train_C_tilde_polar[[1]] %>% pull(2) %>% unlist, train_C_tilde_polar[[2]] %>% pull(2) %>% unlist) # Funciona, pues es univariante
  #
  # if (!(ks.test_1$p.value < 0.05 | ks.test_2$p.value < 0.05)) {
  #   warning("Las distribuciones son iguales. Posiblemente nada funcione\n")
  # }

  ff.test = fasano.franceschini.test::fasano.franceschini.test(train_C_tilde_polar[[1]], train_C_tilde_polar[[2]]) # Funciona, pues es multivariante
  if (!(ff.test$p.value < 0.05)) {
    warning("No hay evidencia significativa de diferencia entre las distribuciones. Posiblemente nada funcione\n")
  } else {
    return(list(train = train_C_tilde_polar, test = test_C_tilde_polar))
  }
}

qmat = function(Data, n_breaks = 3, objective_var = "y", verbose = 0, test_var = "test", debug = FALSE) {
  # Data = Data_list[[1]]; n_breaks = 3; objective_var = "y"; verbose = 3; test_var = "test"
  C_tilde_polar <- get_C_tilde_polar(Data, objective_var = objective_var, n_breaks = n_breaks, verbose = verbose, test_var = "test")

  D_tilde_train <- C_tilde_polar$train # Una entrada por clase
  D_tilde_test <- C_tilde_polar$test # Una entrada por clase

  classify = function(x, D_tilde) {
    # x = c(0.01, 0.02)
    clases = 1:2 # Solo admitimos clasificación binaria actualmente
    new_class = clases %>%
      map_dbl(~ f_gorro_h(D_tilde, x, clase = .x)
      ) %>%
      which.max() # Hacemos la estimación basada en verosimilitud
    return(new_class)
  }

  return(
    D_tilde_test %>% imap(
    ~ .x %>% mutate(
      .pred = map_dbl(
        .x = pmap_dbl(select(., r, phi_1), ~ c(...) %>% classify(D_tilde = D_tilde_train)),
        .f = ~ .x
      ),
      .truth = .y
    ) %>%
      mutate(across(c(.pred, .truth), as.factor))
  ) %>%
    bind_rows()
  )
}

