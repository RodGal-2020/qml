F_0 = Data$design %>%
    group_by(Class) %>%
    summarise(across(everything(), sum))

F_1 = F_0 %>%
  # Map across all columns but Class the sqrt function to each element
  mutate(across(-Class, sqrt)) %>%
  select(-Class) %>%
  as.matrix()

FF = t(F_1) %*% F_1
tr = function (A) sum(diag(A))
rho = FF / tr(FF)

# We generate now the USU decomposition by means of the eigen decomposition of matrix rho
eigen_rho = eigen(rho)
D = eigen_rho$values
U = eigen_rho$vectors
rho_p = U %*% diag(D) %*% t(U)

# Checkup
all.equal(dim(rho), dim(rho_p))
all.equal(rho, rho_p)
rho == rho_p # Not exactly
(rho - rho_p) < 1e-4 # Okey!

# We now generate a basis using the first 2 eigenvectors, therefore computing the new values for the variables in the new space
U_2 = U[, 1:2] # The loadings
# DC-like elements
loadings <- U_2 %>%
  as_tibble() %>%
  set_names(c("DC1", "DC2")) %>%
  mutate(variable = colnames(F_1))
explained_variance <- (D * 100) %>% set_names(c("DC1", "DC2", "DC3", "DC4", "DC5"))

F_2 = F_1 %*% U_2 # We can do this
DMM = Data$design %>% select(-Class) %>% as.matrix()
DMM = DMM %*% U_2 %>% # We can also do this, without losing information
  as_tibble() %>%
  bind_cols(Data$design %>% select(Class)) %>%
  rename(DC1 = V1, DC2 = V2, Class = Class)

# > DMM
# # A tibble: 100 × 3
# DC1    DC2 Class
# <dbl>  <dbl> <fct>
#   1 0.855 0.190  1
# 2 0.958 0.860  1
# 3 0.855 0.190  1

basic_dmm_plot <- DMM %>%
# DMM %>%
  ggplot() +
  # Now we include the loadings:
  # geom_text(data = loadings, aes(x = DC1, y = DC2, label = variable), size = 3, hjust = 0, vjust = 0) +
  ggrepel::geom_text_repel(data = loadings, aes(x = DC1 * max(abs(DMM$DC1)), y = DC2 * max(abs(DMM$DC2)), label = variable), color = "blue", size = 4) +
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = DC1 * max(abs(DMM$DC1)),
                   yend = DC2 * max(abs(DMM$DC2))),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "blue", alpha = 0.8) +
  labs(
    x = paste0("DC1 (", round(explained_variance[1], 1), "% variance)"),
    y = paste0("DC2 (", round(explained_variance[2], 1), "% variance)"),
    title = "DC Biplot",
    subtitle = "Scores and Loadings (2/5 eigenvectors)"
  ) +
  # We want to include a note on the explained variance
  # annotate("text", x = 0, y = -0.4, label = "There were 5 more eigenvectors", hjust = 0, vjust = 0, size = 3, color = "black") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )
basic_dmm_plot

# dmm_plot = basic_dmm_plot +s
dmm_plot = basic_dmm_plot +
  geom_point(aes(x = DC1, y = DC2, color = Class), alpha = 0.7, size = 3)
dmm_plot

# Now with some error for better visualization, with jitter
jit = 0.10; dmm_plot_jit = basic_dmm_plot +
  geom_jitter(aes(x = DC1, y = DC2, color = Class), alpha = 0.7, size = 3, width = jit, height = jit)
dmm_plot_jit






