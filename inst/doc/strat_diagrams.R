## ----setup, include = FALSE---------------------------------------------------
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(tidypaleo)
theme_set(theme_paleo(8))

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 3,
  fig.width = 6,
  dpi = 96
)

## ---- eval=FALSE--------------------------------------------------------------
#  library(tidyverse)
#  library(tidypaleo)
#  theme_set(theme_paleo(8))

## -----------------------------------------------------------------------------
data("long_lake_plottable")
data("alta_lake_geochem")
data("keji_lakes_plottable")
data("halifax_lakes_plottable")

## -----------------------------------------------------------------------------
alta_lake_geochem

## -----------------------------------------------------------------------------
alta_plot <- ggplot(alta_lake_geochem, aes(x = value, y = depth)) +
  geom_lineh() +
  geom_point() +
  scale_y_reverse() +
  facet_geochem_gridh(vars(param)) +
  labs(x = NULL, y = "Depth (cm)")

alta_plot

## -----------------------------------------------------------------------------
alta_plot +
  geom_hline(yintercept = c(4, 16), col = "red", lty = 2, alpha = 0.7)

## -----------------------------------------------------------------------------
zone_data <- tibble(ymin = 4, ymax = 16, xmin = -Inf, xmax = Inf)
alta_plot +
  geom_rect(
    mapping = aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax), 
    data = zone_data, 
    alpha = 0.2,
    fill = "blue",
    inherit.aes = FALSE
  )

## -----------------------------------------------------------------------------
cu_standard_data <- tibble(param = "Cu", xmin = 35.7, xmax = Inf, ymin = -Inf, ymax = Inf)
alta_plot +
  geom_rect(
    mapping = aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax), 
    data = cu_standard_data, 
    alpha = 0.2,
    fill = "red",
    inherit.aes = FALSE
  )

## ---- warning=FALSE-----------------------------------------------------------
alta_plot + 
  geom_errorbarh(aes(xmin = value - stdev, xmax = value + stdev), height = 0.5)

## ---- eval=FALSE--------------------------------------------------------------
#  alta_lake_geochem %>%
#    mutate(param = fct_relevel(param, "Ti", "Cu", "C/N")) %>%
#    ggplot(aes(x = value, y = depth)) +
#    ...

## ---- echo=FALSE--------------------------------------------------------------
alta_lake_geochem %>%
  mutate(param = fct_relevel(param, "Ti", "Cu", "C/N")) %>%
  ggplot(aes(x = value, y = depth)) +
  geom_lineh() +
  geom_point() +
  scale_y_reverse() +
  facet_geochem_gridh(vars(param)) +
  labs(x = NULL, y = "Depth (cm)")

## ---- eval=FALSE--------------------------------------------------------------
#  alta_lake_geochem %>%
#    filter(param %in% c("d15N", "d13C", "C/N")) %>%
#    ggplot(aes(x = value, y = depth)) +
#    ...

## ---- echo=FALSE--------------------------------------------------------------
alta_lake_geochem %>%
  filter(param %in% c("d15N", "d13C", "C/N")) %>%
  ggplot(aes(x = value, y = depth)) +
  geom_lineh() +
  geom_point() +
  scale_y_reverse() +
  facet_geochem_gridh(vars(param)) +
  labs(x = NULL, y = "Depth (cm)")

## ---- message=FALSE-----------------------------------------------------------
alta_adm <- age_depth_model(
  alta_lake_bacon_ages, 
  depth = depth_cm,
  age = 1950 - age_weighted_mean_year_BP
)

alta_plot +
  scale_y_depth_age(
    alta_adm,
    age_name = "Age (Year AD)"
  )

## -----------------------------------------------------------------------------
alta_plot +
  facet_geochem_gridh(
    vars(param),
    units = c("C/N" = NA, "Cu" = "ppm", "d13C" = "‰", "d15N" = "‰"),
    default_units = "%"
  )

## -----------------------------------------------------------------------------
combined_data <- bind_rows(long_lake_plottable, alta_lake_geochem)
combined_data

## ---- fig.height=6------------------------------------------------------------
ggplot(combined_data, aes(x = value, y = depth)) +
  geom_lineh() +
  geom_point() +
  scale_y_reverse() +
  facet_geochem_gridh(vars(param), grouping = vars(location), scales = "free") +
  labs(x = NULL, y = "Depth (cm)")

## ---- fig.height=6------------------------------------------------------------
alta_plot_1 <- combined_data %>% 
  filter(location == "ALGC2") %>% 
  ggplot(aes(x = value, y = depth)) +
  geom_lineh() +
  geom_point() +
  scale_y_reverse() +
  facet_geochem_gridh(vars(param), scales = "free") +
  labs(x = NULL, y = "Depth (cm)", title = "Alta Lake")

long_plot_2 <- combined_data %>% 
  filter(location == "LL PC2") %>% 
  ggplot(aes(x = value, y = depth)) +
  geom_lineh() +
  geom_point() +
  scale_y_reverse() +
  facet_geochem_gridh(vars(param), scales = "free") +
  labs(x = NULL, y = "Depth (cm)", title = "Long Lake")

library(patchwork)
wrap_plots(alta_plot_1, long_plot_2, ncol = 1)

## -----------------------------------------------------------------------------
coniss <- alta_lake_geochem %>%
  nested_data(qualifiers = c(age, depth), key = param, value = value, trans = scale) %>%
  nested_chclust_coniss()

alta_plot +
  layer_dendrogram(coniss, aes(y = depth), param = "CONISS") +
  layer_zone_boundaries(coniss, aes(y = depth))

## -----------------------------------------------------------------------------
alta_plot + 
  facet_geochem_wraph(vars(param), rotate_axis_labels = 0, ncol = 4)

## ---- fig.width=3, fig.height=5-----------------------------------------------
ggplot(alta_lake_geochem, aes(x = age, y = value)) +
  geom_line() +
  geom_point() +
  scale_y_reverse() +
  facet_geochem_grid(vars(param)) +
  labs(x = "Age (Year AD)", y = NULL)

## -----------------------------------------------------------------------------
data("keji_lakes_plottable")
keji_lakes_plottable

## ---- fig.height=6------------------------------------------------------------
keji_plot <- ggplot(keji_lakes_plottable, aes(x = rel_abund, y = depth)) +
  geom_col_segsh() +
  scale_y_reverse() +
  facet_abundanceh(vars(taxon), grouping = vars(location)) +
  labs(x = "Relative abundance (%)", y = "Depth (cm)")

keji_plot

## ---- fig.height=6------------------------------------------------------------
ggplot(keji_lakes_plottable, aes(x = rel_abund, y = depth)) +
  geom_areah() +
  scale_y_reverse() +
  facet_abundanceh(vars(taxon), grouping = vars(location)) +
  labs(x = "Relative abundance (%)", y = "Depth (cm)")

## ---- fig.height=6------------------------------------------------------------
ggplot(keji_lakes_plottable, aes(x = rel_abund, y = depth)) +
  geom_col_segsh() + 
  geom_lineh() +
  scale_y_reverse() +
  facet_abundanceh(vars(taxon), grouping = vars(location)) +
  labs(x = "Relative abundance (%)", y = "Depth (cm)")

## ---- fig.height=6------------------------------------------------------------
keji_plot +
  geom_lineh_exaggerate(exaggerate_x = 5, col = "grey70", lty = 2)

## ---- fig.height=6------------------------------------------------------------
keji_pca_scores <- keji_lakes_plottable %>%
  group_by(location) %>%
  nested_data(qualifiers = depth, key = taxon, value = rel_abund, trans = sqrt) %>%
  nested_prcomp() %>%
  unnest(qualifiers, scores) %>%
  gather(key = component, value = value, starts_with("PC")) %>%
  filter(component %in% c("PC1", "PC2"))

keji_pca_plot <- ggplot(keji_pca_scores, aes(x = value, y = depth)) +
  geom_lineh() +
  geom_point() +
  scale_y_reverse() +
  facet_geochem_gridh(vars(component), grouping = vars(location)) +
  labs(x = NULL)
  
library(patchwork)
wrap_plots(
  keji_plot + 
    theme(strip.background = element_blank(), strip.text.y = element_blank()),
  keji_pca_plot +
    theme(axis.text.y.left = element_blank(), axis.ticks.y.left = element_blank()) +
    labs(y = NULL),
  nrow = 1,
  widths = c(4, 1)
)

## ---- fig.height=6------------------------------------------------------------
keji_coniss <- keji_lakes_plottable %>%
  group_by(location) %>%
  nested_data(qualifiers = depth, key = taxon, value = rel_abund) %>%
  nested_chclust_coniss()

library(patchwork)

# method 1: use existing non-abundance plot
wrap_plots(
  keji_plot + 
    theme(strip.background = element_blank(), strip.text.y = element_blank()),
  keji_pca_plot +
    layer_dendrogram(keji_coniss, component = "CONISS", aes(y = depth)) +
    theme(axis.text.y.left = element_blank(), axis.ticks.y.left = element_blank()) +
    labs(y = NULL),
  nrow = 1,
  widths = c(2, 1)
)

## ---- fig.height=6------------------------------------------------------------
# method 2: create a standalone plot for CONISS
coniss_plot <- ggplot() +
  layer_dendrogram(keji_coniss, aes(y = depth)) +
  scale_y_reverse() +
  facet_geochem_gridh(vars("CONISS"), grouping = vars(location)) +
  labs(x = NULL)

wrap_plots(
  keji_plot + 
    theme(strip.background = element_blank(), strip.text.y = element_blank()),
  coniss_plot +
    theme(axis.text.y.left = element_blank(), axis.ticks.y.left = element_blank()) +
    labs(y = NULL),
  nrow = 1,
  widths = c(6, 1)
)

## -----------------------------------------------------------------------------
data("halifax_lakes_plottable")
halifax_lakes_plottable

## -----------------------------------------------------------------------------
halifax_plot <- ggplot(halifax_lakes_plottable, aes(x = rel_abund, y = location, fill = sample_type)) +
  geom_colh(width = 0.5, position = "dodgev") +
  facet_abundanceh(vars(taxon)) +
  labs(x = "Relative abundance (%)", y = NULL, fill = "Sample Type")

halifax_plot

## ---- eval=FALSE--------------------------------------------------------------
#  halifax_lakes_plottable %>%
#    mutate(location = fct_relevel(location, "Bell Lake", "Bayers", "Little Springfield") %>% fct_rev()) %>%
#    ggplot(aes(x = rel_abund, y = location, fill = sample_type)) +
#    ...

## ---- echo=FALSE--------------------------------------------------------------
halifax_lakes_plottable %>%
  mutate(location = fct_relevel(location, "Bell Lake", "Bayers", "Little Springfield") %>% fct_rev()) %>%
  ggplot(aes(x = rel_abund, y = location, fill = sample_type)) +
  geom_colh(width = 0.5, position = "dodgev") +
  facet_abundanceh(vars(taxon)) +
  labs(x = "Relative abundance (%)", y = NULL, fill = "Sample Type")

## -----------------------------------------------------------------------------
halifax_clust <- halifax_lakes_plottable %>%
  filter(sample_type == "top") %>%
  nested_data(qualifiers = location, key = taxon, value = rel_abund) %>%
  nested_hclust(method = "average")

dendro_order <- halifax_clust %>%
  unnest(qualifiers, dendro_order) %>%
  arrange(dendro_order) %>% 
  pull(location)

library(patchwork)
wrap_plots(
  halifax_plot + 
    scale_y_discrete(limits = dendro_order) +
    theme(legend.position = "left"),
  ggplot() + 
    layer_dendrogram(halifax_clust, aes(y = location)) +
    scale_y_discrete(limits = dendro_order) +
    labs(x = "Dispersion", y = NULL) +
    theme(axis.text.y.left = element_blank(), axis.ticks.y.left = element_blank()),
  widths = c(4, 1)
)

