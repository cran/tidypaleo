## ----setup, include = FALSE---------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(tidypaleo)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 3,
  fig.width = 5,
  dpi = 150
)

## ----eval=FALSE---------------------------------------------------------------
# library(tidyverse)
# library(tidypaleo)

## -----------------------------------------------------------------------------
alta_lake_geochem

## -----------------------------------------------------------------------------
alta_nested <- nested_data(
  alta_lake_geochem,
  qualifiers = c(age, depth, zone),
  key = param,
  value = value,
  trans = scale
)

alta_nested

## -----------------------------------------------------------------------------
alta_nested %>% unnested_data(data)
alta_nested %>% unnested_data(qualifiers, data)

## -----------------------------------------------------------------------------
pca <- alta_nested %>% nested_prcomp()
pca

## -----------------------------------------------------------------------------
plot(pca)
pca %>% unnested_data(qualifiers, scores)
pca %>% unnested_data(variance)
pca %>% unnested_data(loadings)

## -----------------------------------------------------------------------------
keji_nested <- keji_lakes_plottable %>%
  group_by(location) %>%
  nested_data(qualifiers = depth, key = taxon, value = rel_abund)

keji_nested %>% unnested_data(qualifiers, data)

## -----------------------------------------------------------------------------
coniss <- keji_nested %>% 
  nested_chclust_coniss()

plot(coniss, main = location)

## -----------------------------------------------------------------------------
plot(coniss, main = location, xvar = qualifiers$depth, labels = "")

## -----------------------------------------------------------------------------
coniss %>% select(location, zone_info) %>% unnest(zone_info)

## -----------------------------------------------------------------------------
keji_nested %>%
  nested_chclust_coniss(n_groups = c(3, 2)) %>%
  select(location, zone_info) %>% 
  unnested_data(zone_info)

## -----------------------------------------------------------------------------
halifax_nested <- halifax_lakes_plottable %>%
  nested_data(c(location, sample_type), taxon, rel_abund, fill = 0)

halifax_nested %>% unnested_data(qualifiers, data)

## -----------------------------------------------------------------------------
hclust <- halifax_nested %>%
  nested_hclust(method = "average")

plot(
  hclust, 
  labels = sprintf(
    "%s (%s)",
    qualifiers$location,
    qualifiers$sample_type
  )
)

## -----------------------------------------------------------------------------
alta_nested %>%
  nested_analysis(vegan::rda, data) %>%
  plot()

## -----------------------------------------------------------------------------
biplot(pca)

