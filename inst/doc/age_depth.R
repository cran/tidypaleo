## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 4,
  fig.width = 5,
  dpi = 150
)

## -----------------------------------------------------------------------------
library(tidypaleo)
alta_lake_adm <- age_depth_model(
  alta_lake_210Pb_ages,
  depth = depth_cm, age = age_year_ad,
  age_max = age_year_ad + age_error_yr, 
  age_min = age_year_ad - age_error_yr
)
alta_lake_adm

## ----alta_lake_adm_plot-------------------------------------------------------
plot(alta_lake_adm)

## -----------------------------------------------------------------------------
predict(alta_lake_adm, depth = seq(-1, 10, 0.5))

## ---- eval = FALSE------------------------------------------------------------
#  age_depth_model(
#    ...,
#    interpolate_age = age_depth_interpolate,
#    extrapolate_age_below = ~age_depth_extrapolate(.x, .y, x0 = last, y0 = last),
#    extrapolate_age_above = ~age_depth_extrapolate(.x, .y, x0 = first, y0 = first),
#    interpolate_age_limits = trans_exact,
#    extrapolate_age_limits_below = trans_na,
#    extrapolate_age_limits_above = trans_na
#  )

## -----------------------------------------------------------------------------
alta_lake_adm2 <- age_depth_model(
  alta_lake_210Pb_ages,
  depth = depth_cm, age = age_year_ad,
  age_max = age_year_ad + age_error_yr, 
  age_min = age_year_ad - age_error_yr,
  extrapolate_age_below = ~age_depth_extrapolate(
    tail(.x, 3), tail(.y, 3), x0 = dplyr::last, y0 = dplyr::last
  ),
  extrapolate_age_above = ~age_depth_extrapolate(
    head(.x, 3), head(.y, 3), x0 = dplyr::first, y0 = dplyr::first
  )
)

plot(alta_lake_adm2)

