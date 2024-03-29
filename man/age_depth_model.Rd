% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/age_depth_model.R
\name{age_depth_model}
\alias{age_depth_model}
\title{Create age depth models}
\usage{
age_depth_model(
  .data = NULL,
  depth,
  age,
  age_min = NA_real_,
  age_max = NA_real_,
  interpolate_age = age_depth_interpolate,
  extrapolate_age_below = ~age_depth_extrapolate(.x, .y, x0 = last, y0 = last),
  extrapolate_age_above = ~age_depth_extrapolate(.x, .y, x0 = first, y0 = first),
  interpolate_age_limits = age_depth_exact,
  extrapolate_age_limits_below = age_depth_na,
  extrapolate_age_limits_above = age_depth_na
)
}
\arguments{
\item{.data}{A data frame}

\item{depth, age, age_min, age_max}{Expressions evaluated in \code{.data} that
provide the known depths, known ages, and error information if available.
These expressions are evaluated like they are within \link[dplyr:mutate]{mutate}
if \code{.data} is present.}

\item{interpolate_age, extrapolate_age_below, extrapolate_age_above}{These
arguments provide the rules for interpolating and extrapolating ages based
on depths.}

\item{interpolate_age_limits, extrapolate_age_limits_below, extrapolate_age_limits_above}{These arguments provide the rules for interpolating and extrapolating age
min and max values based on depths.}
}
\value{
An age depth model object.
}
\description{
Create age depth models
}
\examples{
age_depth_model(
  alta_lake_210Pb_ages,
  depth = depth_cm, age = age_year_ad,
  age_max = age_year_ad + age_error_yr,
  age_min = age_year_ad - age_error_yr
)

}
