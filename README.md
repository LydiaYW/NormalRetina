
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NormalRetina

<!-- badges: start -->

[![R-CMD-check](https://github.com/UofUEpiBio/egpkg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/UofUEpiBio/egpkg/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of NormalRetina is to predict normative retina sensitivity,
providing tools for data processing, modeling, prediction and
visualization. It is particularly useful for age-adjusted normative data
for retinal sensitivity.

## Installation

You can install the development version of NormalRetina from
[GitHub](https://github.com/LydiaYW/NormalRetina) with:

``` r
# install.packages("devtools")
devtools::install_github("LydiaYW/NormalRetina")
```

## Features

- **Data Preprocessing** *SensForFit*: A function to clean and prepare
  retinal sensitivity data for analysis, with variables: participant
  age, eye, eccentricity and angular distance from the fovea, and
  spatial coordinates (x,y). Also patient-wise k-fold split for model
  training.
- **Virtual Datasets**: Includes two example datasets representing
  normative retinal sensitivity for reference patients. First is
  *ref77*, a virtrual patient of age 77, for spatial interpolation and
  visualization. The other is *refMes*, a virtrual population with 100
  healthy participants’ Mesopic sensitivity results.
- **Prediction Models**:
  - Support for **Linear Mixed Models (LMM)**, **Bayesian Quantile
    Regression (BQR)**, and **Random Forest (RF)**.
  - Functions to compare model performance and provide age-adjusted
    prediction based on the selected best method based on a population.
- **Spatial Interpolation**: A method for interpolating retinal
  sensitivity for one patient.
- **Visualization**:
  - Functions for creating hill-of-vision map of predicted retina
    sensitivity for different exam types.

------------------------------------------------------------------------

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(NormalRetina)

##----pre processing----
data("refMes")
refMes <- SensForFit(dt = refMes, examcol = "Examtype", idcol = "Patient", agecol = "Age", senscol = "MeanSens", k = 10)

##----model comparison----
PredictCompare(dt = refMes, exam = "Mesopic", CalibSplit = 0.2, coverage = 0.95)
## e.g. LMM is the best model
## prediction for a future patient aged 77
pred_BQR_77 <- PredictSensitivity(model = "BQR", age = 77, dt = refMes)
## hill-of-vision for new patient
VisualRetina(pred_sens = pred_BQR_77, exam = "Mesopic")
```

<img src="man/figures/README-example-1.png" style="width:30.0%"
data-fig-align="left" />

``` r
##----Interpolation----
data("ref77")
interpolated_77 <- Interpolation(dt = ref77)
## hill-of-vision from interpolation 
VisualRetina(pred_sens = interpolated_77[[2]], exam = unique(interpolated_77[[2]]$Examtype)) # plot mesopic results for comparison
```

<img src="man/figures/README-example-2.png" style="width:30.0%"
data-fig-align="left" />
