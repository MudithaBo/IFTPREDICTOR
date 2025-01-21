# IFTPredictor: Item-Focused Tree Model Predictor

## Overview
`IFTPredictor` is an R package for predicting item response probabilities 
and responses using the **item-focused tree (IFT) model**. Patient-reported outcome measures (PROMs) 
are appraisals from patients about their well-being and quality of life. 
Differential item functioning (DIF) is a potential source of measurement bias associated with PROMs data.
DIF occurs when patients with same health status respond or interpret PROM items differently 
due to patient characteristics such as age, sex, or educational background.
The IFT model combines logistic regression with recursive partitioning to detect 
DIF in dichotomous items. The **DIFtree** package in R fits the IFT models, 
but it lacks tools to evalute model performance. We developed an R package, 
to predict item responses by accounting for DIF effects in PROM items.

## Key Features
- Predicts item responses adjusted for Differential Item Functioning (DIF) using the IFT model

## Installation
To install the package from GitHub:
```R
# Install devtools if not already installed
install.packages("devtools")

# Install IFTPredictor
devtools::install_github("MudithaBo/IFTPredictor")

install.packages("DIFtree")

library(DIFtree)
```

The following code examples can be found in the package description.
```r
# Load the package
library(IFTPredictor)

# Load example data
data("mydata", package = "IFTPredictor")
head(mydata)

# Split data into responses (Y) and covariates (X)
Y <- mydata[, 1:20]
X <- mydata[, 21:24]

# Compute total score for each participant
mydata$total_score <- rowSums(Y)

# Fit the DIFtree model
if (requireNamespace("DIFtree", quietly = TRUE)) {
  mod <- DIFtree(Y, X, model = "Logistic", type = "udif", alpha = 0.05, nperm = 100, trace = TRUE)

  # Predict responses using the model
  results <- predict_item_responses(mod, dataset = mydata, total_score = "total_score")
  print(results$predictions)
} else {
  message("The 'DIFtree' package is required to run this example. Please install it.")
}
```
## References

-Berger, Moritz and Tutz, Gerhard (2016): Detection of Uniform and Non-Uniform Differential Item Functioning by Item Focused Trees,
 Journal of Educational and Behavioral Statistics 41(6), 559-592.

## Authors
Muditha Bodawatte Gedara (muditha.lakmali.1993@gmail.com),
Barret Monchka,
Lisa Lix
