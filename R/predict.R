#' Predictions Using Item-Focused Tree Models
#'
#' This function predicts item response probabilities and item responses using the item-focused tree (IFT) model.
#' The IFT model combines logistic regression with recursive partitioning to detect Differential Item Functioning (DIF)
#' in dichotomous items. The model applies partitioning rules to the data, splitting it into homogeneous subgroups,
#' and uses logistic regression within each subgroup to explain the data. DIF detection is achieved by examining
#' potential group differences in item response patterns. This model produces tree diagrams to visualize homogeneous
#' subgroups within the population exhibiting similar response patterns and may therefore be helpful for developing personalized interventions and optimizing resource allocation in healthcare.
#'
#' @param model A DIFtree model object.
#' @param dataset A data frame containing the required data. The 'total_score' column must be included.
#' @param total_score The name of the column in the dataset representing the total score (e.g., "total_score").
#'
#' @details
#' The logistic regression model for the \eqn{i}-th PROM item is defined as:
#'
#' \deqn{
#' \log \left( \frac{P(Y_{pi} = 1 \mid S_{p}, g)}{P(Y_{pi} = 0 \mid S_{p}, g)} \right) = \eta_{pi} = \beta_{0i} + S_{p} \beta_{i} + \gamma_{ig},
#' }{
#' log(P(Y[pi] = 1 | S[p], g) / P(Y[pi] = 0 | S[p], g)) = eta[pi] = beta[0i] + S[p] beta[i] + gamma[ig],
#' }
#'
#' where,
#' \eqn{Y_{pi} \in \{0, 1\}}: The response of person \eqn{p} to the \eqn{i}-th item.
#' \eqn{p = 1, 2, \dots, P}: The number of persons.
#' \eqn{i = 1, 2, \dots, I}: The number of items.
#' \eqn{g}: Group membership (\eqn{g = 0} for the reference group, \eqn{g = 1} for the focal group).
#' \eqn{S_p}: The ability level (e.g., total PROM score) of person \eqn{p}.
#' \eqn{\beta_{0i}}: The intercept or item difficulty parameter.
#' \eqn{\beta_{i}}: The slope or item discrimination parameter.
#' \eqn{\gamma_{ig}}: The group-specific parameter.
#'
#' The IFT model extends this logistic regression model for DIF detection for the \eqn{i}-th PROM item:
#'
#' \deqn{
#' \eta_{pi} = \beta_i S_p + \left[ \gamma_{ik} I(x_{pl} \leq c_l) + \gamma_{ir} I(x_{pl} > c_l) \right],
#' }{
#' eta[pi] = beta[i] S[p] + [gamma[ik] I(x[pl] <= c[l]) + gamma[ir] I(x[pl] > c[l])],
#' }
#'
#' where,
#' \eqn{l = 1, \dots, L}: The number of partitions.
#' \eqn{c_l}: The threshold for the \eqn{l}-th variable.
#' \eqn{x_{pl} \leq c_l} and \eqn{x_{pl} > c_l}: The subgroups defined by tree partitions.
#' \eqn{I(\cdot)}: The indicator function (1 if true, 0 otherwise).
#' \eqn{\gamma_{ik}} and \eqn{\gamma_{ir}}: Subgroup-specific intercepts for logistic regression models in partitioned regions.
#' The terminal or leaf nodes of the tree represent the final groups of patients with similar response patterns.
#' The IFT model assumes unidimensionality, and the covariates \eqn{x_{pl}} can be either continuous or categorical.
#'
#' If an item is never chosen for splitting, it is assumed to be free of DIF. The equation for an item free of DIF can be defined as:
#' \deqn{
#' \eta_{pi} = \beta_i S_p + \beta_0i,
#' }{
#' eta[pi] = beta[i] S[p] + beta[0i]
#' }
#'
#'
#' @import DIFtree
#'
#' @return A list containing:
#' \item{equations}{A set of logistic regression equations generated for each item.}
#' \item{predictions}{A dataset with predicted probabilities (\eqn{p}) and item responses (\eqn{I}), where \eqn{I = 1} if \eqn{p \geq 0.5}, and \eqn{I = 0} otherwise.}
#'
#' @examples
#' if (requireNamespace("DIFtree", quietly = TRUE)) {
#'   # Load DIFtree
#'   library(DIFtree)
#'
#' # Load the dataset
#' data("mydata", package = "IFTPredictor")
#'
#' # Observe the data
#' head(mydata)
#'
#' # Extract response and covariate data
#' Y <- mydata[, 1:20]  # Item responses
#' X <- mydata[, 21:24]  # Covariates
#'
#' # Create total score column calcualting total item score for each patient
#' mydata$total_score <- rowSums(mydata[, 1:20])
#'
#' # Fit the DIFtree model (Y = response data, X = covariate data)
#' mod <- DIFtree(Y, X, model = "Logistic", type = "udif", alpha = 0.05, nperm = 100, trace = TRUE)
#'
#' # Predict item responses using the model and the total score
#' result <- predict_item_responses(mod, dataset = mydata, total_score = "total_score")
#'
#' } else {
#'   message("The 'DIFtree' package is not installed. Please install it to run this example.")
#' }
#' @references
#' Berger, Moritz and Tutz, Gerhard (2016): Detection of Uniform and Non-Uniform Differential Item Functioning by Item Focused Trees,
#' Journal of Educational and Behavioral Statistics 41(6), 559-592.
#'
#' @seealso
#' \code{\link[DIFtree]{DIFtree}} for training the DIFtree model.
#'
#' @author
#' Muditha Bodawatte Gedara (muditha.lakmali.1993@gmail.com),
#' Barret Monchka,
#' Lisa Lix
#' @export
predict_item_responses <- function(model, dataset, total_score) {
  # Ensure total_score column is provided
  if (missing(total_score) || !total_score %in% colnames(dataset)) {
    stop("Please provide the 'total_score' column. Calculate it first and pass it to the function.")
  }

  # Step 1: Initialize columns
  #dataset <- Initilize_result_columns(model, dataset)

  # Step 2: Extract coefficients
  betas <- model$coefficients$betas
  gammas_dif <- model$coefficients$gammas_dif
  gammas_nodif <- model$coefficients$gammas_nodif

  # Step 3: Use splits from the model
  splits <- model$splits  # Extract splits from the model directly

  # Object to store equations for each item
  equations <- vector("list", length(betas))
  names(equations) <- paste0("Item_", seq_along(betas))

  # Step 4: Loop through betas and compute predictions
  for (i in seq_along(betas)) {

    dataset[[paste0("n", i)]] <- NA
    dataset[[paste0("p", i)]] <- NA
    dataset[[paste0("I", i)]] <- NA

    # Select the beta coefficient
    beta_coefficient <- betas[[i]]

    # Part 1 of the equation: reference the dataset directly
    equation_part1 <- paste0("(", beta_coefficient, " * dataset[['", total_score, "']])")

    if (i %in% names(gammas_dif)) {
      # DIF item: Generate branch equation
      leaf_node_names <- names(gammas_dif[[as.character(i)]])
      leaf_node_numbers <- as.numeric(leaf_node_names)
      leaf_node_coefficients <- gammas_dif[[as.character(i)]]

      split_rows <- splits[splits$item == i, ]
      split_rows_sorted <- split_rows[order(split_rows$number), ]

      equation_part2 <- branch_equation_generator(1, split_rows_sorted, leaf_node_numbers, leaf_node_coefficients)
      full_equation <- paste0(equation_part1, " + ", equation_part2)
    } else {
      # No DIF item: Use gamma_nodif
      gamma_nodif <- gammas_nodif[[paste0("gamma", i)]]
      full_equation <- paste0(equation_part1, gamma_nodif)
    }

    # Store the equation for the current item
    equations[[paste0("Item_", i)]] <- full_equation

    # Evaluate and compute columns
    dataset[[paste0("n", i)]] <- eval(parse(text = full_equation))
    dataset[[paste0("p", i)]] <- exp(dataset[[paste0("n", i)]]) / (1 + exp(dataset[[paste0("n", i)]]))
    dataset[[paste0("I", i)]] <- ifelse(dataset[[paste0("p", i)]] >= 0.5, 1, 0)
  }

  # Return the equations and the modified dataset as a list
  return(list(
    equations = equations,
    predictions = dataset
  ))
}

