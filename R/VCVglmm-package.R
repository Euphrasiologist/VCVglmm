#' VCVglmm: manipulating, diagnosing and explaining MCMCglmm and lme4 models
#'
#' Start with \code{\link{glmm_explain}} for a plain-English summary of a model,
#' \code{\link{MCMCdiag}} / \code{\link{merdiag}} for diagnostics and
#' \code{\link{glmm_varpart}} / \code{\link{glmm_R2}} for variance partitioning.
#'
#' @import MCMCglmm
#' @importFrom methods as
#' @importFrom stats density var
#' @importFrom graphics pairs
#' @importFrom utils globalVariables
#' @keywords internal
"_PACKAGE"

# column names used in ggplot2/data.table non-standard evaluation
utils::globalVariables(c(
  "component", "value", "mode", "lower", "upper", "iteration", "chain",
  "running.mean", "lag", "acf", "row", "r", "distribution", "fitted",
  "resid", "variable", "group", "enhance", "contrast", "estimate", "SE",
  "ptext", "B", "med.x", "med.y", "val_mean", "parameter", "."
))
