#' Yacht Hydrodynamics Dataset for Package
#'
#' This curated dataset encompasses 100 observations tailored for hydrodynamic performance analysis in yacht design. It incorporates six explanatory variables alongside a response variable, the Residuary Resistance per Unit Weight of Displacement (RRPUWOD), which serves as a critical metric in evaluating yacht performance. The dataset facilitates the exploration of how various design parameters impact the hydrodynamic efficiency of yachts.
#'
#' @format A data frame comprising 100 observations across 7 features:
#' \describe{
#'   \item{RRPUWOD}{Numeric: Residuary Resistance per Unit Weight of Displacement, a pivotal response variable indicative of hydrodynamic performance.}
#'   \item{LPOTCOB}{Numeric: Longitudinal Position of the Center of Buoyancy, influencing yacht balance and stability.}
#'   \item{PrismaticCoefficient}{Numeric: A dimensionless quantity reflecting the volume distribution along the length of the yacht.}
#'   \item{LengthDisplacementRatio}{Numeric: Ratio signifying the yacht's length to its displacement, affecting its speed and maneuverability.}
#'   \item{BeamDraughtRatio}{Numeric: Beam to Draught Ratio, crucial for assessing the yacht's lateral stability in water.}
#'   \item{LengthBeamRatio}{Numeric: Length to Beam Ratio, relevant for determining the yacht’s operational efficiency.}
#'   \item{FroudeNumber}{Numeric: A non-dimensional parameter pivotal for comparing dynamic similarities between different maritime vessels.}
#' }
#'
#' @usage
#' data(yacht)
#'
#' @details
#' The dataset is instrumental for conducting regression analysis and predictive modeling to discern the influence of design attributes on yacht hydrodynamics. This version, specifically adapted for the package, simplifies complex hydrodynamic assessments, making it an invaluable resource for both educational and research purposes in naval architecture.
#'
#' @references
#' This dataset represents a condensed version (first 100 observations) of the comprehensive dataset available at the UCI Machine Learning Repository - Yacht Hydrodynamics:
#'
#' [UCI Yacht Hydrodynamics Dataset](https://archive.ics.uci.edu/ml/datasets/Yacht+Hydrodynamics)
#'
#' It is used here for illustrative purposes within the scope of the package.
"yacht"
