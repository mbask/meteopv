###############################################################################
#
# treecm: An R package to 
# author: Marco Bascietto <marco@bascietto.name>
#
# This is released under a GPL license.
#
###############################################################################

#' @title Assessment of the position of the centre of mass of trees
#'
#' @description The centre of mass is a crucial data for arborists in order to consolidate a tree using steel or dynamic cables. Given field-recorded data on branchiness of a tree, the package:
#' \itemize{
#'   \item{computes and plots the centre of mass of the tree itself}
#'   \item{simulates the shift in CM position as branches are pruned}
#'   \item{computes branches slenderness coefficient in order to aid the arborist identify potentially dangerous branches}
#'   \item{computes the force acting on a ground plinth and its best position relating to the tree centre of mass, should the tree need to be stabilized by a steel cable}
#' }
#' The tree stem is ideally sectioned in logs. The weight of tree components is assessed based on
#' \itemize{
#'   \item the sum of volume of stem logs
#'   \item the sum of branches biomass
#' }
#' @note \bold{Branch biomass} is computed by allometric equations relating its fresh weight (wood + leaves) to its diameter at point of insertion on the stem. \bold{Log biomass} is computed by converting its volume to #' A sample \code{.CSV} file is provided to guide through data filling in the field
#' @name meteopv-package
#' @aliases meteopv
#' @docType package
#' @author Marco Bascietto \email{marco@@bascietto.name}
#' @keywords package
#' @references Source code is hosted at GitHub (\url{http://mbask.github.com/meteopv/})
NULL
