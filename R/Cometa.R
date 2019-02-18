#' El Cometa Lagoon data frame with vegetation and texture attributes.
#'
#' This dataframe contains the plant community's structural and diversity attributes
#' for each sampling plot: mean height, standard deviation of height, crown cover area,
#' basal area, density of individuals, aboveground biomass, species richness,
#' Simpson's diversity index, Shannon's diversity index. Additionaly, for each plot, its
#' glcm metrics  or Haralick's textures are shown, 8 textures for the red band (R) and 8
#' for NIR (NIR) extracted from a Kompsat-2 multispectral image. These eight textures were
#' mean, variance, homogeneity, contrast, dissimilarity, entropy, second angular moment and
#' correlation.
#'
#' @docType data
#'
#' @usage data(Cometa)
#'
#' @format An object of class data.frame
#'
#' @keywords datasets
#'
#' @references Solórzano et al. (2018) Applied Journal of Remote Sensing 12(3): 036006
#' (\href{https://doi.org/10.1117/1.JRS.12.036006)

#' @examples
#' data(Cometa)
#' TotalmeanHeight <- mean(Cometa_data$meanHeight)


