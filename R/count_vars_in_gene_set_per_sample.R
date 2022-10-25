#'@title count variants in gene set per sample
#'@description the function is used to count the variants in specified genes input by the user. This function provides reference of which gene set has a higher impact on the disease.
#' @param table  a tibble contains a column of gene names and columns of population gene types which have been processed into numeric form.
#' @param gene.set  A column of characters of the name of genes you are interested in.
#' @param gene.set.name  A column of characters of the name of gene sets you come up with.
#'
#' @return The return of the function is a data frame containing two columns named by "Sample ID" and user's input "Name". The "Name" column displays the sum of variants of the genes offered by the user.
#' @importFrom rlang .data
#' @examples
count_vars_in_gene_set_per_sample <- function(table, gene.set, gene.set.name) {

  if(tibble::is_tibble(table) == FALSE){
    stop("The input table needs to be a tibble format")
  }

  table %>%
    dplyr::filter(dplyr::if_all(1, ~ . %in% gene.set)) %>%
    dplyr::summarise(dplyr::across(2:ncol(table),base::sum)) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "sampleID", values_to = gene.set.name)
}
