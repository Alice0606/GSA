#' count genotype of specified genes
#' the function is used to count the genotype of specified genes inputed by the user. A reference of which geneset has a higher impact on the disease.
#' @param Table  a dataframe contains a colume of gene names and columes of population genetype which have been processed into numeric form.
#' @param GeneSet  The name of gene(can be more than one) you are interested in.
#' @param Name  the name of geneset you come up with.
#'
#' @return The return of the function is a table containing two columes named by "Sample ID" and user's input "Name". The "Name" colume dislpays the sum of genotypes of the genes offered by the user.
#'
#' @examples
Count_variant <- function(Table, GeneSet, Name) {
  
  if(is.tibble(Table) == FALSE){
    stop("Table needs to be a tibble")
  }
  
  variant.per.gene_set.per.sample <- Table %>%
    dplyr::filter(gene %in% GeneSet) %>%
    dplyr::summarise(across(2:ncol(Table),sum)) %>%
    tidyr::pivot_longer( everything(), names_to = "sampleID", values_to = Name)
  
  return(variant.per.gene_set.per.sample)
}