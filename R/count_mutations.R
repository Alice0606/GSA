#' change genotype to number
#'
#' This function is used to convert the genotype offered by the users to numeric form.
#'
#' @param string The input genotype given by the user.
gt_to_num <- function(string) {
  base::sum(as.numeric(str_extract_all(string, "\\d")[[1]]))
}
#'  process table according to different counting alleles methods
#'the function is used to process a given data table with a colume of gene names and columes of population genotype. Note that three forms of output can be chosen by inputing one of the flags below:
#' 1. Alleles: How many alternative alleles each person carries.
#' 2. Snps: How many rare SNPs each person carries on this gene.
#' 3. Carrier: Whether a person has any allele on this gene or not.
#' @param table:  a dataframe contains a colume of gene names and columes of population genetype
#' @param gene.col: The serial number of the colume containing gene names
#' @param gt.col: The serial number of the first colume containing genetype, in the form of "1/1"
#' @param flag: one of the three forms of output: "Alleles", "Snps", "Carrier"
#' @return the table returned has rows named by different genes and columes named by sample names. The genetype will be counted accoring to the different methods chosen by the user, all in the form of a number(e.g. 1 or 0)
#' @examples
count_mutations <- function(table, gene.col, gt.col, flag = c("Alleles", "Snps", "Carrier")){


  table %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dplyr::across(gt.col:ncol(table), gt_to_num))

  table1 <- table %>%
    for(j in gt.col:ncol(table)){
      set(table, i= which(dt[[j]]!=0), j=j, value =1)
    }

  Allele <- table %>%
    dplyr::group_by(gene.col) %>%
    dplyr::summarise(across((gt.col-1):(ncol(table)-1),base::sum))

  carrier <- Allele %>%
    if(carrier[row,col] > 1){
      carrier[row, col] = 1
    }

  snps <- table1 %>%
    dplyr::group_by(gene.col) %>%
    dplyr::summarise(dplyr::across((gt.col-1):(ncol(table)-1),base::sum))

  if (flag == "Allele") {
    return(Allele)
  }
  if (flag == "snps") {
    return(snps)
  }
  if(flag == "carrier"){
    return(carrier)
  }
}
