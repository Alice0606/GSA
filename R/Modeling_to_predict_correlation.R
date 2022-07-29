#'@title tidy and extract needed data from glm
#'@description This function serves to clear up the total output of glm in a tidier form for further data analysis. Then, a row named "count" can be extracted out.
#'@param mod The output dataframe of mode function
b_fun <- function(mod){
  broom::tidy(mod) %>%
    dylyr::filter(term == "count")
}


#'@title model for glm
#'@description This function helps users to use the generalized linear models for predicting the correlation.

mod_fun <- function(df){
  stats::glm(disease~., data = df, family = "binomial")
}


#'@title Use modeling to calculate correlation
#'
#'@description With covariants and genesets, this function would calculate the necessary values that uncovers the correlation between genesets and diseases.
#' @param ped A raw dataframe from the bioinformation database.
#' @param disease The type of disease to be tested.
#' @param geneset The genesets(can be more than one)that users want to test the correlation with the disease.
#' @param covariates The covariates(e.g. age, gender)help testing the correlation.
#'
#' @return The return of this function is a table with p-value, conf.high and conf.high that give users direction on which geneset has greatest correlation to the disease.
#'
#' @examples
Modeling_to_predict_correlation <- function(ped, disease, geneset, covariates){
  data <- dpylr::select(ped, disease, geneset, covariates) %>%
    dplyr::as_tibble()

  df <- data %>%
    tidyr::pivot_longer(col = geneset, names_to = "category", values_to = "count") %>%
    dplyr::group_by(category) %>%
    tidyr::nest()%>%
    dplyr::mutate(model = purrr::map(data, mod_fun)) %>%
    dplyr::transmute(category,
                     beta = purrr::map(model, b_fun)) %>%
    tidyr::unnest() %>%
    dplyr::select(-term) %>%
    dplyr::ungroup()
  base::summary(glm.fit)

  output <- broom::tidy(glm.fit, conf.int = T)%>%
    dplyr::mutate(or = exp(estimate), pv = p.value, conf.low = conf.low, conf.high = conf.high)

  print(output)
}
