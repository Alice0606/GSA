#' @title tidy and extract needed data from glm
#' @description This function serves to clear up the total output of glm in a tidier form for further data analysis. Then, a row named "count" can be extracted out.
#' @param mod The output dataframe of mode function
#' @importFrom rlang .data
b_fun <- function(mod){
  broom::tidy(mod) %>%
    dplyr::filter(.data$term == "count")
}


#' @title model for glm
#' @description This function helps users to use the generalized linear models for predicting the correlation.
#' @param df The dataframe users want to use glm on.
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
#' @importFrom rlang .data
#' @examples
Modeling_to_predict_correlation <- function(ped, disease, geneset, covariates){
  data <- dplyr::select(ped, disease, geneset, covariates) %>%
    dplyr::as_tibble()

  df <- data %>%
    tidyr::pivot_longer(col = geneset, names_to = "category", values_to = "count") %>%
    dplyr::group_by(.data$category) %>%
    tidyr::nest()%>%
    dplyr::mutate(model = purrr::map(data, mod_fun)) %>%
    dplyr::transmute(.data$category,
                     beta = purrr::map(.data$model, b_fun)) %>%
    tidyr::unnest() %>%
    dplyr::select(-.data$term) %>%
    dplyr::ungroup()
  base::summary(.data$glm.fit)

  output <- broom::tidy(.data$glm.fit, conf.int = T)%>%
    dplyr::mutate(or = exp(.data$estimate), pv = .data$p.value, conf.low = .data$conf.low, conf.high = .data$conf.high)

  print(output)
}
