Task3 <- function(ped, disease, geneset, covariates){
  data <- select(ped, disease, geneset, covariates) %>%
    as.tibble()
  
  mod_fun <- function(df){
    glm(disease~., data = df, family = "binomial")
  }
  
  b_fun <- function(mod){
    tidy(mod) %>%
      filter(term == "count")
  }
  
  df <- data %>%
    pivot_longer(col = geneset, names_to = "category", values_to = "count") %>%
    group_by(category) %>%
    nest()%>%
    mutate(model = map(data, mod_fun)) %>%
    transmute(category, beta = map(model, b_fun))%>%
    unnest() %>%
    select(-term) %>%
    ungroup()
  
  output <- broom::tidy(glm.fit, conf.int = T)%>%
    mutate(or = exp(estimate), 
           pv = p.value, 
           conf.low = conf.low, 
           conf.high = conf.high)
  
}