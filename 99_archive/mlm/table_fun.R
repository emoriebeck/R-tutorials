table_fun <- function(model){
  fixed <- broom::tidy(model) %>% filter(group == "fixed") %>%
    select(term, estimate) 
  ## add random effects ##
  rand <- VarCorr(model)[[1]]
  if(nrow(rand) > 1){
  rand <- rand[1:nrow(rand), 1:nrow(rand)]
  }
  colnames(rand)[colnames(rand) == "(Intercept)"] <- "Intercept"
  rownames(rand)[rownames(rand) == "(Intercept)"] <- "Intercept"
  vars <- rownames(rand)
  rand[upper.tri(rand)] <- NA
  rand <- data.frame(rand) %>% mutate(var1 = rownames(.)) %>%
    gather(key = var2, value = estimate, -var1, na.rm = T) %>%
    mutate(var1 = mapvalues(var1, vars, 0:(length(vars)-1)),
           var2 = mapvalues(var2, unique(var2), 0:(length(vars)-1))) %>%
    filter(var1 == var2) %>%
    unite(var, var1, var2, sep = "") %>%
    mutate(var = sprintf("$\\tau_{%s}$", var))
  ## get confidence intervals ##
  CI <- data.frame(confint.merMod(model, method = "boot", nsim = 10, oldNames = F)) %>%
    mutate(term = rownames(.)) %>% setNames(c("lower", "upper", "term"))
  
  CI %>% filter(term == "sigma") %>%
    mutate(estimate = sigma(model),
           term = "$\\sigma^2$",
           type = "Residuals")
  
  ## Get ICC & R2 values ##
  ICC <- reghelper::ICC(model)
  R2 <- MuMIn::r.squaredGLMM(model)
  
  ## format the fixed effects
  fixed <- fixed %>% left_join(CI %>% filter(!grepl(".sig", term))) %>%
    mutate(type = "Fixed Parts")
  
  rand <- rand %>%
    left_join(
      CI %>% filter(grepl("sd", term)) %>%
        mutate(lower = lower^2, upper = upper^2,
               var = mapvalues(term, unique(term), 0:(length(unique(term))-1)),
               var = sprintf("$\\tau_{%s%s}$", var, var)) %>% select(-term)) %>%
    mutate(type = "Random Parts") %>% rename(term = var)
  
  mod_terms <- tribble(
    ~term, ~estimate, ~type,
    # "ICC", ICC, "Model Terms",
    "$R^2_m$", R2[1], "Model Terms",
    "$R^2_c$", R2[2], "Model Terms"
  )
  
  tab <- fixed %>%
    full_join(rand) %>%
    mutate(CI = sprintf("[%.2f, %.2f]", lower, upper)) %>%
    select(-lower, -upper) %>%
    full_join(mod_terms) %>%
    mutate(estimate = sprintf("%.2f", estimate)) %>%
    dplyr::rename(b = estimate) %>%
    select(type, everything())
  return(tab)
}
