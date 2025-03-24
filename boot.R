digga <- function(models, R = 1000, data = NULL, seed = 123) {
  require(lme4)
  require(lmerTest)
  require(ggplot2)
  require(boot)
  
  
  if(is.null(data)) {
    source_script("all")
    if(lpa_done == TRUE) {
      data <- create_data(lpa = TRUE)
    } else {
      data <- create_data(lpa = FALSE)
    }
  }
  
  results <- list()
  
  run_boot_lmm <- function(model, R, data, seed) {
    
    
    original_model <- lme4::lmer(as.formula(model), data = data)
    
    fe_names <- names(lme4::fixef(original_model))
    re_names <- c("Var_ID", "Var_Residual")
    fit_names <- c("AIC", "BIC", "logLik")
    t_names <- paste0("t_", fe_names)
    all_names <- c(fe_names, re_names, fit_names, t_names)
    
    # check if p-values are available
    model_summary <- summary(original_model)
    has_p_values <- "Pr(>|t|)" %in% colnames(model_summary$coefficients)
    
    boot_lmm <- function(data, indices) {
      boot_data <- data[indices, ]
      
      model_fit <- lme4::lmer(as.formula(model),
                              data = boot_data,
                              REML = FALSE,
                              control = lmerControl(optimizer = "bobyqa",
                                                    optCtrl = list(maxfun = 2e5))
                              )
      
      fixed_effects <- lme4::fixef(model_fit)
      random_effects <- c(lme4::VarCorr(model_fit)$ID[1], attr(lme4::VarCorr(model_fit), "sc")^2)
      model_fit_stats <- c(AIC(model_fit), BIC(model_fit), logLik(model_fit))
      t_values <- coef(summary(model_fit))[, "t value"]
      
      return(c(fixed_effects, random_effects, model_fit_stats, t_values))
    }
    
    set.seed(seed)
    boot_results <- boot::boot(data, boot_lmm, R = R)
    
    colnames(boot_results$t) <- all_names
    
    boot_summary <- data.frame(
      Original = c(fixef(original_model),
                   c(VarCorr(original_model)$ID[1],
                     attr(VarCorr(original_model), "sc")^2),
                   c(AIC(original_model),
                     BIC(original_model),
                     logLik(original_model)
                     ),
                   coef(summary(original_model))[, "t value"]),
      Mean = colMeans(boot_results$t, na.rm = TRUE),
      SE = apply(boot_results$t, 2, sd, na.rm = TRUE),
      CI_2.5 = apply(boot_results$t, 2, 
                     quantile, probs = 0.025, na.rm = TRUE),
      CI_97.5 = apply(boot_results$t, 2, 
                      quantile, probs = 0.975, na.rm = TRUE)
    )
    
    boot_summary$Significant <- (boot_summary$CI_2.5 > 0 | boot_summary$CI_97.5 < 0)
    
    plots <- list()
    
    model_data <- model.frame(original_model)
    
    plot_data <- data.frame(
      fitted = fitted(original_model),
      residuals = residuals(original_model)
    )
    
    plots$residuals_vs_fitted <- ggplot(plot_data) +
      geom_point(aes(fitted, residuals)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "Residuals vs Fitted", subtitle = model)
    
    if ("day" %in% colnames(model_data)) {
      model_data$residuals <- residuals(original_model)
      plots$residuals_by_day <- ggplot(model_data) +
        geom_boxplot(aes(x = factor(day), y = residuals)) +
        labs(title = "Residuals by Day")
    }
    
    return(list(
      summary = boot_summary,
      original_model = original_model,
      boot_results = boot_results,
      plots = plots
    ))
  }
  
  for(i in seq_along(models)) {
    model_formula <- models[i]
    cat(paste0("\nModel ", i, ": ", model_formula, " started.\n"))
    
    results[[paste0("model", i)]] <- run_boot_lmm(model = model_formula,
                                                  R = R,
                                                  data = data,
                                                  seed = seed)
  }
  
  names(results) <- paste0("model", seq_along(models))
  
  return(results)
}



boot_lmm_var_R <- function(R_values, data = NULL, model = NULL) {
  if(is.null(model)) {
    model <- "PHQ ~ day + (1 | ID)"
  }
  
  if(is.null(data)) {
    if (!exists("all", envir = globalenv())) {
      source_script("all")
      if(lpa_done == TRUE) {
        data <- create_all_df(lpa = TRUE)
        } else {
          data <- create_all_df(lpa = FALSE)
        }
      } else {
        data <- all
      }
    }
  
  results <- list()
  
  boot_lmm <- function(data, indices) {
    boot_data <- data[indices, ]
    
    model_fit <- lme4::lmer(as.formula(model), data = boot_data, REML = FALSE,
                            control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    
    fixed_effects <- lme4::fixef(model_fit)
    random_effects <- c(lme4::VarCorr(model_fit)$ID[1], 
                        attr(lme4::VarCorr(model_fit), "sc")^2)
    model_fit_stats <- c(AIC(model_fit), 
                         BIC(model_fit), 
                         logLik(model_fit))
    t_values <- coef(summary(model_fit))[, "t value"]
    
    return(c(fixed_effects, random_effects, model_fit_stats, t_values))
  }
  
  for (R in R_values) {
    cat("Bootstrapping with R =", R, "\n")
    
    boot_results_var <- boot::boot(data = data, 
                                   boot_lmm, 
                                   R = R)
    
    if (is.null(colnames(boot_results_var$t))) {
      colnames(boot_results_var$t) <- c("Intercept", "day", "Var_ID", "Var_Residual", "AIC", "BIC", "logLik", "t_Intercept", "t_day")
    }
    
    boot_summary <- data.frame(
      Variable = colnames(boot_results_var$t),
      R_Samples = R,
      Mean = colMeans(boot_results_var$t, na.rm = TRUE),
      SE = apply(boot_results_var$t, 2, sd, na.rm = TRUE),
      CI_2.5 = apply(boot_results_var$t, 2, quantile, probs = 0.025, na.rm = TRUE),
      CI_97.5 = apply(boot_results_var$t, 2, quantile, probs = 0.975, na.rm = TRUE)
    )
    
    results[[as.character(R)]] <- boot_summary
  }
  
  return(do.call(rbind, results))
}