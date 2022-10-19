
# clear environment  
    rm(list = ls())  
    
# a function that will create a balance table  
    balance_table_frame <- function(vars, treat_var, data_set) {
    # load packages using pacman (installs package if necessary)
    if (!require(pacman)) install.packages('pacman', repos = 'https://cran.rstudio.com')
    pacman::p_load(broom, haven, knitr, sandwich, data.table, tidyverse) 
  
    # turn off scientific notation except for big numbers
    options(scipen = 9) 
    options(digits=3)
    # function to calculate corrected SEs for OLS regression 
    cse = function(reg) {
      rob = sqrt(diag(vcovHC(reg, type = "HC1")))
      return(rob)
    } 
    n_vars <- length(vars) 
    treat <- rep(treat_var, t=n_vars)
    data <- rep(data_set, t=n_vars) 
  
    coeff <- function(y, x, d) { 
      df <- get(d) 
      reg <- lm(as.formula(paste0(y," ~ ", x)), data=df) 
      n_obs <- summary(reg)$df[2] + 2        # grab the regression sample size
      coef <- tidy(reg) %>% 
        select(term, estimate, std.error)  
      rob_se <- as.data.frame(cse(reg)) %>%           # robust SEs
        rownames_to_column(var = "term") %>% 
        rename(se_robust = `cse(reg)`) 
      treat_est <- coef %>% 
        left_join(rob_se) %>% 
        mutate(treat_var = x,
               variable = y,
               n_obs = n_obs, 
               p_value = 2*pnorm(-abs(estimate/se_robust),0,1)) 
      treat_est <- as.data.frame(treat_est)
      return(treat_est)
    }

  # use mapply to get the coefficients and p values 
    qq <- mapply(coeff, vars, treat, data, SIMPLIFY = FALSE) 
  
  # get the data set for table
    qqq <- do.call("rbind",qq)  
    control <- qqq %>% 
      filter(term=="(Intercept)") %>% 
      select(control=estimate, variable)
    balance_table <- qqq %>% 
      filter(term!="(Intercept)")  %>% 
      select(diff=estimate, variable, p_value, n_obs) %>% 
      left_join(control) %>% 
      mutate(treated = control+diff) %>% 
      select(variable, control, treated, diff, p_value, n_obs)
    }
  
# create a data frame 
  N=1000
  set.seed(78)
  df=data.frame(education=sample(10:20, N, replace=TRUE),
              ofjobs=rbinom(N,1,.6),
              yearsexp=sample(5:130, N, replace=TRUE),
              computerskills=rnorm(N,50,5),
              female=rbinom(N,1,.3),
              treat_group=sample(1:3, N, replace=TRUE))   
table(df$treat_group)
# apply the balance table function
 xx <- balance_table_frame(c("education","ofjobs","yearsexp","computerskills","female"), c("treat_group"), c("df"))
 xx
 