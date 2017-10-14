ridgereg <- setRefClass(
  
  Class = "ridgereg",
  
  fields = list(
    
    formula1 = "formula",
    
    data1 = "data.frame",
    
    lambda1 = "numeric",
    
    beta_hat1 = "matrix",
    
    beta_zero1 = "numeric",
    
    y_hat1 = "matrix",
    
    data_name1 = "character"
    
  ),
  
  methods = list(
    
    initialize = function(formula1, data1, lambda1) {
      
      formula1 <<- formula1
      
      data1 <<- data1
      
      lambda1 <<- lambda1
      
      normalise <- TRUE
      
      
      
      data_name1 <<- deparse(substitute(data1))
      
      
      X1 <- model.matrix(formula1, data1)
      
      
      
      if (normalise == TRUE) {
        
        
        X1 <- scale(X1[, -1])
        
      }
      
      dep_name1 <- all.vars(expr = formula1)[1]
      
      y1 <- (data1[, dep_name1])
      
      I1 <- diag(ncol(X))
      
      beta_hat1 <<- solve((t(X1) %*% X1 + lambda1 * I1)) %*% t(X1) %*% y1
      
      beta_zero1 <<- mean(y1)
      
      
      
      y_hat1 <<- X1 %*% beta_hat1 + beta_zero1
      
    },
    
    coef = function() {
      
      coef_v1 <- as.vector(beta_hat1)
      
      names(coef_v1) <- c(row.names(beta_hat1))
      
      return(coef_v1)
      
    },
    
    print = function() {
      
      cat(sep = "\n")
      
      cat("Call:")
      
      cat(sep = "\n")
      
      cat(
        
        paste(
          
          "ridgereg(",
          
          "formula = ",
          
          formula1[2],
          
          " ",
          
          formula1[1],
          
          " ",
          
          formula1[3],
          
          ", ",
          
          "data = ",
          
          data_name1,
          
          ")",
          
          sep = ""
          
        )
        
      )
      
      
      
      cat(sep = "\n")
      
      cat(sep = "\n")
      
      cat("Coefficients:")
      
      
      
      cat(sep = "\n")
      
      coef_v1 <- c(beta_zero1, beta_hat1)
      
      names(coef_v1) <- c("(Intercept)", row.names(beta_hat1))
      
      
      
      return(coef_v1)
      
      
      
    },
    
    predict = function() {
      
      return((Fitted_values = round(y_hat1, 2)))
      
    }
    
    
    
  )
  
)