
#' A linear regression package in R.

#'

#' A package to handle linear regression models. We will use linear algebra to create the most basic functionality in the R package.

#'

#' @field formula formula which the model will process. 

#' @field data data.frame which is the input data sample. 

#' @field parsedata character variable which we need to parse input data. 

#' @field beta matrix result for regressions coefficients. 

#' @field ybar matrix result for the fitted values.

#' @field ebar matrix which contains the result of the residuals. 

#' @field ddf numeric which contains the degrees of freedom. 

#' @field sigma_sq numeric which contains the residual variance. 

#' @field varian numeric which is the variance of the sample data. 

#' @field stdres matrix is the standardized residuals value.

#' @field var_beta matrix is the variance of the regression coefficients. 

#' @field t_beta matrix is the t-values for each coefficient. 

#' @field p matrix gives the  p-values for each regression coefficient.

#' 

#' @return print() prints out the coefficients and coefficient names,

#'         plot() plots the following two plots using ggplot2,

#'         resid() returns the vector of residuals e,

#'         pred() returns the predicted values of ybar,

#'         coef() returns the coefficients as a named vector,

#'         summary() returns a similar printout as printed for lm objects.

#'         

#' @import methods

#' 

#' @references \href{https://en.wikipedia.org/wiki/Linear_regression}{Linear Regression Model}

#' @examples linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length,data=iris)

#'  linreg_mod$print()

#'  linreg_mod$pred()

#'  linreg_mod$resid()

#'  linreg_mod$coef()

#'  linreg_mod$summary()

#'  linreg_mod$plot()

#'           

#' 



linreg <-setRefClass( Class = "linreg",
                      
                      fields = list(formula = "formula",
                                    
                                    data = "data.frame",parsedata = "character",beta="matrix",
                                    
                                    ybar="matrix",ebar="matrix",
                                    
                                    ddf="numeric",sigma_sq="numeric",varian="numeric",stdres="matrix",
                                    
                                    var_beta="matrix",t_beta="matrix",p="matrix"),
                      
                      methods=list(
                        
                        initialize= function(formula,data){
                          
                          formula  <<- formula
                          
                          data <<- data
                          
                          parsedata <<- deparse(substitute(data))
                          
                          x <- model.matrix(formula, data)
                          
                          yy  <- all.vars(expr = formula)[1]
                          
                          y <- (data[, yy])
                          
                          #Computations using ordinary least squares
                          
                          
                          
                          beta<<-solve(t(x) %*% x) %*% t(x) %*% y
                          
                          ybar<<-x %*% beta
                          
                          ebar<<- y -ybar
                          
                          ddf<<- nrow(x)-ncol(x)
                          
                          sigma_sq<<-as.numeric((t(ebar) %*% ebar) / ddf)
                          
                          var_beta<<-  sigma_sq * solve(t(x) %*% x)
                          
                          varian<<-round(sqrt(sigma_sq), 2)
                          
                          t_beta<<-beta / sqrt(diag(var_beta))
                          
                          p<<-pt(abs(t_beta), ddf,lower.tail=FALSE)
                          
                          stdres<<-sqrt(abs((ebar - mean(ebar)) / sqrt(sigma_sq)))
                          
                        },     
                        
                        #Methods for Linear Regression
                        
                        
                        
                        #Print method
                        
                        print = function() {
                          
                          
                          
                          cat("\n","Call:","\n",
                              
                              paste("linreg(", "formula = ", formula[2]," ", formula[1], " ", formula[3],", ", "data = ", parsedata, ")",sep = "", collapse = "\n" ),
                              
                              "\n","Coefficients:","\n",
                              
                              paste(row.names(beta),
                                    
                                    sep = "  ", collapse ="  " ),"\n",
                              
                              format(round(beta,2), justify = "centre",width = 10))
                          
                        },
                        
                        
                        
                        plot=function() {
                          
                          
                          
                          library(ggplot2)
                          
                          library(ggThemeAssist)
                          
                          
                          
                          #Theme             
                          
                          liu_graphic_theme<-   theme(
                            
                            
                            
                            panel.background = element_rect(fill="white"),
                            
                            plot.margin = unit(c(1,1,1,1), "cm"),
                            
                            plot.caption = element_text(size=12, hjust=0.5, margin=margin(t=15)),
                            
                            plot.title = element_text(color="#666666", face="bold", size="15",hjust=0.5),
                            
                            panel.grid.major.y = element_blank(),
                            
                            panel.grid.minor.y = element_blank(),
                            
                            panel.grid.major.x = element_blank(),
                            
                            panel.grid.minor.x = element_blank(),
                            
                            axis.line = element_line(color= "#666666", size=0.1),
                            
                            axis.text.x = element_text(color="#666666", size="5"),
                            
                            axis.text.y = element_text(color="#666666", size="5"),
                            
                            axis.title.x = element_text(color="#666666", size="12", face="bold"),
                            
                            axis.title.y = element_text(color="#666666", size="12", face="bold"),
                            
                            axis.ticks.x = element_line(color = "blue", size = 0.3)
                            
                          )
                          
                          
                          
                          b<-expression(bold(sqrt(bold("Standardized Residuals"))))
                          
                          caption<-  paste("linreg(", formula[2]," ", formula[1], " ", formula[3], ")")
                          
                          #Plot1           
                          
                          df<- data.frame(FittedValues=ybar,Residulas=ebar)
                          
                          Plot1<-ggplot(df, aes(x = FittedValues, y = Residulas))+
                            
                            geom_point(shape = 21, colour = "#666666", fill = "white", size = 3, stroke = 1.5) + ggtitle("Residual vs Fitted") +
                            
                            geom_abline(slope = 0, intercept = 0,linetype = "dotted") +
                            
                            geom_smooth(method = "loess",color = "red", se = FALSE) +
                            
                            ylab("Residuals") + xlab("Fitted values")+labs(caption=caption)+
                            
                            liu_graphic_theme
                          
                          
                          
                          df1<- data.frame(FittedValues=ybar,Residulas1=stdres)
                          
                          #Plot2        
                          
                          
                          
                          Plot2<-ggplot(df1, aes(x = FittedValues, y = Residulas1))+
                            
                            geom_point(shape = 21, colour = "#666666", fill = "white", size = 3, stroke = 1.5) + ggtitle("Scale-Location") +
                            
                            geom_abline(slope = 0, intercept = 0,linetype = "dotted") +
                            
                            geom_smooth(method = "loess",color = "red", se = FALSE) +ylab(b)+
                            
                            labs(caption=caption)+
                            
                            xlab("Fitted values")+ 
                            
                            liu_graphic_theme
                          
                          
                          
                          return(list("Residual vs Fitted"= Plot1,"Scale Location"= Plot2))
                          
                          
                          
                        },
                        
                        #predicted value method
                        
                        
                        
                        pred =function(){
                          
                          cat("Predicted values or fitted values:","\n")
                          
                          return(as.vector(round(ybar, 2)))
                          
                        },
                        
                        
                        
                        #Residuals method
                        
                        
                        
                        resid = function(){
                          
                          cat("Vector of Residuals:","\n")
                          
                          return(as.vector(round(ebar, 2)))
                          
                        },
                        
                        
                        
                        #Coefficient method 
                        
                        
                        
                        coef = function() {
                          
                          cat("Regressions coefficients:","\n\n")
                          
                          return(as.vector(round(beta,2)))
                          
                        },
                        
                        
                        
                        #Summary method 
                        
                        summary = function() {
                          
                          output <- data.frame( rname = rownames(beta),
                                                
                                                Estimate =round(beta,2),Std_err = round(sqrt(diag(var_beta)),2),t_value = round(t_beta, 2),
                                                
                                                pr = round(p, 4))
                          
                          output$rname <- as.character(output$rname)
                          
                          row.names(output) <- NULL
                          
                          output <- rbind(c("","Estimate", "Std. Error", "t value", "Pr(>|t|)"), output)
                          
                          
                          
                          for(i in 2:nrow(output)){
                            
                            
                            
                            if(output$pr[i] == 0){
                              
                              output$pr[i] <- "***"
                              
                            } else if(output$pr[i] > 0 & output$pr[i] <= 0.001){
                              
                              output$pr[i] <- "**"
                              
                            } else if(output$pr[i] > 0.001 & output$pr[i] <= 0.01){
                              
                              output$pr[i] <- "*"
                              
                            } else if(output$pr[i] > 0.01 & output$pr[i] <= 0.05){
                              
                              output$pr[i] <- "."
                              
                            } else if(output$pr[i] > 0.05 & output$pr[i] <= 0.1){
                              
                              output$pr[i] <- " "
                              
                            } else if(output$pr[i] > 0.1){
                              
                              output$pr[i] <- " "
                              
                            }
                            
                          }
                          
                          cat("\n Call:\n\n",
                              
                              paste("linreg(", "formula = ", formula[2]," ", formula[1], " ", formula[3],", ", "data = ", parsedata, ")",sep = "", collapse = "\n" ),
                              
                              "\n\n Coefficients:\n\n")
                          
                          
                          
                          for(i in 1:ncol(output)){
                            
                            distance <- max(nchar(as.character(output[, i])), na.rm = TRUE)
                            
                            for(j in 1:nrow(output)){
                              
                              output[j, i] <- format(output[j, i], width=distance,justify = c("right"))
                              
                            }
                            
                          }
                          
                          
                          
                          for(i in 1:nrow(output)){
                            
                            cat(paste(as.character(output[i, ]),collapse = " "),"\n")
                            
                          }
                          
                          cat("\n Residual standard error:",varian, "on", ddf, "degrees of freedom")
                          
                          
                          
                        }))

 

 linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length,data=iris)

 

 #linreg_mod$print()

 
 #linreg_mod$pred()

 #linreg_mod$resid()

 #linreg_mod$coef()

#linreg_mod$summary()

linreg_mod$plot()