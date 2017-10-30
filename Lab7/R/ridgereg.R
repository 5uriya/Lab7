ridgereg <- setRefClass( Class = "ridgereg",
                         fields = list(formula = "formula",data = "data.frame",
                                       parsedata = "character",
                                       beta_ridge_QR ="matrix",ybar_ridge_QR="matrix", 
                                       names="vector"),
                         methods=list(
                           initialize= function(formula,data,lambda,normalise = TRUE){
                             formula  <<- formula
                             data <<- data
                             parsedata <<- deparse(substitute(data))
                             x <- model.matrix(formula, data)
                             x<- ((x[,-1]-mean(x[,-1])) / sd(x[,-1]))
                             x<-x-mean(x)/sd(x)
                             lambda<-lambda
                             yy  <- all.vars(expr = formula)[1]
                             y <- as.matrix(data[, yy])
                             x_norm<-x
                             #using QR decompositon
                             identity_matrix<- diag(ncol(x_norm))
                             QR_ridge<- qr(x_norm)
                             Q<-qr.Q (QR_ridge)
                             R<-qr.R(QR_ridge)
                             RR<- ((R %*% t(Q) %*% Q) + lambda *(t(Q) %*% Q %*% solve(t(R))))
                             
                             beta_ridge_QR<<- backsolve(RR, crossprod(Q,y))
                             names<<-c(colnames(R))
                             ybar_ridge_QR<<- (x_norm %*% beta_ridge_QR)
                             
                           },     
                           
                           print= function(){
                             
                             
                             cat("\n","Call:","\n",
                                 paste("ridgereg(", "formula = ", formula[2]," ", formula[1], " ", formula[3],
                                       ", ", "data = ", parsedata, ", lambda = 0)",sep = "", collapse = "\n" ),
                                 "\n","Coefficients:","\n",
                                 format(names, justify = "centre",width = 12),"\n",
                                 format(round(beta_ridge_QR,2), justify = "centre",width = 12))
                             
                           },
                           
                           predict =function(){
                             cat("\n \n Predicted values or fitted values using QR decomposition:","\n\n")
                             return(as.vector(round(ybar_ridge_QR, 2)))
                           },
                           
                           coef= function(){
                             cat("\n \nRegressions coefficients using QR decomposition:","\n\n")
                             ridge_coef_QR <-as.vector(round(beta_ridge_QR,2))
                             names(ridge_coef_QR)<-names
                             return (ridge_coef_QR)
                           }
                           
                         ))