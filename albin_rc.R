make_class_linreg_RC<-setRefClass("linreg", fields = list(Call="formula",
                                                          Coefficients="numeric",
                                                          X_terms="matrix",
                                                          Y_terms="matrix",
                                                          Fits="numeric",
                                                          Residuals="numeric",
                                                          df="numeric",
                                                          Var_residuals="numeric",
                                                          Std_betas="numeric",
                                                          tBetas="numeric",
                                                          Pvalues="numeric",
                                                          Input="character"),
                                  methods = list(
                                    pred = function(){
                                      return(Fits)
                                    }
                                    
                                    
                                  ))