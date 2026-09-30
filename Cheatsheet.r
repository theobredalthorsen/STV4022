
 
# Funksjon for standardavvik
se_func <- function(x){
  var <- x 
  se <- sd(var)/sqrt(length(var))
  return(se)
}
