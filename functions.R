#   Splits vector into list of vectors, each with n items then prods growth
#   rates to get agg. growth rates.

agg_prod <- function(vector, n) {
    
    nr <- length(vector)
    
    list_of_vecs <- split(vector, ceiling(1:nr / n))
    return_value <- sapply(list_of_vecs, prod)
    
    return(return_value)
}
