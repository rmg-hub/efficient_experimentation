#### Optimal threshold for targeting ####
opt_tau <- function(offer_cost, customer_value){
  threshold <- offer_cost/customer_value
  return(threshold)
}

#### Catalogue Campaign Profit ####

catalogue_profit <- function(y, g, contact_cost, offer_cost=0, value){
  total <- 
    # Not treated/no purchase
    sum((1-g)*(1-y)) * 0 +
    # Treated/purchase
    sum(g*y)         * (value - contact_cost) +
    # Treated/no purchase
    sum(g*(1-y))     * (-contact_cost)+
    # Not treated/ purchase 
    sum((1-g)*y)     * (value)
  
  return(total)
}