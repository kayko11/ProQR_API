library(plumber)
source("dataIngest.R")

#* Get total contracts by supplier
#* @param supplier_name The name of the supplier
#* @get /metrics/total_contracts
function(supplier_name) {
  if (missing(supplier_name) || supplier_name == "") {
    return(list(error = "Please provide a supplier_name parameter"))
  }
  
  total_contracts <- get_total_contracts(supplier_name)
  list(supplier = supplier_name, total_contracts = total_contracts)
}

#* Get all suppliers matching a pattern
#* @param pattern The pattern to search for in supplier names
#* @get /suppliers/matching
function(pattern) {
  if (missing(pattern) || pattern == "") {
    return(list(error = "Please provide a pattern parameter"))
  }
  
  matched_suppliers <- get_matching_suppliers(pattern)
  list(pattern = pattern, suppliers = matched_suppliers)
}

#* Get average contract value for a supplier
#* @param supplier_name The name of the supplier
#* @get /metrics/average_contract_value
function(supplier_name) {
  if (missing(supplier_name) || supplier_name == "") {
    return(list(error = "Please provide a supplier_name parameter"))
  }
  
  avg_contract_value <- get_average_contract_value(supplier_name)
  list(supplier = supplier_name, average_contract_value = avg_contract_value)
}

#* Get contract trends by year
#* @get /metrics/contract_trends
function() {
  trends <- get_contract_trends()
  list(trends = trends)
}

#* Get supplier performance over time
#* @param supplier_name The name of the supplier
#* @get /metrics/supplier_performance
function(supplier_name) {
  if (missing(supplier_name) || supplier_name == "") {
    return(list(error = "Please provide a supplier_name parameter"))
  }
  
  performance <- get_supplier_performance(supplier_name)
  list(supplier = supplier_name, performance = performance)
}
