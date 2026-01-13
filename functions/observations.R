
read_observations = function(scientificname = "Eubalaena glacialis",
                             minimum_year = 1970, 
                             basis_of_record = NULL,
                             individual_count = NULL,
                             ...){
  
  #' Read raw OBIS data and then filter it
  #' 
  #' @param scientificname chr, the name of the species to read
  #' @param minimum_year num, the earliest year of observation to accept or 
  #'   set to NULL to skip
  #' @param ... other arguments passed to `read_obis()`
  #' @return a filtered table of observations
  
  # Happy coding!
  
  # read in the raw data
  if (!is.null(scientificname)){
    x = read_obis(scientificname = scientificname, ...) |>
      dplyr::mutate(month = factor(month, levels = month.abb))

    # if the user provided a non-NULL filter by year
    if (!is.null(minimum_year)){
      x = x |>
        filter(year >= minimum_year)
    }
    
    x = x |> filter(!is.na(eventDate))
    
    if (!is.null(basis_of_record)){
      x = x |> filter(basisOfRecord == basis_of_record)
    }
    
    if (!is.null(individual_count)){
      x = x |> filter(individualCount == individual_count)
    }
    
    return(x)
  }
}
