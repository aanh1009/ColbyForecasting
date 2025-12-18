



read_workflow = function(version = "Aug",
                         type = "rand_forest",
                         path = data_path("models")){
  #' Read a workflow given the version and path
  #' 
  #' @param version str the version to read
  #' @param type str the model type name ala "rand_forest"
  #' @param path str the path to the data directory
  #' @return workflow
  
  filename = file.path(path,
                       sprintf("%s-%s-workflow.rds", version, type))
  if (!file.exists(filename)) stop("file not found:", filename)
  readr::read_rds(filename) |>
    bundle::unbundle()
}


write_workflow = function(x,
                          version = "Aug",
                          type = get_model_type(x),
                          path = data_path("models")){
  
  #' Write a workflow given the workflow, version and path
  #' 
  #' @param x workflow object
  #' @param version str the version to write
  #' @param path str the path to the data directory
  #' @return workflow itself is returned

  filename = file.path(path,
                       sprintf("%s-%s-workflow.rds", version, type))
  x |>
    bundle::bundle() |>
    readr::write_rds(filename)
}


get_model_type = function(x){
  
  #' A convenience to quickly get the type name of a model 
  #' 
  #' @param x workflow object
  #' @return string value of the model type
  
  workflows::extract_spec_parsnip(x) |> 
    class() |>
    getElement(1)
}
