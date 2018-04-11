#' Setup Folders
#'
#' creates the folders necessary for the data that will be produced. The rawmempool
#' folder has to be filled with the raw webscraped data since it will be used as the basis
#' for the data generation.
#'
#' @return NULL
#' @export
#'
#' @examples
#' setup_folder()

setup_folder <- function(){

  folder_names <- c("data/merged_rmp_val_nonval_blockinfo",
                    "data/nonvalidated_tx",
                    "data/nonvalidated_tx_no_duplicates",
                    "data/rawmempool",
                    "data/validated_tx")

  print_help <- sum(!dir.exists(folder_names))

  for(folder in folder_names){
    if(!dir.exists(folder)){
      dir.create(folder, recursive = TRUE)
    }
  }

  cat(print_help, " folders got created in ", getwd())
  return(NULL)
}
