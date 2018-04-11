#' eliminate_duplicates_nonvaltx
#'
#' @param blocknr vector with blocknumbers for which to create the files without duplicates in an extra folder
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' eliminate_duplicates_nonvaltx(500000:501000)
#' }

eliminate_duplicates_nonvaltx <- function(blocknr) {

  for(i in 1:(length(blocknr) - 1) ){

    # load the first one
    file_1 <- paste0("./data/nonvalidated_tx/nonvaltx_", blocknr[i] ,".csv")
    temp_1 <- fread(file_1, stringsAsFactors = FALSE, data.table = FALSE)

    # load the second one
    file_2 <- paste0("./data/nonvalidated_tx/nonvaltx_", blocknr[i] + 1 ,".csv")
    temp_2 <- fread(file_2, stringsAsFactors = FALSE, data.table = FALSE)

    # delete the tx in the first one which are also in the second one
    # get indices of tx in file_1 which are also in file2
    ind <- temp_1[,1] %in% temp_2[,1]
    temp_1 <- temp_1[!ind, ]

    # save data
    filename <- paste0("./data/nonvalidated_tx_no_duplicates/nonvaltx_", blocknr[i] ,".csv")
    fwrite(x = temp_1,file = filename, row.names = FALSE, dec = ",", sep = ";")

    print(i)
  }
  # need to save the last one manually
  file_1 <- paste0("./data/nonvalidated_tx/nonvaltx_", blocknr[length(blocknr)] ,".csv")
  temp_1 <- fread(file_1, stringsAsFactors = FALSE, data.table = FALSE)

  filename <- paste0("./data/nonvalidated_tx_no_duplicates/nonvaltx_", blocknr[length(blocknr)] ,".csv")
  fwrite(x = temp_1,file = filename, row.names = FALSE, dec = ",", sep = ";")

  return(NULL)
}
