#' fill_missing_rawmempool
#'
#' Since we are only taking a snapshot of the rawmempool every minute it can happpen
#' that two blocks get verified inbetween two snapshots, thus we are missing rawmempoolfiles.
#' To coutneract this we look at the rawmempool of the block before and dismiss the
#' validated transactions.
#'
#' @param blocknr sequence for which missing rawmempool files will be created
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' fill_missing_rawmempool(blocknr = 500000:501000)
#' }

fill_missing_rawmempool <- function(blocknr){

  # extract blocknr where data exists
  rmp_blocknr <- as.numeric(substr(list.files("./data/rawmempool"), start = 12, stop = 17))

  # calculate missing blocknr
  # setdiff = A / B
  missingdata_blocknr <- setdiff(blocknr, rmp_blocknr)

  if(length(missingdata_blocknr) == 0){
    cat("no files are missing. \n")
    return(NULL)
  }

  # create missing data
  for(i in missingdata_blocknr){

    #load the rmp_file and the validated_tx_file
    rmp_filename <- paste0("./data/rawmempool/rawmempool_", i - 1, ".csv")
    rmp_temp <- read.csv2(file = rmp_filename, header = TRUE, dec = ",", stringsAsFactors = FALSE)

    valtx_filename <- paste0("./data/validated_tx/valtx_", i - 1 ,".csv")
    valtx_temp <- read.csv2(file = valtx_filename, header = TRUE, dec = ",", stringsAsFactors = FALSE)

    # save the tx which were not validated as the rawmempool for the next block
    ind_savethese <- !(rmp_temp$transid %in% valtx_temp$transid)

    data <- rmp_temp[ind_savethese, ]

    file_name <- paste0("./data/rawmempool/rawmempool_", i ,".csv")
    write.csv2(data, file = file_name, row.names = FALSE)

  }
  cat(length(missingdata_blocknr), " files were created \n")
  return(NULL)

}
