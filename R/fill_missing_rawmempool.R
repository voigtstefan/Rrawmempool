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

fill_missing_rawmempool <- function(){

  # extract blocknr where data exists
  rmp_blocknr <- na.omit(as.numeric(substr(list.files(pattern='rawmempool_.*.csv'), start = 12, stop = 17)))
  # calculate missing blocknr
  missingdata_blocknr <- setdiff(min(rmp_blocknr):max(rmp_blocknr), rmp_blocknr)

  if(length(missingdata_blocknr) == 0){
    cat("no files are missing. \n")
    return(NULL)
  }

  # create missing data
  for(i in missingdata_blocknr){

    #load the rmp_file and the validated_tx_file
    rmp_filename <- paste0("rawmempool_", i - 1, ".csv")
    rmp_temp <- read.csv(file = rmp_filename,
                         stringsAsFactors = FALSE)

    valtx_filename <- paste0("./data/validated_tx/valtx_", i  ,".csv")
    valtx_temp <- read.csv2(file = valtx_filename, 
                            header = TRUE, 
                            dec = ",", 
                            stringsAsFactors = FALSE)

    # save the tx which were not validated as the rawmempool for the next block
    ind_savethese <- !(rmp_temp$id %in% valtx_temp$txid)

    data <- rmp_temp[ind_savethese, ]

    file_name <- paste0("rawmempool_", i ,".csv")
    write.csv2(data, file = file_name, row.names = FALSE)

  }
  return(NULL)

}
