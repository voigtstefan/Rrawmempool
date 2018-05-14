#' get_valtx
#'
#' Create a .csv file for each blocknr which contains the transaction_id and the blocknumber
#'
#' @param blocknr vector of blocknumbers
#' @param writecsv logical, should a .csv file be created
#'
#' @return NULL
#' @export

get_valtx <- function(blocknr, writecsv = FALSE){

  for(i in 1:length(blocknr)){
      
      tryCatch({
          hash <- get_block_hash(blocknr[i])
          tmp <- get_block_data(hash)
          tx <- tmp$tx
          tx <- tx[,-c(2,3,5,7,8,9)]
          tx$height <- tmp$height
          tx$verification_time <-  tmp$time
          
          # save data
          if(writecsv){
              file_name <- paste0("./data/validated_tx/valtx_", blocknr[i],".csv")
              write.csv2(tx, 
                         file = file_name, 
                         row.names = FALSE)
          }
      })
      return(tx)
    }
  return(NULL)
}
