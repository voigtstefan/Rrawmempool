#' get_nonvaltx
#'
#' Creates csv files for the blocks in blocknr. In order to do so the rawmempool files and the validated_tx files have to exist, because the nonvalidated_tx are created by dismissing the validated_tx from the rawmempool
#'
#' @param blocknr vector of blocknr for which to create .csv files containing the nonvalidated
#' transactions
#' @param writecsv logical, should a .csv file be created
#' @param filename optional: the name of the file(s) to be created (has to be "xxx.csv")
#'
#' @return NULL
#' @export
#'
#' @examples
#'\dontrun{
#'get_nonvaltx(491464:491465, writecsv = TRUE)
#'}

get_nonvaltx <- function(blocknr, writecsv = FALSE, filename){

  for(i in 1:length(blocknr)){

    file_raw <- paste0("./data/rawmempool/rawmempool_", blocknr[i] ,".csv")
    temp_raw <- fread(file_raw, stringsAsFactors = FALSE, data.table = FALSE)

    file_val <- paste0("./data/validated_tx/valtx_", blocknr[i] ,".csv")
    temp_val <- fread(file_val, stringsAsFactors = FALSE, data.table = FALSE)

    # only keep NONexecuted transactions
    # drop  = FALSE because I only have one column left
    temp_raw <- temp_raw[!(temp_raw$transid %in% temp_val$transid), 1, drop = FALSE]

    # need to check if nonvalidated tx exist, otherwise i get an error after the if
    bool <- nrow(temp_raw) > 0  # nonvalidatex tx exist
    if(bool){
      temp_raw$blocknr <- blocknr[i]
    }

    # save data
    if(writecsv){
      if(missing(filename)){
        file_name <- paste0("./data/nonvalidated_tx/nonvaltx_", blocknr[i],".csv")
      } else{
        file_name <- paste0("./data/nonvalidated_tx/", filename[i])
      }
      fwrite(temp_raw, file = file_name, row.names = FALSE, dec = ",", sep = ";")

    }
    print(i)
  }

  return(NULL)
}
