#' get_valtx
#'
#' Create a .csv file for each blocknr/blockhash which contains the transaction_id and the blocknumber
#'
#' @param blocknr vector of blocknumbers
#' @param blockhash vector of the corresponding blockhashes
#' @param writecsv logical, should a .csv file be created
#' @param filename optional: the name of the file(s) to be created (has to be "xxx.csv")
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' blocknr <- 491464:491465
#' hash <- c("000000000000000000744807b0696371c148e33fc2b26d8bd1d4de304eda4681",
#' "0000000000000000003bf8b86a1238d87946e41da066bcafad9a3d93c65dac31")
#' get_valtx(blocknr = blocknr, blockhash = hash, writecsv = TRUE)
#' get_valtx(blocknr = blocknr, blockhash = hash, writecsv = TRUE, filename = c("a.csv", "b.csv"))
#' }

get_valtx <- function(blocknr, blockhash, writecsv = FALSE, filename){

  for(i in 1:length(blocknr)){

    # fetch raw data including
    # safety check because site url can change, depending of SSL-certificate.
    url1 <- paste0("http://chainquery.com/bitcoin-api/getblock/", blockhash[i],"/true")
    url2 <- paste0("https://chainquery.com/bitcoin-api/getblock/", blockhash[i],"/true")

    temp <- try(readLines(url1, warn = FALSE), silent = TRUE)

    if(class(temp) == "try-error"){
      rawdata <- readLines(url2, warn = FALSE)
    } else {
      rawdata <- temp
    }


    start_ind <- grep("merkleroot", rawdata)[1] + 2
    end_ind <- grep("\\t\\t],", rawdata)  # unique identifier when list of tx stop
    rawdata <- rawdata[start_ind:(end_ind - 1)]


    tx_df <- data.frame(transid = substr(rawdata, start = 10, stop = 73), blocknr = blocknr[i])


    # save data
    if(writecsv){
      if(missing(filename)){
        file_name <- paste0("./data/validated_tx/valtx_", blocknr[i],".csv")
      } else{
        file_name <- paste0("./data/validated_tx/", filename[i])
      }
      fwrite(tx_df, file = file_name, row.names = FALSE, dec = ",", sep = ";")

    }
    print(i)
  }

  return(NULL)
}
