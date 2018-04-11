#' get_rawmempool
#'
#' Calls the chainquery.com site and makes a snapshot of the transactions of the rawmempool.
#'
#' Since it needs to download a lot of data this function can take up to 15 sec. to run.
#' A folder named "data" must exist in the current work directory if writecsv is set to TRUE.
#' The default name is rawmempool_####.csv where #### is the next blocknumber.
#' By default it overwrites itself if there is already a .csv file of the transaction for the next block.
#' This guarantees that only one file per block exists which is the most recent one.
#'
#' @param writecsv logical, should a .csv file be created
#' @param filename optional: the name of the file to be created (has to be "xxx.csv")
#'
#' @return a data.frame with size, fee, time, height, and transactionid or NULL if a .csv file was saved.
#' @export
#'
#' @import utils
#'
#' @examples
#' \dontrun{
#' get_rawmempool()
#' get_rawmempool(writecsv = TRUE)
#' get_rawmempool(writecsv = TRUE, filename = "snapshot01.csv")
#' }

get_rawmempool <- function(writecsv = FALSE, filename) {

  # fetch raw data including
  # safety check because site url can change, depending of SSL-certificate.
  url1 <- "http://chainquery.com/bitcoin-api/getrawmempool/true"
  url2 <- "https://chainquery.com/bitcoin-api/getrawmempool/true"

  temp <- try(readLines(url1, warn = FALSE), silent = TRUE)

  if(class(temp) == "try-error"){
    rawdata <- readLines(url2, warn = FALSE)
  } else {
    rawdata <- temp
  }

  # the transactions are for the next block -> + 1
  blocknr <- get_latest_blocknr() + 1

  # result indicator for start of data
  ind_result <- grep(pattern = "result", x = head(rawdata, 150))  # 119 should stay the same every time
  # error indicator for end of data
  ind_error <- grep(pattern = "error", x = rawdata)  # will be different every time

  # data starts here
  ind_data_start <- ind_result + 1
  # data ends here
  ind_data_end <- ind_error - 2

  # transactions do not have a fixed nr. of lines
  rawdata <- rawdata[ind_data_start:ind_data_end]

  # grab the indices of variables
  data_size_ind <- grep(";size", rawdata)
  # only searching for ;fee is not enough, bc sometimes the hashes start with "fee" (hexadecimal values)
  data_fee_ind  <- grep(";fee&quot;", rawdata)
  data_time_ind <- grep(";time", rawdata)
  data_transid_ind <- data_size_ind - 1
  data_height_ind <- data_time_ind + 1

  # grab the relevant strings
  data_transid <- rawdata[data_transid_ind]  # always the line before
  data_size <- rawdata[data_size_ind]
  data_fee  <- rawdata[data_fee_ind]
  data_time <- rawdata[data_time_ind]
  data_height <- rawdata[data_height_ind]

  # where does the number start
  str_start_size <-   unlist(gregexpr(pattern = ":", data_size)) + 2  # not always 22
  str_start_fee  <-   unlist(gregexpr(pattern = ":", data_fee))  + 2  # not always 21
  str_start_time <-   unlist(gregexpr(pattern = ":", data_time)) + 2  # not always 22
  str_start_height <- unlist(gregexpr(pattern = ":", data_height)) + 2

  # where does the number end
  str_end_size <- unlist(lapply(data_size, nchar)) - 1
  str_end_fee  <- unlist(lapply(data_fee, nchar))  - 1
  str_end_time <- unlist(lapply(data_time, nchar)) - 1
  str_end_height <- unlist(lapply(data_height, nchar)) - 1

  # split the string and grab the data
  size <- as.numeric(substr(x = data_size, start = str_start_size, stop = str_end_size))
  fee  <- as.numeric(substr(x = data_fee , start = str_start_fee,  stop = str_end_fee))
  time <- as.numeric(substr(x = data_time, start = str_start_time, stop = str_end_time))
  transid <- substr(data_transid, start = 9, stop = 72)
  height <- as.numeric(substr(x = data_height, start = str_start_height, stop = str_end_height))

  # for better readability when saving the data
  # maybe reset after function call
  options(scipen = 10000)

  # crate final data.frame
  df <- data.frame(transid, time, height, fee, size, stringsAsFactors = FALSE)


  # saving the data
  if(writecsv){

    if(missing(filename)){
      file_name <- paste0("./data/rawmempool_", blocknr,".csv")
    } else{
      file_name <- paste0("./data/", filename)
    }
    write.csv2(df, file = file_name, row.names = FALSE)

    return(NULL)
  }

  # only if file was not saved on disk.
  return(df)
}

