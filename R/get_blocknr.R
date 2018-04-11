#' get_latest_blocknr
#'
#' Calls the chainquery.com site and fetches the the number of the latest block of the blockchain
#'
#' @return a single number of the latest block
#' @export
#'
#' @examples
#' get_latest_blocknr()

get_latest_blocknr <- function() {

  # fetch raw data including
  # safety check because site url can change, depending of SSL-certificate.
  url1 <- "http://chainquery.com/bitcoin-api/getblockcount"
  url2 <- "https://chainquery.com/bitcoin-api/getblockcount"

  temp <- try(readLines(url1, warn = FALSE), silent = TRUE)

  if(class(temp) == "try-error"){
    rawdata <- readLines(url2, warn = FALSE)
  } else {
    rawdata <- temp
  }


  line <- rawdata[grepl("result", rawdata)]
  nline <- nchar(line)
  block_nr <- substr(line, start = (gregexpr(":", line)[[1]] + 2), stop = nline - 1)

  return(as.numeric(block_nr))
}

