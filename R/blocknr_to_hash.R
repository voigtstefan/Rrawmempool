#' blocknr_to_hash
#'
#' Gets the blockhash corresponding to a certain blocknr from the rawmempool 
#'
#' @param blocknr a vector of blocknumbers
#' @param writecsv logical, should a .csv file be created
#' @param filename optional: the name of the file to be created (has to be "xxx.csv")
#'
#' @return a data.frame with the blocknr and the blockhash256 character long hash of the blocknr
#' @export
#'
#' @examples
#' \dontrun{
#' blocknr_to_hash(blocknr = 500000:500010)
#' blocknr_to_hash(blocknr = 500000:500010, writecsv = TRUE)
#' blocknr_to_hash(blocknr = 500000:500010, writecsv = TRUE, filename = "hash.csv")
#' }

blocknr_to_hash <- function(blocknr, writecsv = FALSE, filename) {

  # needed because certain numbers (eg 500000) are represented as 5e+05 instead of 500000
  # which in gets us the hash of blocknr = 505 instead of 500000
  options(scipen = 10000)

  blockhash <- vector(mode = "character", length = length(blocknr))

  for(i in 1:length(blocknr)){

    # fetch raw data including
    # safety check because site url can change, depending of SSL-certificate.
    url1 <- paste0("http://chainquery.com/bitcoin-api/getblockhash/", blocknr[i])
    url2 <- paste0("https://chainquery.com/bitcoin-api/getblockhash/", blocknr[i])

    temp <- try(readLines(url1, warn = FALSE), silent = TRUE)

    if(class(temp) == "try-error"){
      rawdata <- readLines(url2, warn = FALSE)
    } else {
      rawdata <- temp
    }

    line <- rawdata[grepl("result&quot;: &quot;", rawdata)]
    nline <- nchar(line)
    blockhash[i] <- substr(line, start = (gregexpr(":", line)[[1]] + 8), stop = nline - 7)
    print(i)

  }

  df <- data.frame(blocknr = blocknr,
                   blockhash = blockhash,
                   stringsAsFactors = FALSE)

  if(writecsv){
    if(missing(filename)){
      filename <- "./data/blockhash.csv"
    }else{
      filename <- paste0("./data/", filename)
    }
    fwrite(x = df, file = filename, row.names = FALSE, dec = ",", sep = ";")
  }

  return(df)
}
