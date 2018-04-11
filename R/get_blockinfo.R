#' get_blockinfo
#'
#' Fetch basic blockinfos fo a vector of blockhashes and optionally save them into a .csv file
#'
#' For eash blocknumber one line will be added to the data.frame. It contains the blocknumber,
#'  blocksize in bytes, blockweight, blocktime, difficulty, number of transactions
#'  in the rawmempool, number of verified transaction in this block and the time
#'
#' @param blocknr  vector of blocknumbers for which to generate the blockinfo file.
#' @param blockhash vector of blockhashes corresponding to the blocknr.
#' @param writecsv logical, should a .csv file be created.
#' @param filename optional: the name of the file to be created (has to be "xxx.csv").
#'
#' @return a data.frame with the infos of the requested blocks.
#' @export
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#' blocknr <- 499999
#' blockhash <- "0000000000000000007962066dcd6675830883516bcf40047d42740a85eb2919"
#' get_blockinfo(blocknr = blocknr, blockhash = blockhash)
#' get_blockinfo(blocknr = blocknr, blockhash = blockhash, writecsv = TRUE)
#' get_blockinfo(blocknr = blocknr, blockhash = blockhash, writecsv = TRUE, filename = "hashes.csv")
#' }

get_blockinfo <- function(blocknr, blockhash, writecsv = FALSE, filename) {


  # temporary list for results
  l <- vector(mode = "list", length = length(blockhash))

  for(i in 1:length(blockhash)){

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

    # use time and size as anchor points (size before tx, time after tx)
    ind_size <- grep("size", rawdata)[2]  # 1... strippedsize, 2... size 3/4... help
    ind_weight <- ind_size + 1
    ind_height <- ind_size + 2

    ind_time <- grep("time", rawdata)[1]
    ind_difficulty <- ind_time + 4


    rawdata_size <- rawdata[ind_size]
    rawdata_weight <- rawdata[ind_weight]
    rawdata_height <- rawdata[ind_height]

    rawdata_time <- rawdata[ind_time]
    rawdata_difficulty <- rawdata[ind_difficulty]

    size <- substr(rawdata_size, start = unlist(gregexpr(":", rawdata_size)) + 2, stop = nchar(rawdata_size)-1)
    weight <- substr(rawdata_weight, start = unlist(gregexpr(":", rawdata_weight)) + 2, stop = nchar(rawdata_weight)-1)
    height <- substr(rawdata_height, start = unlist(gregexpr(":", rawdata_height)) + 2, stop = nchar(rawdata_height)-1)
    time <- substr(rawdata_time, start = unlist(gregexpr(":", rawdata_time)) + 2, stop = nchar(rawdata_time)-1)
    difficulty <- substr(rawdata_difficulty,
                         start = unlist(gregexpr(":", rawdata_difficulty)) + 2,
                         stop = nchar(rawdata_difficulty)-1)

    l[[i]] <- as.numeric(c(height, size, weight, time, difficulty))
    print(i)
  }



  # turn list into df
  df <- do.call(what = rbind, l)
  colnames(df) <- c("blocknr", "blocksize", "blockweight", "blocktime", "difficulty")



  # generate filepaths of rmp files
  filenames_rmp <- paste0("./data/rawmempool/rawmempool_", blocknr,".csv")

  # load rmp files
  l_rmp <-  vector(mode = "list", length = length(filenames_rmp))
  for (i in 1:length(filenames_rmp)) {
    # only keep one column in order to save memory
    l_rmp[[i]] <- data.table::fread(file = filenames_rmp[i],
                                    header = TRUE,
                                    stringsAsFactors = FALSE,
                                    select = "transid",  # needed later
                                    data.table = FALSE)
    print(i) # think about deleting this
  }

  # calculate number of tx per block in rmp
  nr_tx_rmp <- sapply(l_rmp, nrow)




  # get file names valtx
  filenames_valtx <- paste0("./data/validated_tx/valtx_", blocknr,".csv")

  # load valtx files
  l_vtx <-  vector(mode = "list", length = length(filenames_valtx))
  for (i in 1:length(filenames_valtx)) {
    l_vtx[[i]] <- data.table::fread(file = filenames_valtx[i],
                                    header = TRUE,
                                    stringsAsFactors = FALSE,
                                    select = "transid",  # needed later
                                    data.table = FALSE)
    print(i) # think about deleting this
  }

  # calculate number of validated tx per block
  nr_tx_vtx <- sapply(l_vtx, nrow)


  # merge data
  # df is still a list
  df <- as.data.frame(df)

  df$nr_tx_rmp <- nr_tx_rmp
  df$nr_tx_vtx <- nr_tx_vtx
  df$blocktime_alt <- as.POSIXct(df$blocktime, origin = "1960-01-01")

  # get file creation times
  df$file_creation_time <- unclass(as.POSIXct(file.mtime(filenames_rmp)))

  # watch out for files manualy created at a later point because we got no record for it.
  # have to do some data cleaning
  # for now i just substract 1 second from the next block which gives us
  # at worst a time which is off by 59 seconds but all waiting times should
  # be positive.

  ind <- which(diff(df$file_creation_time) <= 0)

  # need to go backwards and more than one in case more than one block
  # is unobservedand got created manually and

  while(length(ind) > 0){
    for (i in rev(ind)) {
      df$file_creation_time[i] <- df$file_creation_time[i + 1] -1
    }
    ind <- which(diff(df$file_creation_time) <= 0)
  }


  # calculate how many validated transactions I observed in absolute numbers
  temp_res <-  rep(0, length(blocknr))
  for(i in 1:length(blocknr)){
    temp_res[i] <- sum(l_vtx[[i]]$transid %in% l_rmp[[i]]$transid)
  }
  df$nr_obs_valtx <- temp_res


  # save data
  if(writecsv){
    if(missing(filename)){
      file_name <- paste0("./data/blockinfo.csv")
    } else{
      file_name <- paste0("./data/", filename)
    }
    write.csv2(df, file = file_name, row.names = FALSE)

  }

  return(df)
}
