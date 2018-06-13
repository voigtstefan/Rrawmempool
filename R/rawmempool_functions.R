#' get_raw_mempool
#'
#' @return a data.frame with the blocknr and the blockhash256 character long hash of the blocknr
#' @importFrom jsonlite fromJSON
#' @export
get_raw_mempool <- function() jsonlite::fromJSON(system('bitcoin-cli getrawmempool true', intern=TRUE))

#' get_block_hash
#'
#' @param n block number
#' @return block hash
#' @export
get_block_hash <- function(n) system(paste0('bitcoin-cli getblockhash ', n), intern=TRUE)

#' get_block_data
#'
#' @param hash block hash
#' @return get_block_data
#' @importFrom jsonlite fromJSON
#' @export
get_block_data <- function(hash) jsonlite::fromJSON(system(paste0('bitcoin-cli getblock ', hash,' ', 2), intern=TRUE))

#' get_transaction_data
#'
#' @param tc_id transaction id
#' @return transaction_data
#' @importFrom jsonlite fromJSON
#' @export
get_transaction_data <- function(tc_id) jsonlite::fromJSON(system(paste0(' bitcoin-cli getrawtransaction ', tc_id,' ', 1), intern=TRUE))

#' get_time_of_last_block
#'
#' @return time_of_last_block
#' @importFrom jsonlite fromJSON
#' @export
get_time_of_last_block <- function() jsonlite::fromJSON(system(paste0('bitcoin-cli getblock ',get_block_hash(as.numeric(system('bitcoin-cli getblockcount', intern=TRUE)))),intern=TRUE))$time

#' get_fee_recommendation
#'
#' @param blocks required number of blocks
#' @importFrom jsonlite fromJSON
#' @return recommended fees
#' @export
get_fee_recommendation <- function(blocks) jsonlite::fromJSON(system(paste0('bitcoin-cli estimatesmartfee ', blocks), inter=TRUE))$feerate

#' get_block_count
#'
#' @return block count
#' @importFrom jsonlite fromJSON
#' @export
get_block_count <- function() as.numeric(system('bitcoin-cli getblockcount', intern=TRUE))

#' get_val_tx
#'
#' @return data.frame with validated transactions
#' @importFrom jsonlite fromJSON
#' @export
get_val_tx <- function(n){
    hash <- get_block_hash(n)
    data <- get_block_data(hash)
    data <- data.frame(data$tx)
    tx$height <- n
    tx$verification_time <- data$time
    return(tx)
} 
