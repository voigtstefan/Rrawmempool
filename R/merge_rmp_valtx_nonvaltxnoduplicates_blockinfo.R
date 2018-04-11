#' merge_rmp_valtx_nonvaltxnoduplicates_blockinfo
#'
#' Merges the infos of the rawmempool with the validated and nonvalidated transactions without dupicates and the blockinfo.
#' This means for the whole data.frame which consists out of many block each transaction will only be includes once.
#'
#' @param blocknr  vector of blocknumbers for which to generate the merged files.
#' @param blockhash vector of blockhashes corresponding to the blocknr.
#' @param writecsv logical, should a .csv file be created.
#' @param blockinfo data.frame, which contains data of the single blocks to be merged, created by get_blockinfo()
#'
#' @return returns a dataframe of the merged info where every transaction is included only once
#'
#' If writecsv = TRUE it creates a .csv with the following columns:blocknr, validated, time, height, fee, size, blocksize, blockweight, blocktime, difficulty, nr_tx_rmp, nr_tx_vtx, file_creation_time, nr_obs_valtx
#' @export
#'
#' @examples
#'\dontrun{
#'merge_rmp_valtx_nonvaltxnoduplicates_blockinfo(blocknr = blocknr,
#' blockhash = blockhash, writecsv = TRUE)
#'}

merge_rmp_valtx_nonvaltxnoduplicates_blockinfo <- function(blocknr, blockhash, blockinfo, writecsv = FALSE){

  l <- vector(mode = "list", length = length(blocknr))

  for(i in 1:length(blocknr)){

    # get rmp filename
    rmp_filename <- paste0("./data/rawmempool/rawmempool_", blocknr[i] ,".csv")
    # load rmp file
    rmp_file <- data.table::fread(file = rmp_filename,
                                  header = TRUE,
                                  stringsAsFactors = FALSE,
                                  data.table = FALSE)

    # get valtx filename
    valtx_filename <- paste0("./data/validated_tx/valtx_", blocknr[i] ,".csv")
    # load valtx file
    valtx_file <- data.table::fread(file = valtx_filename,
                                    header = TRUE,
                                    stringsAsFactors = FALSE,
                                    data.table = FALSE)
    if(nrow(valtx_file) > 0) {
      # validated 1 = validated
      valtx_file$validated <- 1
    }


    # get valtx filename
    nonvaltx_filename <- paste0("./data/nonvalidated_tx_no_duplicates/nonvaltx_", blocknr[i] ,".csv")
    # load valtx file
    nonvaltx_file <- data.table::fread(file = nonvaltx_filename,
                                       header = TRUE,
                                       stringsAsFactors = FALSE,
                                       data.table = FALSE)
    if(nrow(nonvaltx_file) > 0){
      # validated 0 = not validated
      nonvaltx_file$validated <- 0
    }

    # combine both files
    temp_val_nonval <- rbind(valtx_file, nonvaltx_file)

    # merge rmp with before
    # we do not need transid (column 1) anymore
    # saves a lot of memory
    merged_rmp_val_nonval <- merge(x = temp_val_nonval, y = rmp_file, by = "transid")[-1]

    # merge blockinfo with before and delete blocktime_alt
    l[[i]] <- merge(x = merged_rmp_val_nonval, y = blockinfo, by = "blocknr")[,-13]

    cat(i, blocknr[i], "\n")
  }

  df <- do.call(what = rbind, l)

  # save the df as .csv file
  filename <- paste0("./data/blockdata" ,".csv")
  data.table::fwrite(x = df, file = filename, dec = ",", sep = ";")

  return(df)
}
