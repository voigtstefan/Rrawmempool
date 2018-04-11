
<!-- README.md is generated from README.Rmd. Please edit that file -->
Basic Workflow to create data yourself from rawmempool data
===========================================================

``` r
#  install package
devtools::install_github("thomas-hillebrand/bcdataprep")

# load the package
library(bcdataprep)

# setup necessary folders, no arguments needed
setup_folder()


# IMPORTANT!
# now copy the rawmempooldata into the rawmempool folder which was just created



# either manually input blocknr for which data should be calculated or
# check the blockrange of the rawmempool files automatically
list_filenames <- list.files("./data/rawmempool/")
file_range <- range(as.numeric(substr(list_filenames, start = 12, stop = 17)))

# generate sequence of blocknumbers
blocknr <- file_range[1]:file_range[2]

# generate blockhashes.csv file  ------------------------------------------
blockhash_df <- blocknr_to_hash(blocknr = blocknr, writecsv = TRUE)

# if they are already generated load them
# blockhash_df <- read.csv2("./data/blockhash.csv", stringsAsFactors = FALSE)

# extract the hashes into a vector for future use
blockhash <- blockhash_df[,2]

# generate validated tx files ---------------------------------------------
get_valtx(blocknr = blocknr, blockhash = blockhash, writecsv = TRUE)

# fill missing rawmempool files -------------------------------------------
fill_missing_rawmempool(blocknr = blocknr)

# generate nonvalidated tx ------------------------------------------------
get_nonvaltx(blocknr = blocknr, writecsv = TRUE)

# generate nonvalidated tx with no duplicates -----------------------------
eliminate_duplicates_nonvaltx(blocknr = blocknr)

# generate blockinfo.csv file ---------------------------------------------
blockinfo <- get_blockinfo(blocknr = blocknr, blockhash = blockhash, writecsv = TRUE)

# if it is already generated load it
# blockinfo <- read.csv2("./data/blockinfo.csv", stringsAsFactors = FALSE)

# merge data on per block basis -------------------------------------------
merge_rmp_valtx_nonvaltx_blockinfo(blocknr = blocknr, blockhash = blockhash, blockinfo = blockinfo, writecsv = TRUE)

# merge data not on per block basis ---------------------------------------
merge_rmp_valtx_nonvaltxnoduplicates_blockinfo(blocknr = blocknr, blockhash = blockhash, blockinfo = blockinfo, writecsv = TRUE)
```
