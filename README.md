# Basic Workflow to create data yourself from rawmempool data

#  install package
devtools::install_github("voigtstefan/bcdataprep_node")

# load the package
library(Rrawmempool)

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

