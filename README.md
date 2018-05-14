# Rrawmempool

A simple **R** tool to extract information from a running Bitcoin node.

Install the package by running

    devtools:install_github('voigtstefan/Rrawmempool')
    library(Rrawmempool)
    
Functionalities include (among others):

- Get current raw mempool `get_raw_mempool()`
- Get block hash `get_block_hash(n)`
- Get block data `get_block_data(hash)`
- Get transaction data `get_transaction_data(id)`
- Get time of last block `get_time_of_last_block()`
- Get fee recommendation `get_fee_recommendation()`
- Get block count `get_block_count()`
- Get validated transactions from a block `get_valtx(n)`

The package is actively extended in the moment, comments and questions are more than welcome!
