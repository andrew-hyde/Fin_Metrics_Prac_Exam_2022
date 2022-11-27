# wrap in a function to neaten it up
q6_mv_garch_func <- function(df_data){

    DCCPre <- dccPre(xts_q6_data_combined_use, include.mean = T, p = 0)

    # estimates of volatility for each series.
    Vol <- DCCPre$marVol
    colnames(Vol) <- colnames(xts_q6_data_combined_use)

    Vol <- data.frame(cbind(date = index(xts_q6_data_combined_use), Vol)) %>%
        # Add date column
        mutate(date = as.Date(date)) %>%  tbl_df()


    # volatility
    Vol

}
