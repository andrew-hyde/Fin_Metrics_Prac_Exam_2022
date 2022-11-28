
q6_nested_graph_function <- function(df_data) {


################################################################################

# wrap in a function to neaten it up
q6_mv_garch_func <- function(df_data){


#-------------------------------------------------------------------------------
    # wrap in a function to neaten it up
    q6_data_cleaning_func <- function(df_data){

        # struggling to get results inside of 'left_join'
        # do it individually
        msci <- msci %>% mutate(Return = log(Price) - log(lag(Price))) %>% select(-Price)
        comms <- comms %>% mutate(Return = log(Price) - log(lag(Price))) %>% select(-Price)
        bonds <- bonds %>% mutate(Return = log(Bond_10Yr) - log(lag(Bond_10Yr))) %>% select(-Bond_10Yr)

        # this works if calc individually firms
        q6_data_combined <- left_join(msci %>%
                                          spread(Name, Return) %>%
                                          select(date, MSCI_ACWI, MSCI_RE),

                                      bonds %>% spread(Name, Return) %>% select(date, US_10Yr),
                                      by = "date") %>%

            left_join(.,
                      comms %>% spread(Name, Return) %>% select(date, Bcom_Index),
                      by = "date" )

        # this works if calc individually firms
        library(lubridate)
        q6_data_combined_use <-
            q6_data_combined %>%
            gather(Asset, Return, -date) %>%
            arrange(date) %>%
            group_by(Asset) %>%
            mutate(scaled_ret = Return - mean(Return, na.rm = T)) %>%
            # filter(Name %in% c("MSCI_ACWI", "MSCI_RE", "MSCI_USA", "MSCI_USREIT")) %>%
            filter(date >= as.Date("2012-01-01") & date <= as.Date("2022-12-31")) %>%
            filter(date > dplyr::first(date)) %>%
            select(-Return)


        library(tbl2xts)
        library(rugarch)
        xts_q6_data_combined <-
            q6_data_combined_use %>% tbl2xts::tbl_xts(., cols_to_xts = "scaled_ret", spread_by = "Asset")

        #MTS::MarchTest(xts_q5_data_combined)

        xts_q6_data_combined
    }

# result
xts_q6_data_combined_use <- q6_data_cleaning_func(df_data)

#--------------------------------------------------------------------------------


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

# result
Vol <- q6_mv_garch_func(df_data)

################################################################################

# use 'Vol' in the volatility plot
ggplot(Vol %>% gather(Asset, Sigma, -date)) +
    geom_line(aes(x = date, y = Sigma, colour = Asset)) +

    labs(title = "Volatility of Returns for the past decade",
         subtitle = "Different Asset Classes",
         caption = "Commodities, Equities, Real Estate and Bonds",
         x = "",
         y = "Sigma") +

    fmxdat::theme_fmx(legend.pos = "bottom")


}


