
cum_returns_q3.5_func <- function(df_data, title, subtitle, caption, xlabel, ylabel) {

    library(tidyverse)
    library(tbl2xts)

    ##################
    #---ALSI (J200)---#
    ###################

    pacman::p_load(tidyverse);pacman::p_load(tbl2xts)

    # Let's first determine when we need to rebalance (go through my code here at least 5 times to understand all the nuances, especially the grouping logic):
    Rebalance_Days <- RebDays %>%
        filter(Date_Type == "Reb Trade Day") %>%
        pull(date)

    # Rights, so with our data and rebalancing dates in place, let’s create a capped weights dataframe to use for rebalancing purposes:

    # check for missing values
    colSums(is.na(T40_imputed))

    rebalance_col_J200 <-
        T40_imputed %>% ###-----J200 WEIGHTS-----###
        mutate(weight = J200) %>%
        select(date, Tickers, Sector, Return, weight) %>%
        filter(date %in% Rebalance_Days) %>%
        # Now we have to distinguish rebalances - to create something to group by:
        mutate(RebalanceTime = format(date, "%Y%B")) %>%
        # Now we can group...
        group_by(RebalanceTime) %>%
        # Now trim down to 30 stocks and reweight so sum(w)=1
        arrange(desc(weight)) %>%
        top_n(40, weight) %>%
        mutate(weight = weight/sum(weight)) %>%
        ungroup() %>%
        arrange(date) %>%
        select(-Sector, -Return)

    #TEST for NAs
    colSums(is.na(rebalance_col))
    #TEST that weights per rebalancing day add up to one
    # rebalance_col %>% arrange(date) %>% head(40) %>%
    #     mutate(sum = sum(weight))

    #-------------------------------------------------------------------------------
    # Now, to map this across all the dates, we can use purrr::map_df as follows:
    Capped_df_10percent_J200 <-
        rebalance_col_J200 %>%
        # Split our df into groups (where the groups here are the rebalance dates:
        group_split(RebalanceTime) %>%
        # Apply the function Proportional_Cap_Foo to each rebalancing date:
        map_df(~Proportional_Cap_Func(., W_Cap = 0.101) ) %>% select(-RebalanceTime)

    #------------------------------------------
    wts_J200 <-
        Capped_df_10percent_J200 %>%
        tbl_xts(cols_to_xts = weight, spread_by = Tickers)


    T40_USE <- T40_imputed %>% ###-----J200 WEIGHTS-----###
        mutate(return = Return, weight = J200) %>%
        select(date, Tickers, Sector, return, weight)

    rts <-
        T40_USE %>%

        filter(Tickers %in% unique(Capped_df_10percent_J200$Tickers) ) %>%

        tbl_xts(cols_to_xts = return, spread_by = Tickers)

    wts_J200[is.na(wts_J200)] <- 0

    rts[is.na(rts)] <- 0

    Idx_J200 <-
        rmsfuns::Safe_Return.portfolio(R = rts, weights = wts_J200, geometric = T) %>%

        # Let's make this a tibble:
        xts_tbl() %>%

        rename(capped_Idx_J200 = portfolio.returns)



    df_q3.5a_data <- Idx_J200 %>% mutate(cumulative_returns_J200 = cumprod(1+capped_Idx_J200))

#-------------------------------------------------------------------------------

    ##################
    #---SWIX (J400)---#
    ###################

    pacman::p_load(tidyverse);pacman::p_load(tbl2xts)

    # Let's first determine when we need to rebalance (go through my code here at least 5 times to understand all the nuances, especially the grouping logic):
    Rebalance_Days <- RebDays %>%
        filter(Date_Type == "Reb Trade Day") %>%
        pull(date)

    # Rights, so with our data and rebalancing dates in place, let’s create a capped weights dataframe to use for rebalancing purposes:

    # check for missing values
    colSums(is.na(T40_imputed))

    rebalance_col_J400 <-
        T40_imputed %>% ###-----J400 WEIGHTS-----###
        mutate(weight = J400) %>%
        select(date, Tickers, Sector, Return, weight) %>%
        filter(date %in% Rebalance_Days) %>%
        # Now we have to distinguish rebalances - to create something to group by:
        mutate(RebalanceTime = format(date, "%Y%B")) %>%
        # Now we can group...
        group_by(RebalanceTime) %>%
        # Now trim down to 30 stocks and reweight so sum(w)=1
        arrange(desc(weight)) %>%
        top_n(40, weight) %>%
        mutate(weight = weight/sum(weight)) %>%
        ungroup() %>%
        arrange(date) %>%
        select(-Sector, -Return)

    #TEST for NAs
    colSums(is.na(rebalance_col))
    #TEST that weights per rebalancing day add up to one
    # rebalance_col %>% arrange(date) %>% head(40) %>%
    #     mutate(sum = sum(Weight))

    #-------------------------------------------------------------------------------
    # Now, to map this across all the dates, we can use purrr::map_df as follows:
    Capped_df_6percent_J400 <-
        rebalance_col_J400 %>%
        # Split our df into groups (where the groups here are the rebalance dates:
        group_split(RebalanceTime) %>%
        # Apply the function Proportional_Cap_Foo to each rebalancing date:
        map_df(~Proportional_Cap_Func(., W_Cap = 0.6) ) %>% select(-RebalanceTime)

    #------------------------------------------
    wts_J400 <-
        Capped_df_6percent_J400 %>%
        tbl_xts(cols_to_xts = weight, spread_by = Tickers)


    T40_USE <- T40_imputed %>% ###-----J400 WEIGHTS-----###
        mutate(return = Return, weight = J400) %>%
        select(date, Tickers, Sector, return, weight)

    rts <-
        T40_USE %>%

        filter(Tickers %in% unique(Capped_df_6percent_J400$Tickers) ) %>%

        tbl_xts(cols_to_xts = return, spread_by = Tickers)

    wts_J400[is.na(wts_J400)] <- 0

    rts[is.na(rts)] <- 0

    Idx_J400 <-
        rmsfuns::Safe_Return.portfolio(R = rts, weights = wts_J400, geometric = T) %>%

        # Let's make this a tibble:
        xts_tbl() %>%

        rename(capped_Idx_J400 = portfolio.returns)



    df_q3.5b_data <- Idx_J400 %>% mutate(cumulative_returns_J400 = cumprod(1+capped_Idx_J400))
#-----------------------------
    #combine DATA
#-----------------------------

    #left_join(weighted_returns_J400, weighted_returns_J200, by = "date")

df_q3.5_data <- left_join(df_q3.5a_data,
                            df_q3.5b_data,
                              by = "date") %>%
                    select(date, cumulative_returns_J200, cumulative_returns_J400) %>%
                    gather(Name, Value, -date)


graph <- ggplot(df_q3.5_data) +
        geom_line(aes(x = date, y = Value, color = Name), alpha = 0.8,
                  size = 0.5) +

        scale_color_manual(values=c('black', 'red')) +

        #facet_wrap(~Name, scales = "free_y", nrow = 3) +

        #geom_bar(aes(x = reorder(location, value), y = value, colour = label, fill = label), stat = "identity") +
        #geom_text(aes(x = continent, y = value, label = value), vjust = 0) +

        labs(title = title,
             subtitle = subtitle,
             caption = caption,
             x = xlabel,
             y = ylabel) +

        theme() +
        theme(axis.text.x = element_text(angle = 0, hjust = -1)) +
        theme(legend.position="bottom") +
        theme(legend.title = element_blank())


    graph


}



