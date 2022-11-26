
graph_q3.2_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){

    library(tidyverse)
    library(tbl2xts)

# create a xts of returns for each equity over time
T40_return <- df_data %>%
    filter(Sector == "Financials") %>%
    select(date, Tickers, Return) %>%
        tbl_xts(., cols_to_xts = Return, spread_by = Tickers)

#-------------------------------------------------------------------------------
# J400 weights
# create a xts of weights for each equity over time
weights_J400 <- df_data %>%
    filter(Sector == "Financials") %>%
    select(date, Tickers, J400) %>%
    arrange(date) %>% group_by(Tickers) %>%
    tbl_xts(cols_to_xts = J400, spread_by = Tickers)

# replace NA in J400 weight column with zero
weights_J400[is.na(weights_J400)] <- 0

# calc returns
returns_J400 <-
    rmsfuns::Safe_Return.portfolio(R = T40_return, weights = weights_J400, geometric = T) %>%
    xts_tbl() %>%
    rename(port_returns_J400 = portfolio.returns)

# calc cumulative returns of the J200 weighted porfolio
weighted_returns_J400 <- returns_J400 %>% mutate(cumulative_returns_J400 = cumprod(1+port_returns_J400))

#-------------------------------------------------------------------------------
# J200 weights
# create a xts of weights for each equity over time
weights_J200 <- df_data %>%
        filter(Sector == "Financials") %>%
        select(date, Tickers, J200) %>%
        arrange(date) %>% group_by(Tickers) %>%
        tbl_xts(cols_to_xts = J200, spread_by = Tickers)

# replace NA in J400 weight column with zero
weights_J200[is.na(weights_J200)] <- 0

# calc returns
returns_J200 <-
    rmsfuns::Safe_Return.portfolio(R = T40_return, weights = weights_J200, geometric = T) %>%
    xts_tbl() %>%
    rename(port_returns_J200 = portfolio.returns)

# calc cumulative returns of the J200 weighted porfolio
weighted_returns_J200 <- returns_J200 %>% mutate(cumulative_returns_J200 = cumprod(1+port_returns_J200))

#-------------------------------------------------------------------------------

#left_join(weighted_returns_J400, weighted_returns_J200, by = "date")

df_q3.2_data <- left_join(returns_J400 %>% mutate(cumulative_returns_J400 = cumprod(1+port_returns_J400)),
                        returns_J200 %>% mutate(cumulative_returns_J200 = cumprod(1+port_returns_J200)),
                        by = "date")

df_q3.2_data_tidy <- df_q3.2_data %>%
    select(date, cumulative_returns_J400, cumulative_returns_J200) %>%
    gather(Name, Value, -date)

#-------------------------------------------------------------------------------


graph <-  ggplot(df_q3.2_data_tidy) +
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

