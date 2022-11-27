
weights_graph_q3.7_func <- function(df_data, title, subtitle, caption, xlabel, ylabel) {


#-------------------------------------------------------------------------------
library(tidyverse)
library(tbl2xts)

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
#colSums(is.na(T40_imputed))

rebalance_col_J400 <-
    T40_imputed %>% ###-----J200 WEIGHTS-----###
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
    group_by(date, Sector) %>%
    select(date, Sector, weight) %>%
    summarise(Weight = sum(weight))
    #filter(date > first(date))



#######################

graph <- rebalance_col_J400 %>% ggplot() +

    geom_bar(aes(x =date, y = Weight, colour = Sector, fill = Sector), stat = "identity") +

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


