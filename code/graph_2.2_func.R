
graph_q2.2_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){


# join the data: USA bonds, SA bonds, exchange rate, Break-even inflation estimate and VIX
    q2_data_combined <- left_join(
        # JOIN US DATA
        bonds_10y %>% group_by(Name) %>% filter(Name == "US_10Yr") %>% ungroup() %>% select(-Name),
        bonds_2y %>% group_by(Name) %>% filter(Name == "US_2yr") %>% ungroup() %>% select(-Name),
        by = "date") %>%

        # JOIN SA DATA
        left_join(.,
                  SA_bonds %>% select(-SA_3M),
                  by = "date") %>%
        # JOIN THE VIX
        left_join(.,
                  IV %>% mutate(VIX = Price) %>% group_by(Name) %>% filter(Name == "VIX")
                  %>% ungroup() %>% select(-Name, -Price),
                  by = "date") %>%

        # JOIN US ZAR Exchnage Rate
        left_join(.,
                  usdzar %>% mutate(USDZAR = Price) %>% ungroup() %>% select(-Name, -Price),
                  by = "date") %>%

        # JOIN THE inflation data
        left_join(.,
                  BE_Infl %>% mutate(Infl_est = Price) %>% ungroup() %>% select(-Name, -Price),
                  by = "date")

#-------------------------------------------------------------------------------

    df_q2.2_data <- q2_data_combined %>%
        arrange(date) %>%
        filter(date >= as.Date("2018-01-01") & date <= as.Date("2022-12-31")) %>%
        mutate(SA_10Yr_2Yr = ZA_10Yr - ZA_2Yr) %>%
        mutate(USA_10Yr_2Yr = Bond_10Yr - Bond_2Yr) %>%
        select(date, USA_10Yr_2Yr, SA_10Yr_2Yr, USDZAR) %>%
        gather(Name, Value, -date)


#-------------------------------------------------------------------------------


graph <-  ggplot(df_q2.2_data) +
        geom_line(aes(x = date, y = Value, color = Name), alpha = 0.8,
                  size = 1.2) +

        facet_wrap(~Name, scales = "free_y", nrow = 3) +

        #geom_bar(aes(x = reorder(location, value), y = value, colour = label, fill = label), stat = "identity") +
        #geom_text(aes(x = continent, y = value, label = value), vjust = 0) +

        labs(title = title,
             subtitle = subtitle,
             caption = caption,
             x = xlabel,
             y = ylabel) +

        theme() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(legend.position="bottom") +
        theme(legend.title = element_blank())


    graph

}


