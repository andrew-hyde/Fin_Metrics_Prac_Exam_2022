
graph_q2.3_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){


# join the data: SA bonds and Break-even inflation estimate
q2_data_sa_infl <-

        # JOIN SA DATA
        left_join(SA_bonds %>% select(-SA_3M),
                  BE_Infl %>% mutate(Infl_est = Price) %>% ungroup() %>% select(-Name, -Price),
                  by = "date") # JOIN THE inflation data


#-------------------------------------------------------------------------------


    df_q2.3_data <- q2_data_sa_infl %>%
        arrange(date) %>%
        filter(date >= as.Date("2019-01-01") & date <= as.Date("2022-12-31")) %>%
        select(date, ZA_2Yr, ZA_10Yr, Infl_est) %>%
        gather(Name, Value, -date)


#-------------------------------------------------------------------------------


graph <-  ggplot(df_q2.3_data) +
        geom_line(aes(x = date, y = Value, color = Name), alpha = 0.8,
                  size = 1.2) +

        #facet_wrap(~Name, scales = "free_y", nrow = 3) +

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


