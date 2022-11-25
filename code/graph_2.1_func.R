
graph_q2.1_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){
    library(tidyverse)

    df_q2.1_data <- SA_bonds %>% gather(Bonds, Yield, -date) %>%
        group_by(Bonds) %>%
        arrange(date) %>%
        filter(date >= as.Date("2000-01-01") & date <= as.Date("2022-12-31")) %>%
        ungroup()

#-------------------------------------------------------------------------------

    graph <- ggplot(df_q2.1_data) +

        geom_line(aes(x = date, y = Yield, color = Bonds), alpha = 0.8, size = 1) +

        #geom_bar(aes(x = reorder(location, value), y = value, colour = label, fill = label), stat = "identity") +
        #geom_text(aes(x = continent, y = value, label = value), vjust = 0) +
        #facet_wrap(~label, scales = "free_y", nrow = 2) +

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


