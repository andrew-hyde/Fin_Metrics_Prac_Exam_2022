graph_q4.1_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){

    library(tidyverse)
    library(tbl2xts)

q4.1_data <- T40 %>% arrange(date) %>%
    group_by(Tickers) %>%
    select(date, Tickers, Return)


ggplot(q4.1_data) +

        geom_line(aes(x = date, y = Return, color = Tickers), alpha = 0.8,
                  size = 0.5) +

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
        theme(legend.position="") +
        theme(legend.title = element_blank())

}


