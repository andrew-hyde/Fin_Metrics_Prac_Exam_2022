

Q5.1_graph_func_vol <- function(df_data, title, subtitle, caption, xlabel, ylabel){

    library(tidyverse)
    library(knitr)
    library(lubridate)

top_10_vol <-
    cncyIV %>%
    select(date, Name, Price) %>%
    mutate(Name = gsub("_IV","", cncyIV$Name)) %>% # remove characters
    group_by(Name) %>%
    summarise(across(everything(), mean)) %>%
    select(-date) %>%
    arrange(desc(Price)) %>%
    head(5) %>%
    pull(Name)


df_q5.1_cncyIV_graph_data <-
        cncyIV %>%
        select(date, Name, Price) %>%
    mutate(Name = gsub("_IV","", cncyIV$Name)) %>% # remove characters
    filter(Name %in% top_10_vol)


#-------------------------------------------------------------------------------

graph <- df_q5.1_cncyIV_graph_data %>%
        ggplot() +

        geom_line(aes(x = date, y = Price, color = Name), alpha = 0.8,
                  size = 1) +

        #geom_line(aes(x = date, y = value, color = label), alpha = 0.8,
        # size = 1) +

        #facet_wrap(~Name, scales = "free_y") +

        labs(title = title,
             subtitle = subtitle,
             caption = caption,
             y = ylabel,
             x = xlabel) +

        theme() +
        theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
        theme(legend.position="bottom")

    #scale_x_discrete(guide = guide_axis(n.dodge=1.2))

    graph

}

