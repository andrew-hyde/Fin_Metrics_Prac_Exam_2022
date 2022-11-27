
q1_rolling_returns_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){


#-------------------------------------------------------------------------------
    library(RcppRoll)
    library(lubridate)
    library(tidyverse)

    #SET Period to 36 for a s year period given that data is monthly
    PERIOD = 36

plotdf_roll <- q1_data_combined %>%
        group_by(Tickers) %>%
        # Epic sorcery:
        mutate(RollRets = RcppRoll::roll_prod(x = 1 + ret,
                                              n =  PERIOD,
                                              fill = NA,
                                              align = "right")^(12/PERIOD) - 1) %>%
        # Note this cool trick: it removes dates that have no

        # RollRets at all.
        group_by(date) %>%
        filter(any(!is.na(RollRets))) %>%
        ungroup()

#-------------------------------------------------------------------------------
    library(fmxdat)

    graph <- plotdf_roll %>% ggplot() +

        geom_line(aes(date, RollRets, color = Tickers), alpha = 0.7,
                  size = 1.25) +

    #theme_bw() +

    # guides(color = FALSE, fill = FALSE, alpha = FALSE) +

    # Add titles:
    labs(title = title,
         subtitle = subtitle,
         caption = caption,
         x = xlabel,
         y = ylabel) +

        #     theme() +
        #     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        #     theme(legend.position="bottom") +
        # theme(legend.title=element_blank())

        theme_fmx(title.size = ggpts(30),
                  subtitle.size = ggpts(5), caption.size = ggpts(25), CustomCaption = T) +

        fmx_cols()

    finplot(graph, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T,
            y.pct = T, y.pct_acc = 1)


    graph

}

