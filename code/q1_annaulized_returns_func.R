
q1.1_annualized_return_table <- function(df_data, title, subtitle, caption, xlabel, ylabel){


library(tbl2xts);library(PerformanceAnalytics);library(fmxdat)

#-------------------------------------------------------------------------------

q1_data_combined_xts <- tbl_xts(df_data,
                        cols_to_xts = ret, spread_by = Tickers)

dfplotxts <-

    bind_rows(
        # Don't annualize for less than a year, e.g.:
        q1_data_combined_xts  %>% tail(12) %>% PerformanceAnalytics::Return.annualized(., scale = 12) %>% data.frame() %>% mutate(Freq = "A"),

        q1_data_combined_xts  %>% tail(36) %>% PerformanceAnalytics::Return.annualized(., scale = 12) %>% data.frame() %>% mutate(Freq = "B"),

        q1_data_combined_xts  %>% tail(60) %>% PerformanceAnalytics::Return.annualized(., scale = 12) %>% data.frame() %>% mutate(Freq = "C"),

        q1_data_combined_xts  %>% tail(120) %>% PerformanceAnalytics::Return.annualized(., scale = 12) %>% data.frame() %>% mutate(Freq = "D"),

        q1_data_combined_xts  %>% tail(240) %>% PerformanceAnalytics::Return.annualized(., scale = 12) %>% data.frame() %>% mutate(Freq = "E"),



    ) %>% data.frame() %>% gather(Tickers, mu, -Freq) %>%
    mutate(Tickers = gsub("\\.", " ", Tickers))

#-------------------------------------------------------------------------------

# Barplot_foo:
to_string <- as_labeller(c(`A` = "1 Year", `B` = "3 Years", `C` = "5 Years", `D` = "10 Years", `E` = "20 Years"))

graph <-

    dfplotxts %>%

    # Compare to (they are the exact same):
    # dfplotxts %>%

    ggplot() +

    geom_bar( aes(Tickers, mu, fill = Tickers), stat="identity") +

    facet_wrap(~Freq, labeller = to_string, nrow = 1) +


    labs(title = title,
         caption = caption,
         x = xlabel,
         y = ylabel) +


    fmx_fills() +



    geom_label(aes(Tickers, mu, label = paste0( round(mu, 4)*100, "%" )), size = ggpts(8), alpha = 0.35, fontface = "bold", nudge_y = 0.002) +

    theme_fmx(CustomCaption = T, title.size = ggpts(43), subtitle.size = ggpts(38),
              caption.size = ggpts(30),

              axis.size = ggpts(37),

              legend.size = ggpts(35),legend.pos = "top") +

    theme(axis.text.x = element_blank(), axis.title.y = element_text(vjust=2)) +

    theme(strip.text.x = element_text(face = "bold", size = ggpts(35), margin = margin(.1, 0, .1, 0, "cm")))

graph

}


