

Cum_Fee_Comparison_1.1 <- function(Asisa_funds, Fee = 0*1e-4, Start = ymd(20030101),
                               # Added purely for figure adjustment:
                               Gap = 3, Lvlset = 5,
                               mnthfwd = 18){


library(tidyverse);library(lubridate)

    Asisa_funds <- ASISA_imputed_data %>% gather(Name, Return, -date) %>%
        arrange(date) %>%
        group_by(date) %>%
        mutate(Returns = mean(Return)) %>% # Name ave returns Asisa for left_join
        select(date, Returns) %>%
        unique()

    # Turn annual fee into monthly compounded:
    # Remember, we want to find x so that its compound is 10 bps, or: (1+x)^12 = 10*1e-4
    feeconverter <- function(x, Ann_Level) (1+x)^(1/Ann_Level)-1

    df_p <-
        Asisa_funds %>% filter(date > Start) %>%
        mutate('Return - 0 bps' = Returns - feeconverter(0*1e-4, Ann_Level = 12)) %>%
        mutate('Return - 100 bps' = Returns - feeconverter(100*1e-4, Ann_Level = 12)) %>%
        mutate('Return - 200 bps' = Returns - feeconverter(200*1e-4, Ann_Level = 12)) %>%

        rename(Gross = Returns) %>%
        gather(Type, Rets, -date, -Gross) %>%
        group_by(Type) %>% filter(date > first(date)) %>%
        mutate(CP = cumprod(1+Rets)) %>%
        mutate(Gross = cumprod(1+Gross))

    Ord <- c('Return - 0 bps', 'Return - 100 bps', 'Return - 200 bps')

    Txt <-
        df_p %>% filter(date == last(date)) %>%
        mutate(date = date %m+% months(Gap)) %>%
        mutate(Text = paste0(round(CP/Gross-1, 3)*100, "%")) %>%
        # Order:
        mutate(Type = as.factor(Type)) %>%
        mutate(Type = forcats::fct_relevel(Type, Ord))

    Shock200 <-
        df_p %>% filter(date == last(date)) %>%
        mutate(date = date %m+% months(Gap)) %>%
        filter(Type == 'Return - 200 bps') %>% pull(CP)
    Shock100 <-
        df_p %>% filter(date == last(date)) %>%
        mutate(date = date %m+% months(Gap)) %>%
        filter(Type == 'Return - 100 bps') %>% pull(CP)
    Shock0 <-
        df_p %>% filter(date == last(date)) %>%
        mutate(date = date %m+% months(Gap)) %>%
        filter(Type == 'Return - 0 bps') %>% pull(CP)

    Msg <-
        df_p %>% ungroup() %>% filter(date > first(date) %m+% months(mnthfwd)) %>% slice(1) %>%
        mutate(Lvl = Lvlset) %>% mutate(Msg = glue::glue("R1m invested in {format(Start, '%B %Y')}\n* 0bps: R{round(Shock0, 2)}m\n* 100bps: R{round(Shock100, 2)}m\n* 200bps: R{round(Shock200, 2)}m"))

    graph <-
        df_p %>%
        mutate(Type = as.factor(Type)) %>%
        mutate(Type = forcats::fct_relevel(Type, Ord)) %>%


        ggplot() +
        geom_line(aes(date, Gross), color = "black", size = 1, alpha = 0.95) +
        geom_line(aes(date, CP, color = Type), size = 1, alpha = 0.6) +
        # ggrepel::geom_label_repel(data = Txt, aes(date, CP, label = Text ), hjust = 0, color = "darkred", alpha = 0.25, size = 3) +
        geom_text(data = Txt, aes(date, CP, label = Text ), hjust = 0, color = "darkred", size = 3.3) +
        theme_bw() +

        geom_label(data = Msg, aes(date, Lvl, label = Msg), color = "darkred", alpha = 0.8, size = 3) +
        labs(title = "Fee Impact on Cumulated Wealth: Active Managers",
             subtitle = glue::glue("Base Return: ASISA Ave. Performance | Start Date: {format(Start, '%B %Y')}"),
             x = "", y = "Cumulative Returns") +

        scale_x_date(labels = scales::date_format("%Y"), date_breaks = "1 year") +
        theme(legend.position="bottom") +
        theme(legend.title = element_blank())+
        theme(axis.text.x=element_text(angle = 90, hjust = 1))



graph

}
