
graph_q2.4_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){


# join the data: BRICS Bonds and the VIX
q2_data_global <- left_join(
    # JOIN BRICS DATA
    bonds_10y %>% group_by(Name) %>% filter(Name == "Brazil_10Yr") %>% ungroup() %>% rename(Brazil_10Yr = Bond_10Yr) %>% select(-Name),
    bonds_2y %>% group_by(Name) %>% filter(Name == "Brazil_2yr") %>% ungroup() %>% rename(Brazil_2yr = Bond_2Yr) %>% select(-Name),
    by = "date") %>%

    left_join(.,
    bonds_10y %>% group_by(Name) %>% filter(Name == "CHINA_10Yr") %>% ungroup() %>% rename(CHINA_10Yr = Bond_10Yr) %>% select(-Name),
    by = "date") %>%

    left_join(.,
    bonds_2y %>% group_by(Name) %>% filter(Name == "CHINA_2yr") %>% ungroup() %>%  rename(CHINA_2yr = Bond_2Yr) %>% select(-Name),
    by = "date") %>%

    left_join(.,
    bonds_10y %>% group_by(Name) %>% filter(Name == "India_10Yr") %>% ungroup()  %>% rename(India_10Yr = Bond_10Yr) %>% select(-Name),
    by = "date") %>%

    left_join(.,
    bonds_2y %>% group_by(Name) %>% filter(Name == "India_2yr") %>% ungroup()  %>% rename(India_2yr = Bond_2Yr)%>% select(-Name),
    by = "date") %>%

    left_join(.,
    bonds_10y %>% group_by(Name) %>% filter(Name == "Russia_10Yr") %>% ungroup()  %>% rename(Russia_10Yr = Bond_10Yr) %>% select(-Name),
    by = "date") %>%

    left_join(.,
    bonds_2y %>% group_by(Name) %>% filter(Name == "Russia_2yr") %>% ungroup()  %>% rename(Russia_2yr = Bond_2Yr) %>% select(-Name),
    by = "date") %>%


        # JOIN SA DATA
        left_join(.,
                  SA_bonds %>% select(-SA_3M),
                  by = "date") %>%
        # JOIN THE VIX
        left_join(.,
                  IV %>% mutate(VIX = Price) %>% group_by(Name) %>% filter(Name == "VIX")
                  %>% ungroup() %>% select(-Name, -Price),
                  by = "date")

 #-------------------------------------------------------------------------------


  G <- df_q2.4_data <- q2_data_global %>%
        arrange(date) %>%
        filter(date >= as.Date("2018-01-01") & date <= as.Date("2022-12-31")) %>%

        # Create spreads for each BRICS COuntry
        mutate(SA_10Yr_2Yr = ZA_10Yr - ZA_2Yr) %>%
        mutate(Brazil_10Yr_2Yr = Brazil_10Yr - Brazil_2yr) %>%
        mutate(China_10Yr_2Yr = CHINA_10Yr - CHINA_2yr) %>%
        mutate(India_10Yr_2Yr = India_10Yr - India_2yr) %>%
        mutate(Russia_10Yr_2Yr = Russia_10Yr - Russia_2yr) %>%

        # select the BRICS's Spreads
        select(date, Brazil_10Yr_2Yr, China_10Yr_2Yr, India_10Yr_2Yr, Russia_10Yr_2Yr, SA_10Yr_2Yr, VIX)%>%
        gather(Name, Value, -date)


#-------------------------------------------------------------------------------


graph <-  ggplot(df_q2.4_data) +
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
        theme(legend.position="") +
        theme(legend.title = element_blank())


    graph

}


