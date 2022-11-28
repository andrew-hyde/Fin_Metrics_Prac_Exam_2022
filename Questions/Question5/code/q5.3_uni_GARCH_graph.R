
q5_GARCH_Graph <- function(df_data, title, subtitle, caption, xlabel, ylabel){

    ZAR_Returns <- df_data %>%
        group_by(Name) %>%
        filter(Name == "SouthAfrica_Cncy") %>%
        arrange(date) %>%
        mutate(dlogret = log(Price) - log(lag(Price))) %>%
        mutate(scaledret = (dlogret - mean(dlogret, na.rm = T))) %>%
        ungroup() %>%
        filter(date > first(date))

    ZAR_Returns <- ZAR_Returns %>%
        tbl_xts(., cols_to_xts = dlogret, spread_by = Name)

    ZAR_Returns[is.na(ZAR_Returns)] <- 0

    #-------------------------------------------------------------------------------

    Plotdata = cbind(ZAR_Returns, ZAR_Returns^2, abs(ZAR_Returns))
    colnames(Plotdata) = c("Returns", "Returns_Sqd", "Returns_Abs")



    Plotdata <- Plotdata %>%
        xts_tbl() %>%
        gather(ReturnType, Returns, -date)


    ggplot(Plotdata) +
        geom_line(aes(x = date, y = Returns, colour = ReturnType, alpha = 0.5)) +
        ggtitle("Return Type Persistence: USD/ZAR") +
        facet_wrap(~ReturnType, nrow = 3, ncol = 1, scales = "free") +
        guides(alpha = "none", colour = "none") +
        theme()

    #-------------------------------------------------------------------------------

    garch11 <- ugarchspec(
        variance.model = list(model = c("sGARCH","gjrGARCH","eGARCH","fGARCH","apARCH")[1],
                              garchOrder = c(1, 1)),
        mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
        distribution.model = c("norm", "snorm", "std", "sstd", "ged", "sged", "nig", "ghyp", "jsu")[1])

    # Now to fit, I use as.matrix and the data - this way the plot functions we will use later will work.
    garchfit1 = ugarchfit(spec = garch11, data = ZAR_Returns)

    # now I get the structural volatility and plot it with my returns
    # to visualise the difference actual volatility and noise
    sigma <- sigma(garchfit1) %>% xts_tbl()
    colnames(sigma) <- c("date", "sigma")
    sigma <- sigma %>% mutate(date = as.Date(date))

    #-------------------------------------------------------------------------------

    graph <- ggplot() +
        geom_line(data = Plotdata %>%
                      filter(ReturnType == "Returns_Sqd") %>%
                      select(date, Returns) %>%
                      unique %>%
                      mutate(Returns = sqrt(Returns)), aes(x = date, y = Returns)) +

        geom_line(data = sigma, aes(x = date, y = sigma), color = "red", size = 2, alpha = 0.8) +

        labs(title = title,
             subtitle = subtitle,
             caption = caption,
             y = ylabel,
             x = xlabel) +

        theme() +
        theme(axis.text.x = element_text(angle = 0, hjust = 0)) +
        theme(legend.position = "")

    graph

}



