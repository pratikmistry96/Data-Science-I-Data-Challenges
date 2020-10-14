## Store functions for shiny app

election_comp <- function(data, year_select) {
  comp_plot <- data %>%
    filter(cycle == 2020 |
             cycle == year_select) %>%
    ggplot(aes(x = cycle,
               y = pct_trend_adjusted,
               group = party)) +
    geom_col(aes(fill = party),
             position = "dodge") +
    geom_text(
      aes(
        x = cycle,
        y = pct_trend_adjusted + 9,
        label = str_wrap(candidate_name, 8)
      ),
      position = position_dodge(0.9),
      size = 3
    ) +
    scale_fill_manual(values = c("Blue",
                                 "Red",
                                 "Green")) +
    ylim(0, 100)
  comp_plot <- ggplotly(comp_plot)
  comp_plot$x$data[[3]]$hoverinfo <- "none"
  return(comp_plot)
}


trend_2020 <- function(data,
                       monthStart = 2,
                       monthEnd = 10) {
  trend_plot <- data %>%
    mutate(modeldate = as.Date(modeldate)) %>%
    filter(months(modeldate) >= month.name[monthStart] &
             months(modeldate) <= month.name[monthEnd]) %>%
    ggplot(aes(x = modeldate,
               y = pct_trend_adjusted)) +
    geom_line(aes(color = candidate_name)) +
    ylim(25, 75) +
    xlab("Model Date") +
    ylab("Polling Percentage")
  return(ggplotly(trend_plot))
}


election_map_plot <- ggplot(election_map, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group,
                   fill = winner), alpha = 0.6, color = "black") +
  scale_fill_manual(values = c("Red", "Blue")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray92"),
        legend.background = element_rect(fill = "gray92"))

ggplotly(election_map_plot)




ggplot(aes(x = modeldate,
           y = pct_trend_adjusted,
           color = candidate_name)) +
  geom_line(
    #aes(
    #text = paste0("Poll Date: ", modeldate,
    #              "\nCandidate: ", candidate_name,
    #              "\nPoll Percentage: ",
    #              round(pct_trend_adjusted,1),"%")
    #),
    size = 1) +
  labs(
    title = "2020 Election Polling Trend",
    x = "Model Date",
    y = "Polling Percentage",
    color = "Candidate Name"
  ) +
  theme(
    plot.background = element_rect(fill = "gray92"),
    legend.background = element_rect(fill = "gray92")
  ) +
  ylim(0, 100)
trend.plot <- ggplotly(trend.plot)