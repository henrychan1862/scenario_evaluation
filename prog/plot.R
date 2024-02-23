library(ggplot2)

# plotting config--------------------------
MyThemeLine_grid <- theme_bw() +
    theme(
        panel.border = element_rect(fill = NA),
        panel.grid.minor = element_line(color = NA),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill = "white", colour = "white"),
        strip.text.x = element_text(size = 15, colour = "black", angle = 0, face = "bold"),
        strip.text.y = element_text(size = 15, colour = "black", angle = -90, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1, size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
    )


# box explanation function-------------------
### explain function---------------------------
ggplot_box_legend <- function(family = "serif") {
    # Create data to use in the boxplot legend-------------------------
    set.seed(100)

    sample_df <- data.frame(parameter = "test", values = sample(500))

    # Extend the top whisker a bit:
    sample_df$values[1:100] <- -201:-300
    # Make sure there's only 1 lower outlier:
    # sample_df$values[1] <- 800

    # Function to calculate important values:
    ggplot2_boxplot <- function(x) {

        quartiles <- as.numeric(quantile(x, probs = c(0.25, 0.5, 0.75)))

        names(quartiles) <- c(
            "25th\npercentile",
            "Median",
            "75th\npercentile"
        )

        IQR <- diff(quartiles[c(1, 3)])

        upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
        lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])
        upper_dots <- x[x > (quartiles[3] + 1.5 * IQR)]
        lower_dots <- x[x < (quartiles[1] - 1.5 * IQR)]

        return(list("quartiles" = quartiles,
                    "25th percentile" = as.numeric(quartiles[1]),
                    "Median" = as.numeric(quartiles[2]),
                    "75th percentile" = as.numeric(quartiles[3]),
                    "IQR" = IQR,
                    "upper_whisker" = upper_whisker,
                    "lower_whisker" = lower_whisker,
                    "upper_dots" = upper_dots,
                    "lower_dots" = lower_dots))
    }

    # Get those values------------------------
    ggplot_output <- ggplot2_boxplot(sample_df$values)

    # Lots of text in the legend, make it smaller and consistent font:
    update_geom_defaults("text", list(size = 6))

    # Create the legend-----------------------------
    # The main elements of the plot (the boxplot, error bars, and count)
    # are the easy part.
    # The text describing each of those takes a lot of fiddling to
    # get the location and style just right:
    explain_plot <- ggplot() +
        ggtitle("Box plot guide") +
        geom_boxplot(
            data = sample_df,
            aes(x = parameter, y = values),
            width = 0.3
        ) +
        geom_point(aes(x = "test", y = 600)) +
        theme_minimal(base_size = 5, base_family = family) +
        geom_segment(
            aes(x = 1.3, xend = 1.3,
                y = ggplot_output[["25th percentile"]],
                yend = ggplot_output[["75th percentile"]])
        ) +
        geom_segment(
            aes(
                x = 1.2, xend = 1.3,
                y = ggplot_output[["25th percentile"]],
                yend = ggplot_output[["25th percentile"]]
            )
        ) +
        geom_segment(
            aes(
                x = 1.2, xend = 1.3,
                y = ggplot_output[["75th percentile"]],
                yend = ggplot_output[["75th percentile"]]
            )
        ) +
        geom_text(
            aes(x = 1.4, y = ggplot_output[["Median"]]),
            label = "Interquartile range (IQR)", fontface = "bold", angle = -90, hjust = 0.5, vjust = 0.5, size = 7
        ) +
        geom_text(
            aes(
                x = c("test", "test"),
                y = c(
                    ggplot_output[["upper_whisker"]] + 30,
                    ggplot_output[["lower_whisker"]] - 30
                ),
                label = c(
                    "1.5 x IQR above",
                    "1.5 x IQR below"
                )
            ), fontface = "bold", hjust = 0.5
        ) +
        geom_text(
            aes(x = "test", y = 600, label = "Outlier"), fontface = "bold", nudge_x = 0.1, hjust = 0
        ) +
        geom_text(
            aes(
                x = "test", y = ggplot_output[["quartiles"]],
                label = names(ggplot_output[["quartiles"]])
            ), nudge_x = -0.35,  hjust = "middle", size = 5
        ) +
        ylab("") +
        xlab("") +
        theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            title = element_text(size = 18, face = "bold")
        )

    return(explain_plot)
}