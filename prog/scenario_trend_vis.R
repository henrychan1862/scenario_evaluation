# library & source-----------------------
library(data.table)
library(ggplot2)
library(yaml)

source("prog/plot.R")


# CONSTANT DEFINITION-----------------------
region_factor <- c("World", "OECD & EU", "Asia", "Latin America", "Middle East & Africa", "Reforming Economies")
region_frame <- as.data.table(
    list(
        region = c("World", "R5OECD90+EU", "R5ASIA", "R5LAM", "R5MAF", "R5REF"),
        r5_region = c(
            "World", "OECD & EU (R5)", "Asia (R5)", "Latin America (R5)",
            "Middle East & Africa (R5)", "Reforming Economies (R5)"
        )
    )
)

# set r5 factors
factorize_region <- function(dt, key = "r5_region") {
    return(
        dt[
            region_frame,
            on = c(key)
        ][
            ,
            region := factor(gsub(pattern = " \\(R5\\)", replacement = "", x = r5_region), levels = region_factor)
        ][
            order(year)
        ]
    )
}


# reading data--------------------------------------
## historical data-----------------------
co2_historical <- rbindlist(
    list(
        fread(file = "output/co2_eip_fossil_historical_r5.csv"),
        fread(file = "output/co2_eip_fossil_historical_world.csv")
    ),
) |>
    factorize_region()
ch4_historical <- rbindlist(
    list(
        fread(file = "output/ch4_historical_r5.csv"),
        fread(file = "output/ch4_historical_world.csv")
    ),
) |>
    factorize_region()
tes_historical <- fread(file = "output/tes_historical_r5_world.csv") |>
    factorize_region()
nuclear_historical <- fread(file = "output/elec_nuclear_historical_r5_world.csv") |>
    factorize_region()
renewable_historical <- fread(file = "output/elec_solar_wind_historical_r5_world.csv") |>
    factorize_region()

## scenario data----------------------
# List files matching a specific pattern using list.files with regular expressions
file_names <- list.files("output/", pattern = "^quantile*", full.names = TRUE)
var <- c("CO2", "CH4", "Primary", "nuclear", "solar")
.read_quantile_projections <- function(name) {
    return(
        lapply(
            grep(name, file_names, value = TRUE),
            function(csv_file) {
                return(fread(csv_file))
            }
        ) |>
            rbindlist() |>
            setnames(c("period"), c("year")) |>
            factorize_region(key = "region")
    )
}
co2_projection <- .read_quantile_projections(var[1])
ch4_projection <- .read_quantile_projections(var[2])
tes_projection <- .read_quantile_projections(var[3])
nuclear_projection <- .read_quantile_projections(var[4])
renewable_projection <- .read_quantile_projections(var[5])


# visualisation------------------------
# export historical trend w/ projections-----------------
historical_projections <- function(history, projection, unit, name) {
    ggplot() +
        geom_ribbon(
            data = projection,
            mapping = aes(
                x = year,
                ymin = lower_90,
                ymax = upper_90,
                group = region,
                fill = "5-95% range",
            ),
        ) +
        geom_line(
            data = projection,
            mapping = aes(
                x = year,
                y = median,
                color = region,
                linetype = "Projection"
            )
        ) +
        geom_line(
            data = history |>
                copy() |>
                setnames(3, "value"),
            mapping = aes(
                x = year,
                y = value,
                color = region,
                linetype = "Historical"
            )
        ) +
        facet_wrap(~region, nrow = 3, scales = "free") +
        MyThemeLine_grid +
        xlab("Year") +
        ylab(unit) +
        theme(
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.text = element_text(size = 20),
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 0, hjust = 0.5)
        ) +
        scale_linetype_manual(
            breaks = c("Historical", "Projection"),
            values = c(
                "Historical" = 1,
                "Projection" = 2
            )
        ) +
        scale_fill_manual(
            breaks = c("5-95% range"),
            values = c("5-95% range" = "grey90")
        ) +
        guides(
            linetype = guide_legend(ncol = 1, order = 2, override.aes = list(color = "black")),
            color = guide_legend(override.aes = list(linewidth = 1.5), order = 1),
            fill = guide_legend(order = 3)
        )

    ggsave(filename = paste0("output/", name, "_trend.png"), width = 15, height = 10)
}

historeis <- list(
    co2_historical, ch4_historical, tes_historical, nuclear_historical, renewable_historical
)
projections <- list(
    co2_projection, ch4_projection, tes_projection, nuclear_projection, renewable_projection
)
units <- c("Mt CO2/yr", "Mt CH4/yr", "EJ", "EJ", "EJ")

for (i in 1:5) {
    historical_projections(historeis[[i]], projections[[i]], units[[i]], var[[i]])
}

# refine nuclear and renewable plot -----------------
for (i in 4:5) {
    historical_projections(
        historeis[[i]] |>
            _[
                year > 2005
            ],
        projections[[i]] |>
            _[
                year <= 2030
            ],
        units[[i]],
        paste0("refined_", var[[i]])
    )
}
