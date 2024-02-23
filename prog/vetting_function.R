# read iamc-format file into data.table---------------------
# wide-to-long operation
# shorter process time when reading large file using data.table than tibble/quitte
# most of the time data.table (which is an extension of data.frame) can be used with quitte function
read_iamc_table <- function(file) {
    return(
        fread(file, header = TRUE, showProgress = TRUE, stringsAsFactors = TRUE) |>
            setnames(tolower) |>
            melt.data.table(
                id.vars = c("model", "scenario", "region", "variable", "unit"),
                variable.name = "period", variable.factor = FALSE
            ) |>
            _[, period := as.integer(period)] |>
            setindex(model, scenario, region, variable, period)
    )
}

# parse the format-checked data.frame to data.table
parse_iamc_table <- function(checked_df) {
    return(
        checked_df |>
            as.data.table() |>
            _[
                ,
                `:=`(
                    model = as.factor(model),
                    scenario = as.factor(scenario),
                    region = as.factor(region),
                    variable = as.factor(variable),
                    unit = as.factor(unit),
                    value = as.double(value)
                )
            ] |>
            setindex(model, scenario, region, variable, period)
    )
}



# check aggregate function----------------------------
# check sum of components is compatabile with target variable
# return tibble of result
check_aggregate <- function(df, target, components) {
    print(paste(target, "<-->", paste(components, sep = " & ")))
    if (is.data.table(df)) {
        # if df is a data.table class, use faster data.table style
        return(df[
            components,
            .(component_total = sum(value)), # sum of sub categories
            by = .(model, scenario, region, period),
            on = "variable"
        ][
            df[
                target, # join with target
                .(model, scenario, region, period, target = value),
                on = "variable"
            ],
            on = c("model", "scenario", "region", "period")
        ][
            ,
            diff := target - component_total # difference
        ][
            ,
            relative_diff := diff / target # relative difference
        ][
            ,
            absolute_relative_diff := abs(relative_diff) # absolute value of relativce differecne
        ][
            order(-absolute_relative_diff) # the most different item comes first
        ] |>
            copy())
    } else {
        # if df is a data.frame class, use traditional tidyverse-style process
        return(df |>
            filter( # sum of components
                variable %in% components
            ) |>
            group_by(model, scenario, region, period) |>
            summarise(
                component_total = sum(value)
            ) |>
            inner_join(
                df |>
                    filter( # target
                        variable %in% components
                    ) |>
                    group_by(model, scenario, region, period) |>
                    summarise(
                        target = value
                    ),
                by = c("model", "scenario", "region", "period")
            ) |>
            mutate(
                diff = target - component_total,
                relative_diff = diff / target, # relative difference
                absolute_relative_diff = abs(relative_diff)
            ) |>
            arrange(desc(absolute_relative_diff)))
    }
}


# sub utility functions to be used in vetting------------
# utility internal funciton: get period to be filtered
# get the array of years used to be filtered according to their input mode
.get_period <- function(x, step = 1) {
    if (is.null(names(x))) {
        return(x)
    } else {
        if (names(x) == "before") {
            return(seq(2010, first(x), step))
        } else if (names(x) == "after") {
            return(seq(first(x), 2100, step))
        }
    }
}

# close all the file connections to avoid unwanted export of log
.sink.reset <- function() {
    for (i in seq_len(sink.number())) {
        sink()
    }
}

# 0. compose variable from sub-category if the variable of question is missing
compose_from_order <- function(df, order) {
    if (!is.null(names(order))) {
        cat("Detect variable order. Start composing.", sep = "\n")
        return(
            lapply(
                names(order),   # loop through the parent variable defined in define/variable_order.yaml
                function(parent) {
                    children <- order[[parent]]
                    if (grepl("#[0-9]$", parent)) {
                        parent <- strsplit(parent, "#[0-9]")[1] |> unlist()
                    }
                    vars <- c(parent, children)  # first filter parent variable & children variables
                    filter_vars <- rep(vars != parent) |> as.list() # next filter scenarios w/ children but w/o parent
                    cat(paste(
                        "Composing", paste0("`", parent, "`"),
                        "from", paste(paste0("`", children, "`"), collapse = " & ")
                    ), sep = "\n")
                    return(
                        df[
                            # join the cacndidate with only children variables
                            .(vars[vars != parent]),
                            on = .(variable)
                        ][
                            # get scenarios that can be composed from children
                            df[
                                .(vars),
                                on = .(variable)
                            ] |>
                                dcast.data.table(
                                    model + scenario + region ~ variable,
                                    value.var = "value", fun.aggregate = function(x) any(!is.na(x))
                                ) |>
                                _[
                                    filter_vars,
                                    .(model, scenario, region),
                                    on = vars
                                ],
                            on = .(model, scenario, region)
                        ][
                            !is.na(value),
                            .(
                                unit = unit,
                                variable = parent,  # the resulting variable will be named using parent
                                value = sum(value)
                            ),
                            by = .(model, scenario, region, period)
                        ] |>
                            unique()
                    )
                }
            ) |>
                append(list(df)) |> # bind all composed data.table to the original data.table
                rbindlist(use.names = TRUE)
        )
    } else {
        return(
            df
        )
    }
}

# 1. check if interpolation is needed (if yes apply) and return filterd data.table
interpolate_filter <- function(df, item) {

    if (
        # quick check if there is any NA in the desired range using quick filter (low-cost)
        df[.(item$region), on = c("region")
        ][.(item$variable), on = c("variable")
        ][.(.get_period(item$period)), on = c("period")
        ][, any(is.na(value))
        ]) {
        cat(paste(
            "Value is interpolated from", first(.get_period(item$period)) - 10,
            "through out to", last(.get_period(item$period)) + 10
        ), sep = "\n")
        return(
            # run interpolation if any NA exists, using +/- 10 years of original period
            # this filter is more expensive as it is not utilizing secondary indices
            list(
                # if only one valid value between the expanded period,
                # use that value directly (becoz approx will raise error when only one value presents)
                df[
                    region %in% item$region &
                        variable %in% item$variable &
                        period %between% c(
                            first(.get_period(item$period)) - 10,
                            last(.get_period(item$period)) + 10
                        )
                ][
                    ,
                    n_value := sum(!is.na(value)),
                    by = .(model, scenario, region, variable)
                ][
                    n_value == 1 & !is.na(value)
                ][
                    ,
                    `:=`(
                        unit = unit,
                        period = period,
                        value = .SD[!is.na(value), value]
                    ),
                    by = .(model, scenario, region, variable)
                ][
                    period %in% .get_period(item$period),
                ][
                    ,
                    n_value := NULL
                ],
                # rbind the normally interpolated data.tables
                df[
                    region %in% item$region &
                        variable %in% item$variable &
                        period %between% c(
                            first(.get_period(item$period)) - 10,
                            last(.get_period(item$period)) + 10
                        )
                ][
                    ,
                    n_value := sum(!is.na(value)),
                    by = .(model, scenario, region, variable)
                ][
                    n_value > 1 & !is.na(value),
                    .(unique(.SD))
                ][
                    ,
                    .(
                        # return only desired period
                        unit = first(unit),
                        period = approx(period, value, xout = .get_period(item$period),
                                        method = "linear", rule = 2)$x,
                        # same for value as well
                        value = approx(period, value, xout = .get_period(item$period),
                                       method = "linear", rule = 2)$y
                    ),
                    by = .(model, scenario, region, variable)
                ]
            ) |>
                rbindlist(use.names = TRUE) # vertically combine two data.table
        )
    }

    return(
        # if not just simply filter it
        # NOTE: filter using secondary indices can lead to incomplete result,
        # when multiple conditons are applied to multiple columns simultaneouly
        df[.(item$region), on = c("region")
        ][.(item$variable), on = c("variable")
        ][.(.get_period(item$period)), on = c("period")
        ]
    )
}

# 2. check if aggregation of variable is needed
combine_variable <- function(df, item, parent_df = NULL) {
    if (length(item$variable) > 1) {
        cat(paste("compound variable is made up of",
                  paste(paste0("`", item$variable, "`"), collapse = " + ")),
            sep = "\n")
        if (!is.null(parent_df)) {
            parent_item <- item
            parent_item$variable <- item$use_parent # adjust parent variables to enable interpolate
            cat(paste(
                "Allow use of parent variable", paste0("`", item$use_parent, "`")
            ), sep = "\n")
            return(
                list(
                    # use parent variables for scenarios that are not valid, interpoalte
                    parent_df[
                        !df[
                            ,
                            .(
                                valid = uniqueN(variable) == length(item$variable)
                            ),
                            by = list(model, scenario, region, period)
                        ][
                            .(TRUE),
                            on = .(valid)
                        ][
                            ,
                            valid := NULL
                        ],
                        on = .(model, scenario, region)
                    ] |>
                        interpolate_filter(parent_item),
                    # combine with valid scenarios
                    df[
                        ,
                        .(
                            unit = unit,
                            variable = paste(variable, collapse = " + "),
                            value = sum(value),
                            valid = uniqueN(variable) == length(item$variable)
                        ),
                        by = list(model, scenario, region, period)
                    ][
                        .(TRUE),
                        .(model, scenario, region, variable, unit, period, value),
                        on = .(valid)
                    ]
                ) |>
                    rbindlist(use.names = TRUE)
            )
        }
        return(
            # default return only scenarios having all candidates variables
            df[
                ,
                .(
                    unit = unit,
                    variable = paste(variable, collapse = " + "),
                    value = sum(value),
                    valid = uniqueN(variable) == length(item$variable)
                ),
                by = list(model, scenario, region, period)
            ][
                .(TRUE),
                .(model, scenario, region, variable, unit, period, value),
                on = .(valid)
            ]
        )
    } else {
        if (!is.null(parent_df)) {
            parent_item <- item
            parent_item$variable <- item$use_parent # adjust parent variables to enable interpolate
            cat(paste(
                "Allow use of parent variable", paste0("`", item$use_parent, "`")
            ), sep = "\n")
            return(
                list(
                    # use parent variables for scenarios that are not valid, interpoalte
                    parent_df[
                        !df[
                            ,
                            .(model, scenario, region)
                        ],
                        on = .(model, scenario, region)
                    ] |>
                        interpolate_filter(parent_item),
                    # combine with valid scenarios
                    df
                ) |>
                    rbindlist(use.names = TRUE)
            )
        }
        return(df)
    }
}

# 3. calculate % change if needed & export intermideate data table as csv
calc_percent_change <- function(df, item) {
    if (item$unit == "% change") {
        cat("Percentage Change is calculated", sep = "\n")
        return(
            df[
                ,
                .SD[
                    order(period),
                    .(
                        unit = "% change",
                        period = period,
                        value = (value - first(value)) / first(value) * 100
                    )
                ],
                by = list(model, scenario, region, variable)
            ][value != 0]
        )
    }
    return(df)
}

# 3.5 export intermediate result as csv
export_csv <- function(df, item, path) {
    fwrite( # export intermediate result as csv
        df,
        file = paste0(
            path,
            item$description,
            ".csv"
        )
    )
    return(df)
}

# 4. check if the value fall within the range
vet_with <- function(df, item) {
    print(unique(df), nrows = Inf)
    if (all(!is.nan(item$value))) {
        # if value presents, use with range
        ref_values <- item$value    # named list for specific values
        names(ref_values) <- item$region
        lapply(
            item$region,
            function(reg) {
                if (item$unit == "% change") {
                    # if unit is percentage change, treat range literally +/- value
                    cat(paste(reg, ": ", round(ref_values[[reg]] * 100), "%. "))
                    cat(paste(
                        "*** Ref range is",
                        round((ref_values[[reg]] - item$range) * 100), "% to",
                        round((ref_values[[reg]] + item$range) * 100), "% ***"
                    ), sep = "\n")
                    df[
                        # apply vetting in reference
                        region == reg,
                        pass := between(
                            value,
                            (ref_values[[reg]] - item$range) * 100,
                            (ref_values[[reg]] + item$range) * 100
                        )
                    ]

                } else {
                    # if it is not about percentage change, treat range as percentage
                    cat(paste(reg, ": ", ref_values[[reg]], ". "))
                    cat(paste(
                        "*** Ref range is",
                        ref_values[[reg]] * (1 - item$range), "to", ref_values[[reg]] * (1 + item$range),
                        item$unit, "***"
                    ), sep = "\n")
                    df[
                        # apply vetting in reference
                        region == reg,
                        pass := between(
                            value,
                            ref_values[[reg]] * (1 - item$range),
                            ref_values[[reg]] * (1 + item$range)
                        )
                    ]
                }
            }
        )
        return(df)
    } else if (names(item$range) == "between") {
        # if between is defined, use range
        # NOTE: currently range has to be fixed for all regions
        # May consider update in the future
        cat(
            paste(
                "*** Ref range is",
                first(item$range$between), "to", last(item$range$between),
                item$unit, "***"
            ), sep = "\n")
        return(
            df[
                ,
                pass := between(value, first(item$range$between), last(item$range$between))
            ]
        )
    } else if (names(item$range) == "above") {
        cat(paste("*** Value has to be above",
                  item$range$above,
                  item$unit, "***"), sep = "\n")
        return(
            df[
                ,
                pass := value > item$range$above
            ]
        )
    } else if (names(item$range) == "below") {
        cat(paste("*** Value has to be below",
                  item$range$below,
                  item$unit, "***"), sep = "\n")
        return(
            df[
                ,
                pass := value < item$range$below
            ]
        )
    }
}

# 5. summarize vetting result
overall_outcome <- function(df) {
    return(
        df[
            ,
            .(all_pass = all(pass)),
            by = .(model, scenario, region)
        ]
    )
}


# vetting function---------------------------

# Vetting scenario input in data.table style
# Return summary of vetting result as data.table
# If set `mode = check`, return list of intermediate calculation
# Please see `vetting_template.yaml` for reference
vetting <- function(df, ref_path, var_order = NULL, mode = "summary", log = "vetting.log", csv_path = "output/") {

    .sink.reset() # close all previous connections

    # read data & snapshot
    start_time <- Sys.time() # count the time of process
    origni_width <- getOption("width") # get the original width
    referenceYAML <- yaml.load_file(ref_path) # read vetting reference yaml & set logging

    # set-up
    options(width = 300) # set wider to capture whole data frame
    sink(paste0("log/", log)) # sink the log file to log/
    cat(paste("Total # of scenarios is", uniqueN(df[, .(model, scenario)])), sep = "\n")

    # exit function
    on.exit({
        .sink.reset()
    }, add = TRUE) # close log file after function executed
    on.exit({
        options(width = origni_width)
    }, add = TRUE) # reset terminal width when finished
    on.exit({
        cat(
            paste(
                "Running time is",
                difftime(Sys.time(), start_time, units = "mins") |> as.numeric() |> round(2),
                "min."
            ),
            sep = "\n")
    }, add = TRUE) # report running time

    # read variable order if present and complement variables
    if (!is.null(var_order)) {
        variableOrder <- yaml.load_file(var_order)
        df <- compose_from_order(df, variableOrder)
    }

    # main part
    if (mode == "summary") {
        # if summary, return summary data.table showing result per test
        return(
            lapply(
                referenceYAML |>
                    unlist(recursive = FALSE) |> # unlist to historical & future
                    unlist(recursive = FALSE), # unlist to indicator category (e.g. Emissions)
                function(item) {
                    cat("------------------------------------------------", sep = "\n")
                    cat(paste("Vetting", item$description, "..."), sep = "\n")
                    parent_df <- NULL   # default not to use parent variales unless specified
                    if ("use_parent" %in% names(item)) {
                        parent_df <- df[
                            .(item$use_parent),
                            on = .(variable)
                        ][
                            !is.na(value)
                        ]
                    }
                    return(
                        df |>
                            interpolate_filter(item) |>
                            combine_variable(item, parent_df = parent_df) |>
                            calc_percent_change(item) |>
                            export_csv(item, csv_path) |>
                            vet_with(item) |>
                            overall_outcome()
                    )
                }
            ) |>
                rbindlist(use.names = TRUE, idcol = "indicator") |> # vertical concat
                dcast.data.table(model + scenario + region ~ indicator, # long to wide for readbility
                                 value.var = "all_pass") |>
                _[
                    unique(df[, .(model, scenario)]),
                    on = .(model, scenario)
                ]

        )
    } else if (mode == "check") {
        # if check, return list of data.table of intermediate calculation (i.e. before vetting)
        return(
            lapply(
                referenceYAML |>
                    unlist(recursive = FALSE) |> # unlist to historical & future
                    unlist(recursive = FALSE), # unlist to indicator category (e.g. Emissions)
                function(item) {
                    cat("------------------------------------------------", sep = "\n")
                    cat(paste("Vetting", item$description, "..."), sep = "\n")
                    parent_df <- NULL   # default not to use parent variales unless specified
                    if ("use_parent" %in% names(item)) {
                        parent_df <- df[
                            .(item$use_parent),
                            on = .(variable)
                        ][
                            !is.na(value)
                        ]
                    }
                    return(
                        df |>
                            interpolate_filter(item) |>
                            combine_variable(item, parent_df = parent_df) |>
                            calc_percent_change(item) |>
                            export_csv(item, csv_path)
                    )
                }
            )
        )
    }
}

# WARNING: DEPRECATED, please use vetting() with data.table
# Return a data frame showing the validaity of scenarios per test
# Result of vetting is return in a named list (summary and detail)
# The intermediate process is exported as log file by the given name under log/
# NOTE: unit is not verified before calculation
# Also, currently only support world level
# -  1. Extract the corresponding variable from scenario
# -  2. aggregate to compose the indicator if needed
# -  3. return the vetting result as dataframe
vetting_data_frame <- function(df, ref, file = "vetting_legacy.log") {
    options(width = 180) # set wider to capture whole data frame
    sink(paste0("log/", file)) # sink the log file to log/
    cat(paste("Total # of scenarios vetting is", df |> getScenarios() |> length()), sep = "\n")
    result <- tibble(scenario = getScenarios(userScenario)) # result data frame
    calculated <- list() # list for binding calculated dataframes

    # loop through each indicator
    for (i in seq_len(nrow(ref))) {
        vettingInfo <- ref[i, ] |> as.list() # convert tibble to named list
        cat(paste("Vetting", vettingInfo[["description"]], "..."), sep = "\n")

        # split the variable if needed
        if (grepl("\\+", vettingInfo[["variable"]])) {
            vettingInfo[["variable"]] <- str_split(vettingInfo[["variable"]], "\\+")[[1]]
            cat(paste(" ", vettingInfo[["variable"]]), sep = "\n")
        }

        # get the required years if vetting period is NA
        # this is for the case `before` and `after`
        if (is.na(vettingInfo[["period"]])) {
            year_how <- tail(str_split(vettingInfo[["description"]], " ")[[1]], 2)
            if (year_how[1] == "before") {
                vettingInfo[["period"]] <- c(2010, as.double(year_how[2]))
            } else if (year_how[1] == "after") {
                vettingInfo[["period"]] <- c(as.double(year_how[2]), 2100)
            }
            cat(paste(" ", year_how), sep = "\n")
        }

        # get the delta period if it is of % change
        if (vettingInfo[["unit"]] == "% change") {
            delta_period <- tail(str_split(vettingInfo[["description"]], " ")[[1]], 1)
            vettingInfo[["period"]] <- str_split(delta_period, "-")[[1]] |> as.double()
            cat(paste(" ", vettingInfo[["period"]]), sep = "\n")
        }

        # main part
        if (!anyNA(vettingInfo[["period"]])) {
            # filter the dataframe
            dfVetting <- df |>
                filter(
                    (region == vettingInfo[["region"]]) &
                        (variable %in% vettingInfo[["variable"]])
                )

            # interpolate the required period, taking +/- 5 years to do it
            # this only works when vetting single year
            if (
                (dim(dfVetting |> filter((period %in% vettingInfo[["period"]]) & (is.na(value))))[1] > 0) ||
                    (!any(getPeriods(dfVetting) %in% vettingInfo[["period"]]))
            ) {
                dfVetting <- dfVetting |>
                    filter(between(period, vettingInfo[["period"]] - 10, vettingInfo[["period"]] + 10)) |>
                    interpolate_missing_periods(seq(vettingInfo[["period"]] - 5, vettingInfo[["period"]] + 5, 1))
                cat(paste(
                    "Value is interpolated from", vettingInfo[["period"]] - 5,
                    "through out to", vettingInfo[["period"]] + 5
                ), sep = "\n")
            }

            # filter the specific year or the range of year, exclude NA in case interpoalte doesnt help
            dfVetting <- dfVetting |>
                filter(
                    if (length(vettingInfo[["period"]]) == 1) {
                        period == vettingInfo[["period"]]
                    } else {
                        between(period, vettingInfo[["period"]][1], vettingInfo[["period"]][2])
                    }
                ) |>
                drop_na(value)

            # aggregate value if multiple variables exist
            if (length(vettingInfo[["variable"]]) > 1) {
                dfVetting <- dfVetting |>
                    calc_addVariable(
                        "Combined" = paste(paste0("`", vettingInfo[["variable"]], "`"), collapse = " + "),
                        units = vettingInfo[["unit"]], only.new = TRUE
                    ) |>
                    mutate(
                        variable = paste(vettingInfo[["variable"]], collapse = " + ")
                    )
                cat(paste("compound variable is made up of",
                          paste(paste0("`", vettingInfo[["variable"]], "`"), collapse = " + ")), sep = "\n")
            }

            # calculate % change if specified
            if (vettingInfo[["unit"]] == "% change") {
                dfVetting <- dfVetting |>
                    group_by(across(c(-period, -value))) |>
                    reframe(
                        period = period,
                        value = (value - first(value)) / first(value), # use the latest value
                        unit = vettingInfo[["unit"]]
                    ) |>
                    filter(period == vettingInfo[["period"]][-1])
                cat("Percentage Change is calculated", sep = "\n")
            }

            # check the wranggled data frame
            print.data.frame(dfVetting)
            calculated <- c(calculated, list(dfVetting))

            # get value from range if value is NA
            # calculate the scenario passing for each case
            # if the value is not given, use range value
            if (is.na(vettingInfo[["value"]])) {
                if (grepl("~", vettingInfo[["range"]])) {
                    vettingInfo[["value"]] <- str_split(vettingInfo[["range"]], "~")[[1]] |> as.double()
                    cat(paste("***Ref range is",
                              vettingInfo[["value"]][1], "to", vettingInfo[["value"]][2],
                              vettingInfo[["unit"]], "***"), sep = "\n")
                    dfVetting <- dfVetting |>
                        mutate(
                            pass = between(
                                value,
                                vettingInfo[["value"]][1],
                                vettingInfo[["value"]][2]
                            )
                        )
                } else if (grepl("<", vettingInfo[["range"]])) {
                    vettingInfo[["value"]] <- str_split(vettingInfo[["range"]], "<")[[1]][-1] |> as.double()
                    cat(paste("***Ref range is smaller than",
                              vettingInfo[["value"]], vettingInfo[["unit"]], "***"), sep = "\n")
                    dfVetting <- dfVetting |>
                        mutate(
                            pass = value < vettingInfo[["value"]]
                        )
                } else if (grepl(">", vettingInfo[["range"]])) {
                    vettingInfo[["value"]] <- str_split(vettingInfo[["range"]], ">")[[1]][-1] |> as.double()
                    cat(paste("***Ref range is larger than",
                              vettingInfo[["value"]], vettingInfo[["unit"]], "***"), sep = "\n")
                    dfVetting <- dfVetting |>
                        mutate(
                            pass = value > vettingInfo[["value"]]
                        )
                }
                # check if values fall within range
            } else {
                cat(paste("***Ref Value is", vettingInfo[["value"]],
                          vettingInfo[["unit"]], "of range +/-",
                          as.double(vettingInfo[["range"]]) * 100, "%", "***"), sep = "\n")
                dfVetting <- dfVetting |>
                    mutate(
                        pass = between(
                            value,
                            vettingInfo[["value"]] * (1 - as.double(vettingInfo[["range"]])),
                            vettingInfo[["value"]] * (1 + as.double(vettingInfo[["range"]]))
                        )
                    )
            }

            # show the vetting result, make sure only one record per scenario is counted
            dfVetting <- dfVetting |>
                group_by(across(c(model, scenario, region, variable, unit))) |>
                summarise(
                    all_pass = all(pass)
                ) |>
                ungroup()
            dfVetting |>
                summarise(
                    passed = sum(all_pass),
                    failed = n() - passed,
                    total = n(),
                    na = length(getScenarios(df)) - total
                ) |>
                head() |>
                print()
            # append the result to result dataframe
            result <- result |>
                left_join(
                    dfVetting |> select(scenario, all_pass) |> rename("{vettingInfo[['description']]}" := all_pass), #nolint
                    by = "scenario"
                )
        }
        cat("---------------------------------------------------------------------", sep = "\n")
    }
    sink()
    options(width = 80)
    cal_binded <- bind_rows(calculated, .id = "id")
    return(
        list(
            summary = result,
            detail = cal_binded
        )
    )
}


# summary function------------------------
# Return summary per test
test_summary <- function(result) {
    return(
        result[
            ,
            !c("model", "scenario")
        ][
            ,
            .(
                name = names(.SD), # name of indicator (test)
                pass = lapply(.SD, sum, na.rm = TRUE), # number of pass
                fail = lapply(.SD, function(x) sum(!x, na.rm = TRUE)), # number of fail
                not_reported = lapply(.SD, is.na) |> lapply(sum, na.rm = TRUE) # number of NA
            ),
            by = .(region)
        ][
            ,
            `:=`(tested = mapply(function(x, y) x + y, pass, fail)), # total scenarios tested
            by = .(region)
        ][
            ,
            `:=`(total = mapply(function(x, y) x + y, tested, not_reported)), # for accounting
            by = .(region)
        ][]
    )
}


# WARNING: DEPRECATED, please use plot_result with data.table
# Visualise the result of vetting
# if type = "hist", show the distribution of computed indicator per test
# NOTE: more plotting type to be added
plot_data_frame <- function(result, ref, type = "hist") {
    if (type == "hist") {
        ggplot(data = result$detail |>
            left_join(
                ref |>
                    # use the description as the title of subplots
                    select(id, description) |>
                    mutate(
                        id = as.character(id),
                        description = factor(description, levels = description)
                    ),
                by = "id"
            ) |>
            left_join(
                # get outcome of test for each scenario for highlight
                result$summary |>
                    pivot_longer(
                        cols = where(is.logical),
                        names_to = "description",
                        values_to = "pass"
                    ),
                by = c("scenario", "description")
            )) +
            geom_histogram(aes(x = value, fill = pass)) +
            facet_wrap(. ~ description, scales = "free") # plot
    }
}
