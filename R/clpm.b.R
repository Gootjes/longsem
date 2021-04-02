
# This file is a generated template, your changes will not be overwritten

clpmClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "clpmClass",
    inherit = clpmBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            factors <- self$options$factors

            factor_lengths <- sapply(factors, function(v) length(v$vars))
            ready <- readiness(self$options)
            if(ready$ready == FALSE) {
                if(ready$report) {
                    stop(ready$reason, call. = FALSE)
                }
                return(NULL)
            }

            factor_names <- c("x", "y", "z", "w")

            data <- self$data

            for(i in seq_along(factors)) {
                factor <- factors[[i]]
                for(j in seq_along(factor$vars)) {
                    v <- factor$vars[[j]]
                    nv <- paste0(factor_names[[i]], j)
                    names(data)[names(data) == v] <- nv
                }
            }

            m <- generate_clpm_syntax(factor_length = factor_lengths[1],
                                      nfactors = length(factors),
                                      constrain_autoregressions = self$options$constrain_autoregressions,
                                      constrain_crosslagged = self$options$constrain_crosslagged,
                                      constrain_observed_errors = self$options$constrain_observed_errors,
                                      constrain_residual_variances = self$options$constrain_residual_variances,
                                      constrain_covariances = self$options$constrain_covariances,
                                      estimate_observed_intercepts = self$options$estimate_observed_intercepts,
                                      estimate_observed_errors = self$options$estimate_observed_errors,
                                      estimate_latent_intercepts = self$options$estimate_latent_intercepts)

            #data <- private$.cleanData()

            # print(names(data))
            #
            # for(i in seq_along(factors)) {
            #     for(j in seq_along(factors[[i]]$vars)) {
            #         print(names(data))
            #         print(paste0(factor_names[i], j))
            #         print(factors[[i]]$vars[[j]])
            #         data[[paste0(factor_names[i], j)]] <- data[factors[[i]]$vars[[j]]]
            #         data[factors[[i]]$vars[[j]]] <- NULL
            #     }
            # }



            f_output <- capture_warnings(f <<- lavaan(m, data = data, missing = self$options$missing_data_treatment,
                                                      int.ov.free = F,
                                                      int.lv.free = F,
                                                      auto.fix.first = F,
                                                      auto.fix.single = F,
                                                      auto.cov.lv.x = F,
                                                      auto.cov.y = F,
                                                      auto.var = F))

            r <- capture.output(file = NULL, summary(f, fit.measures = T, rsquare = T, standardized = T, ci = T))

            f_warnings <- prettify_warnings(f_output)

            if(length(f_warnings) > 0 & f_warnings[[1]] != "") {

                warns$deleteRows()

                warns$addRow(rowKey = 1, values = list(warning_text = f_warnings))
            }

            text <- self$results$text
            text$setContent(r)

            lavaan_syntax <- self$results$lavaan_syntax
            lavaan_syntax$setContent(m)

            if(self$options$show_schematic_plot) {
                p <- self$results$schematic_plot
                p$setSize(width = max(c(600, 200*factor_lengths[[1]])), height = 600)
                p$setState(make_schematic(n_waves = factor_lengths[[1]], n_factors = length(factors), return_spec = T,
                                          constrain_autoregressions = self$options$constrain_autoregressions,
                                          constrain_crosslagged = self$options$constrain_crosslagged,
                                          constrain_covariances = self$options$constrain_covariances))
            }


        },
        .cleanData = function() {

            vars <- unlist(self$options$factors)

            data <- list()
            for (var in vars)
                data[[jmvcore::toB64(var)]] <- jmvcore::toNumeric(self$data[[var]])

            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'

            return(data)

        },
        .plotSchematic = function(image, ...) {
            spec <- image$state

            if(!is.list(spec)) {
                print(NULL)
                return(TRUE)
            }

            print(do.call(diagram::plotmat, args = spec))

            TRUE
        })
)
