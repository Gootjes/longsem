
# This file is a generated template, your changes will not be overwritten

riclpmClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "riclpmClass",
    inherit = riclpmBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            factors <- self$options$factors

            factor_names <- c("x", "y", "z", "w")

            factor_lengths <- sapply(factors, function(v) length(v$vars))

            t_factor_definitions <- self$results$factor_definitions

            for(i in seq_along(factors)) {
                factor <- factors[[i]]
                for(j in seq_along(factor$vars)) {
                    v <- factor$vars[[j]]
                    nv <- paste0(factor_names[[i]], j)
                    t_factor_definitions$addRow(rowKey = ((i-1)*length(factors)) + j, values = list(
                        factor = factor_names[[i]],
                        time = j,
                        variable = v
                    ))
                }
            }

            t_factor_definitions

            ready <- readiness(self$options)
            if(ready$ready == FALSE) {
                if(ready$report) {
                    stop(ready$reason, call. = FALSE)
                }
                return(NULL)
            }



            data <- self$data

            for(i in seq_along(factors)) {
                factor <- factors[[i]]
                for(j in seq_along(factor$vars)) {
                    v <- factor$vars[[j]]
                    nv <- paste0(factor_names[[i]], j)
                    names(data)[names(data) == v] <- nv
                }
            }

            if(self$options$include_random_intercept) {
                m <- generate_riclpm_syntax(factor_length = factor_lengths[1],
                                            nfactors = length(factors),
                                            constrain_autoregressions = self$options$constrain_autoregressions,
                                            constrain_crosslagged = self$options$constrain_crosslagged,
                                            constrain_observed_errors = self$options$constrain_observed_errors,
                                            constrain_residual_variances = self$options$constrain_residual_variances,
                                            constrain_covariances = self$options$constrain_covariances,
                                            estimate_observed_intercepts = self$options$estimate_observed_intercepts,
                                            estimate_observed_errors = self$options$estimate_observed_errors,
                                            estimate_latent_intercepts = self$options$estimate_latent_intercepts,
                                            estimate_intercepts_intercepts = self$options$estimate_intercepts_intercepts,
                                            constrain_intercepts_over_time = self$options$constrain_intercepts_over_time,
                                            fix_random_intercept_first_wave_covariance_to_zero = self$options$fix_random_intercept_first_wave_covariance_to_zero)
            } else {
                m <- generate_clpm_syntax(factor_length = factor_lengths[1],
                                            nfactors = length(factors),
                                            constrain_autoregressions = self$options$constrain_autoregressions,
                                            constrain_crosslagged = self$options$constrain_crosslagged,
                                            constrain_observed_errors = self$options$constrain_observed_errors,
                                            constrain_residual_variances = self$options$constrain_residual_variances,
                                            constrain_covariances = self$options$constrain_covariances,
                                            estimate_observed_intercepts = self$options$estimate_observed_intercepts,
                                            estimate_observed_errors = self$options$estimate_observed_errors,
                                            estimate_latent_intercepts = self$options$estimate_latent_intercepts,
                                            constrain_intercepts_over_time = self$options$constrain_intercepts_over_time)
            }



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

            warns <- self$results$lavaan_warnings

            if(length(f_warnings) > 0 & f_warnings[[1]] != "") {

                warns$deleteRows()

                warns$addRow(rowKey = 1, values = list(warning_text = f_warnings))
            }

            lvo <- self$results$lavaan_output
            lvo$setContent(r)

            lvs <- self$results$lavaan_syntax
            lvs$setContent(m)

            #t_estimates <- self$results$parameter_estimates

            estims <- parameterEstimates(f, standardized = TRUE, rsquare = T)

            #tlv <- self$results$latent_variables
            #print(t_estimates$.__enclos_env__$private$.items)
            #tlv <- t_estimates$get("Latent variables")$get("estimates")
            #tlv$setTitle("Latent variables")
            tlv <- self$results$latent_variables

            tlv_estims <- estims

            tlv_estims <- tlv_estims[tlv_estims$op == "=~",]
            tlv_estims <- tlv_estims[substr(tlv_estims$lhs, 1, 3) == "eta",]

            for(i in seq_along(tlv_estims[,1,drop=T])) {
                tlv$addRow(rowKey = i, values = c(tlv_estims[i,]))

            }


            #trilv <- t_estimates$get("Latent random intercept variables")$get("estimates")
            #trilv$setTitle("Latent Random Intercept variables")
            trilv <- self$results$random_intercept_latent_variables

            trilv_estims <- estims

            trilv_estims <- trilv_estims[trilv_estims$op == "=~",]
            trilv_estims <- trilv_estims[substr(trilv_estims$lhs, 1, 2) == "ri",]

            for(i in seq_along(trilv_estims[,1,drop=T])) {
                trilv$addRow(rowKey = i, values = c(trilv_estims[i,]))
            }


            #tal <- t_estimates$get("Autolagged paths")$get("estimates")
            #tal$setTitle("Autolagged paths")
            tal <- self$results$autolagged_paths

            tal_estims <- estims

            tal_estims <- tal_estims[tal_estims$op == "~",]
            tal_estims <- tal_estims[substr(tal_estims$label, 1, 1) == "a",]

            for(i in seq_along(tal_estims[,1,drop=T])) {
                tal$addRow(rowKey = i, values = c(tal_estims[i,]))
            }


            #tcl <- t_estimates$get("Crosslagged paths")$get("estimates")
            #tcl$setTitle("Crosslagged paths")
            tcl <- self$results$crosslagged_paths

            tcl_estims <- estims

            tcl_estims <- tcl_estims[tcl_estims$op == "~",]
            tcl_estims <- tcl_estims[substr(tcl_estims$label, 1, 1) == "c",]

            for(i in seq_along(tcl_estims[,1,drop=T])) {
                tcl$addRow(rowKey = i, values = c(tcl_estims[i,]))
            }

            #tcov <- t_estimates$get("Covariances")$get("estimates")
            #tcov$setTitle("Covariances")
            tcov <- self$results$covariances

            tcov_estims <- estims

            tcov_estims <- tcov_estims[tcov_estims$op == "~~",]
            tcov_estims <- tcov_estims[substr(tcov_estims$label, 1, 3) == "cov",]

            for(i in seq_along(tcov_estims[,1,drop=T])) {
                tcov$addRow(rowKey = i, values = c(tcov_estims[i,]))
            }


            tint <- self$results$intercepts

            tint_estims <- estims

            tint_estims <- tint_estims[tint_estims$op == "~1",]

            for(i in seq_along(tint_estims[,1,drop=T])) {
                tint$addRow(rowKey = i, values = c(tint_estims[i,]))
            }


            #tvar <- t_estimates$get("(Error) variances")$get("estimates")
            tvar <- self$results$variances

            tvar_estims <- estims

            tvar_estims <- tvar_estims[tvar_estims$op == "~~",]
            tvar_estims <- tvar_estims[substr(tvar_estims$label, 1, 1) == "v",]

            for(i in seq_along(tvar_estims[,1,drop=T])) {
                tvar$addRow(rowKey = i, values = c(tvar_estims[i,]))
            }


            if(self$options$show_schematic_plot) {
                p <- self$results$schematic_plot
                p$setSize(width = max(c(600, 200*factor_lengths[[1]])), height = 600)
                p$setState(make_schematic(n_waves = factor_lengths[[1]], n_factors = length(factors), return_spec = T,
                                          constrain_autoregressions = self$options$constrain_autoregressions,
                                          constrain_crosslagged = self$options$constrain_crosslagged,
                                          constrain_covariances = self$options$constrain_covariances,
                                          include_random_intercept = self$options$include_random_intercept))
            }

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
