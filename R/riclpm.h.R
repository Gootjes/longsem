
# This file is automatically generated, you probably don't want to edit this

riclpmOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "riclpmOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            factors = list(
                list(label="X", vars=list()),
                list(label="Y", vars=list())),
            show_schematic_plot = TRUE,
            include_random_intercept = FALSE,
            constrain_crosslagged = FALSE,
            constrain_autoregressions = FALSE,
            constrain_residual_variances = FALSE,
            constrain_observed_errors = FALSE,
            constrain_covariances = FALSE,
            constrain_intercepts_over_time = FALSE,
            estimate_observed_intercepts = TRUE,
            estimate_observed_errors = FALSE,
            estimate_latent_intercepts = FALSE,
            estimate_intercepts_intercepts = FALSE,
            fix_random_intercept_first_wave_covariance_to_zero = FALSE,
            show_lavaan_syntax = FALSE,
            show_lavaan_output = FALSE,
            missing_data_treatment = "listwise", ...) {

            super$initialize(
                package="longsem",
                name="riclpm",
                requiresData=TRUE,
                ...)

            private$..factors <- jmvcore::OptionArray$new(
                "factors",
                factors,
                default=list(
                    list(label="X", vars=list()),
                    list(label="Y", vars=list())),
                template=jmvcore::OptionGroup$new(
                    "factors",
                    NULL,
                    elements=list(
                        jmvcore::OptionString$new(
                            "label",
                            NULL),
                        jmvcore::OptionVariables$new(
                            "vars",
                            NULL,
                            suggested=list(
                                "continuous"),
                            permitted=list(
                                "numeric")))))
            private$..show_schematic_plot <- jmvcore::OptionBool$new(
                "show_schematic_plot",
                show_schematic_plot,
                default=TRUE)
            private$..include_random_intercept <- jmvcore::OptionBool$new(
                "include_random_intercept",
                include_random_intercept,
                default=FALSE)
            private$..constrain_crosslagged <- jmvcore::OptionBool$new(
                "constrain_crosslagged",
                constrain_crosslagged,
                default=FALSE)
            private$..constrain_autoregressions <- jmvcore::OptionBool$new(
                "constrain_autoregressions",
                constrain_autoregressions,
                default=FALSE)
            private$..constrain_residual_variances <- jmvcore::OptionBool$new(
                "constrain_residual_variances",
                constrain_residual_variances,
                default=FALSE)
            private$..constrain_observed_errors <- jmvcore::OptionBool$new(
                "constrain_observed_errors",
                constrain_observed_errors,
                default=FALSE)
            private$..constrain_covariances <- jmvcore::OptionBool$new(
                "constrain_covariances",
                constrain_covariances,
                default=FALSE)
            private$..constrain_intercepts_over_time <- jmvcore::OptionBool$new(
                "constrain_intercepts_over_time",
                constrain_intercepts_over_time,
                default=FALSE)
            private$..estimate_observed_intercepts <- jmvcore::OptionBool$new(
                "estimate_observed_intercepts",
                estimate_observed_intercepts,
                default=TRUE)
            private$..estimate_observed_errors <- jmvcore::OptionBool$new(
                "estimate_observed_errors",
                estimate_observed_errors,
                default=FALSE)
            private$..estimate_latent_intercepts <- jmvcore::OptionBool$new(
                "estimate_latent_intercepts",
                estimate_latent_intercepts,
                default=FALSE)
            private$..estimate_intercepts_intercepts <- jmvcore::OptionBool$new(
                "estimate_intercepts_intercepts",
                estimate_intercepts_intercepts,
                default=FALSE)
            private$..fix_random_intercept_first_wave_covariance_to_zero <- jmvcore::OptionBool$new(
                "fix_random_intercept_first_wave_covariance_to_zero",
                fix_random_intercept_first_wave_covariance_to_zero,
                default=FALSE)
            private$..show_lavaan_syntax <- jmvcore::OptionBool$new(
                "show_lavaan_syntax",
                show_lavaan_syntax,
                default=FALSE)
            private$..show_lavaan_output <- jmvcore::OptionBool$new(
                "show_lavaan_output",
                show_lavaan_output,
                default=FALSE)
            private$..missing_data_treatment <- jmvcore::OptionList$new(
                "missing_data_treatment",
                missing_data_treatment,
                options=list(
                    "ml",
                    "listwise"),
                default="listwise")

            self$.addOption(private$..factors)
            self$.addOption(private$..show_schematic_plot)
            self$.addOption(private$..include_random_intercept)
            self$.addOption(private$..constrain_crosslagged)
            self$.addOption(private$..constrain_autoregressions)
            self$.addOption(private$..constrain_residual_variances)
            self$.addOption(private$..constrain_observed_errors)
            self$.addOption(private$..constrain_covariances)
            self$.addOption(private$..constrain_intercepts_over_time)
            self$.addOption(private$..estimate_observed_intercepts)
            self$.addOption(private$..estimate_observed_errors)
            self$.addOption(private$..estimate_latent_intercepts)
            self$.addOption(private$..estimate_intercepts_intercepts)
            self$.addOption(private$..fix_random_intercept_first_wave_covariance_to_zero)
            self$.addOption(private$..show_lavaan_syntax)
            self$.addOption(private$..show_lavaan_output)
            self$.addOption(private$..missing_data_treatment)
        }),
    active = list(
        factors = function() private$..factors$value,
        show_schematic_plot = function() private$..show_schematic_plot$value,
        include_random_intercept = function() private$..include_random_intercept$value,
        constrain_crosslagged = function() private$..constrain_crosslagged$value,
        constrain_autoregressions = function() private$..constrain_autoregressions$value,
        constrain_residual_variances = function() private$..constrain_residual_variances$value,
        constrain_observed_errors = function() private$..constrain_observed_errors$value,
        constrain_covariances = function() private$..constrain_covariances$value,
        constrain_intercepts_over_time = function() private$..constrain_intercepts_over_time$value,
        estimate_observed_intercepts = function() private$..estimate_observed_intercepts$value,
        estimate_observed_errors = function() private$..estimate_observed_errors$value,
        estimate_latent_intercepts = function() private$..estimate_latent_intercepts$value,
        estimate_intercepts_intercepts = function() private$..estimate_intercepts_intercepts$value,
        fix_random_intercept_first_wave_covariance_to_zero = function() private$..fix_random_intercept_first_wave_covariance_to_zero$value,
        show_lavaan_syntax = function() private$..show_lavaan_syntax$value,
        show_lavaan_output = function() private$..show_lavaan_output$value,
        missing_data_treatment = function() private$..missing_data_treatment$value),
    private = list(
        ..factors = NA,
        ..show_schematic_plot = NA,
        ..include_random_intercept = NA,
        ..constrain_crosslagged = NA,
        ..constrain_autoregressions = NA,
        ..constrain_residual_variances = NA,
        ..constrain_observed_errors = NA,
        ..constrain_covariances = NA,
        ..constrain_intercepts_over_time = NA,
        ..estimate_observed_intercepts = NA,
        ..estimate_observed_errors = NA,
        ..estimate_latent_intercepts = NA,
        ..estimate_intercepts_intercepts = NA,
        ..fix_random_intercept_first_wave_covariance_to_zero = NA,
        ..show_lavaan_syntax = NA,
        ..show_lavaan_output = NA,
        ..missing_data_treatment = NA)
)

riclpmResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "riclpmResults",
    inherit = jmvcore::Group,
    active = list(
        factor_definitions = function() private$.items[["factor_definitions"]],
        lavaan_warnings = function() private$.items[["lavaan_warnings"]],
        schematic_plot = function() private$.items[["schematic_plot"]],
        latent_variables = function() private$.items[["latent_variables"]],
        random_intercept_latent_variables = function() private$.items[["random_intercept_latent_variables"]],
        autolagged_paths = function() private$.items[["autolagged_paths"]],
        crosslagged_paths = function() private$.items[["crosslagged_paths"]],
        covariances = function() private$.items[["covariances"]],
        intercepts = function() private$.items[["intercepts"]],
        variances = function() private$.items[["variances"]],
        lavaan_output = function() private$.items[["lavaan_output"]],
        lavaan_syntax = function() private$.items[["lavaan_syntax"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Random Intercept CLPM")
            self$add(jmvcore::Table$new(
                options=options,
                name="factor_definitions",
                title="Factor definitions",
                rows=0,
                columns=list(
                    list(
                        `name`="factor", 
                        `title`="Factor", 
                        `type`="Text"),
                    list(
                        `name`="time", 
                        `title`="Time", 
                        `type`="Text"),
                    list(
                        `name`="variable", 
                        `title`="Variable", 
                        `type`="Text"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="lavaan_warnings",
                title="Lavaan warnings",
                rows=1,
                columns=list(
                    list(
                        `name`="warning_text", 
                        `title`="Warnings", 
                        `type`="Text", 
                        `content`="No warnings from lavaan!"))))
            self$add(jmvcore::Image$new(
                options=options,
                name="schematic_plot",
                title="Schematic",
                visible="(show_schematic_plot)",
                renderFun=".plotSchematic"))
            self$add(jmvcore::Table$new(
                options=options,
                name="latent_variables",
                title="Latent variables",
                rows=0,
                columns=list(
                    list(
                        `name`="lhs", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="op", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="rhs", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="label", 
                        `title`="Label", 
                        `type`="text"),
                    list(
                        `name`="est", 
                        `title`="Estimate", 
                        `type`="number"),
                    list(
                        `name`="se", 
                        `title`="Std.Err", 
                        `type`="number"),
                    list(
                        `name`="z", 
                        `title`="z-value", 
                        `type`="number"),
                    list(
                        `name`="pvalue", 
                        `title`="p(>|z|)", 
                        `type`="number"),
                    list(
                        `name`="ci.lower", 
                        `title`="ci.lwr", 
                        `type`="number"),
                    list(
                        `name`="ci.upper", 
                        `title`="ci.upr", 
                        `type`="number"),
                    list(
                        `name`="std.lv", 
                        `title`="Std.lv", 
                        `type`="number"),
                    list(
                        `name`="std.all", 
                        `title`="Std.all", 
                        `type`="number"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="random_intercept_latent_variables",
                title="Random Intercept Latent variables",
                rows=0,
                columns=list(
                    list(
                        `name`="lhs", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="op", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="rhs", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="label", 
                        `title`="Label", 
                        `type`="text"),
                    list(
                        `name`="est", 
                        `title`="Estimate", 
                        `type`="number"),
                    list(
                        `name`="se", 
                        `title`="Std.Err", 
                        `type`="number"),
                    list(
                        `name`="z", 
                        `title`="z-value", 
                        `type`="number"),
                    list(
                        `name`="pvalue", 
                        `title`="p(>|z|)", 
                        `type`="number"),
                    list(
                        `name`="ci.lower", 
                        `title`="ci.lwr", 
                        `type`="number"),
                    list(
                        `name`="ci.upper", 
                        `title`="ci.upr", 
                        `type`="number"),
                    list(
                        `name`="std.lv", 
                        `title`="Std.lv", 
                        `type`="number"),
                    list(
                        `name`="std.all", 
                        `title`="Std.all", 
                        `type`="number")),
                visible="(include_random_intercept)"))
            self$add(jmvcore::Table$new(
                options=options,
                name="autolagged_paths",
                title="Autolagged paths",
                rows=0,
                columns=list(
                    list(
                        `name`="lhs", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="op", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="rhs", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="label", 
                        `title`="Label", 
                        `type`="text"),
                    list(
                        `name`="est", 
                        `title`="Estimate", 
                        `type`="number"),
                    list(
                        `name`="se", 
                        `title`="Std.Err", 
                        `type`="number"),
                    list(
                        `name`="z", 
                        `title`="z-value", 
                        `type`="number"),
                    list(
                        `name`="pvalue", 
                        `title`="p(>|z|)", 
                        `type`="number"),
                    list(
                        `name`="ci.lower", 
                        `title`="ci.lwr", 
                        `type`="number"),
                    list(
                        `name`="ci.upper", 
                        `title`="ci.upr", 
                        `type`="number"),
                    list(
                        `name`="std.lv", 
                        `title`="Std.lv", 
                        `type`="number"),
                    list(
                        `name`="std.all", 
                        `title`="Std.all", 
                        `type`="number"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="crosslagged_paths",
                title="Crosslagged paths",
                rows=0,
                columns=list(
                    list(
                        `name`="lhs", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="op", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="rhs", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="label", 
                        `title`="Label", 
                        `type`="text"),
                    list(
                        `name`="est", 
                        `title`="Estimate", 
                        `type`="number"),
                    list(
                        `name`="se", 
                        `title`="Std.Err", 
                        `type`="number"),
                    list(
                        `name`="z", 
                        `title`="z-value", 
                        `type`="number"),
                    list(
                        `name`="pvalue", 
                        `title`="p(>|z|)", 
                        `type`="number"),
                    list(
                        `name`="ci.lower", 
                        `title`="ci.lwr", 
                        `type`="number"),
                    list(
                        `name`="ci.upper", 
                        `title`="ci.upr", 
                        `type`="number"),
                    list(
                        `name`="std.lv", 
                        `title`="Std.lv", 
                        `type`="number"),
                    list(
                        `name`="std.all", 
                        `title`="Std.all", 
                        `type`="number"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="covariances",
                title="Covariances",
                rows=0,
                columns=list(
                    list(
                        `name`="lhs", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="op", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="rhs", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="label", 
                        `title`="Label", 
                        `type`="text"),
                    list(
                        `name`="est", 
                        `title`="Estimate", 
                        `type`="number"),
                    list(
                        `name`="se", 
                        `title`="Std.Err", 
                        `type`="number"),
                    list(
                        `name`="z", 
                        `title`="z-value", 
                        `type`="number"),
                    list(
                        `name`="pvalue", 
                        `title`="p(>|z|)", 
                        `type`="number"),
                    list(
                        `name`="ci.lower", 
                        `title`="ci.lwr", 
                        `type`="number"),
                    list(
                        `name`="ci.upper", 
                        `title`="ci.upr", 
                        `type`="number"),
                    list(
                        `name`="std.lv", 
                        `title`="Std.lv", 
                        `type`="number"),
                    list(
                        `name`="std.all", 
                        `title`="Std.all", 
                        `type`="number"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="intercepts",
                title="Intercepts",
                rows=0,
                columns=list(
                    list(
                        `name`="lhs", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="op", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="rhs", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="label", 
                        `title`="Label", 
                        `type`="text"),
                    list(
                        `name`="est", 
                        `title`="Estimate", 
                        `type`="number"),
                    list(
                        `name`="se", 
                        `title`="Std.Err", 
                        `type`="number"),
                    list(
                        `name`="z", 
                        `title`="z-value", 
                        `type`="number"),
                    list(
                        `name`="pvalue", 
                        `title`="p(>|z|)", 
                        `type`="number"),
                    list(
                        `name`="ci.lower", 
                        `title`="ci.lwr", 
                        `type`="number"),
                    list(
                        `name`="ci.upper", 
                        `title`="ci.upr", 
                        `type`="number"),
                    list(
                        `name`="std.lv", 
                        `title`="Std.lv", 
                        `type`="number"),
                    list(
                        `name`="std.all", 
                        `title`="Std.all", 
                        `type`="number"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="variances",
                title="(Error) variances",
                rows=0,
                columns=list(
                    list(
                        `name`="lhs", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="op", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="rhs", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="label", 
                        `title`="Label", 
                        `type`="text"),
                    list(
                        `name`="est", 
                        `title`="Estimate", 
                        `type`="number"),
                    list(
                        `name`="se", 
                        `title`="Std.Err", 
                        `type`="number"),
                    list(
                        `name`="z", 
                        `title`="z-value", 
                        `type`="number"),
                    list(
                        `name`="pvalue", 
                        `title`="p(>|z|)", 
                        `type`="number"),
                    list(
                        `name`="ci.lower", 
                        `title`="ci.lwr", 
                        `type`="number"),
                    list(
                        `name`="ci.upper", 
                        `title`="ci.upr", 
                        `type`="number"),
                    list(
                        `name`="std.lv", 
                        `title`="Std.lv", 
                        `type`="number"),
                    list(
                        `name`="std.all", 
                        `title`="Std.all", 
                        `type`="number"))))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="lavaan_output",
                title="Lavaan output",
                visible="(show_lavaan_output)"))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="lavaan_syntax",
                title="Lavaan syntax",
                visible="(show_lavaan_syntax)"))}))

riclpmBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "riclpmBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "longsem",
                name = "riclpm",
                version = c(1,0,0),
                options = options,
                results = riclpmResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE)
        }))

#' Random Intercept CLPM
#'
#' 
#' @param data the data as a data frame
#' @param factors a list containing named lists that define the \code{label}
#'   of the factor and the \code{vars} that belong to that factor
#' @param show_schematic_plot .
#' @param include_random_intercept .
#' @param constrain_crosslagged .
#' @param constrain_autoregressions .
#' @param constrain_residual_variances .
#' @param constrain_observed_errors .
#' @param constrain_covariances .
#' @param constrain_intercepts_over_time .
#' @param estimate_observed_intercepts .
#' @param estimate_observed_errors .
#' @param estimate_latent_intercepts .
#' @param estimate_intercepts_intercepts .
#' @param fix_random_intercept_first_wave_covariance_to_zero .
#' @param show_lavaan_syntax .
#' @param show_lavaan_output .
#' @param missing_data_treatment .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$factor_definitions} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$lavaan_warnings} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$schematic_plot} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$latent_variables} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$random_intercept_latent_variables} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$autolagged_paths} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$crosslagged_paths} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$covariances} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$intercepts} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$variances} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$lavaan_output} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$lavaan_syntax} \tab \tab \tab \tab \tab a preformatted \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$factor_definitions$asDF}
#'
#' \code{as.data.frame(results$factor_definitions)}
#'
#' @export
riclpm <- function(
    data,
    factors = list(
                list(label="X", vars=list()),
                list(label="Y", vars=list())),
    show_schematic_plot = TRUE,
    include_random_intercept = FALSE,
    constrain_crosslagged = FALSE,
    constrain_autoregressions = FALSE,
    constrain_residual_variances = FALSE,
    constrain_observed_errors = FALSE,
    constrain_covariances = FALSE,
    constrain_intercepts_over_time = FALSE,
    estimate_observed_intercepts = TRUE,
    estimate_observed_errors = FALSE,
    estimate_latent_intercepts = FALSE,
    estimate_intercepts_intercepts = FALSE,
    fix_random_intercept_first_wave_covariance_to_zero = FALSE,
    show_lavaan_syntax = FALSE,
    show_lavaan_output = FALSE,
    missing_data_treatment = "listwise") {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("riclpm requires jmvcore to be installed (restart may be required)")

    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame())


    options <- riclpmOptions$new(
        factors = factors,
        show_schematic_plot = show_schematic_plot,
        include_random_intercept = include_random_intercept,
        constrain_crosslagged = constrain_crosslagged,
        constrain_autoregressions = constrain_autoregressions,
        constrain_residual_variances = constrain_residual_variances,
        constrain_observed_errors = constrain_observed_errors,
        constrain_covariances = constrain_covariances,
        constrain_intercepts_over_time = constrain_intercepts_over_time,
        estimate_observed_intercepts = estimate_observed_intercepts,
        estimate_observed_errors = estimate_observed_errors,
        estimate_latent_intercepts = estimate_latent_intercepts,
        estimate_intercepts_intercepts = estimate_intercepts_intercepts,
        fix_random_intercept_first_wave_covariance_to_zero = fix_random_intercept_first_wave_covariance_to_zero,
        show_lavaan_syntax = show_lavaan_syntax,
        show_lavaan_output = show_lavaan_output,
        missing_data_treatment = missing_data_treatment)

    analysis <- riclpmClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

