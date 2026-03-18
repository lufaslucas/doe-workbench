# R/doe_registry.R — DoE generation registry and helpers

# ---------------------------------------------------------------------------
# DoE Generation Registry
# Hierarchical structure: category -> design_type -> list of functions/examples
# Each entry stores: package, function, description, doc_url, arguments, examples
# ---------------------------------------------------------------------------
doe_registry <- list(

  # =========================================================================
  # COMPARATIVE DESIGNS
  # =========================================================================
  "Comparative" = list(
    label = "Comparative Designs",
    description = "Designs for comparing treatment means (ANOVA-based analysis)",
    designs = list(

      "crd" = list(
        label = "Completely Randomised Design (CRD)",
        entries = list(
          list(
            id = "agricolae_crd",
            package = "agricolae",
            func = "design.crd",
            description = "Completely randomised design \u2014 treatments allocated purely at random",
            doc_url = "https://www.rdocumentation.org/packages/agricolae/topics/design.crd",
            pkg_url = "https://cran.r-project.org/web/packages/agricolae/index.html",
            args = list(
              trt    = list(label = "Treatments (comma-separated)", type = "text",
                            default = "A,B,C"),
              r      = list(label = "Replications per treatment", type = "numeric",
                            default = 4),
              serie  = list(label = "Plot numbering series (0, 1, 2, 3)", type = "numeric",
                            default = 2),
              seed   = list(label = "Random seed", type = "numeric", default = 42),
              kinds  = list(label = "RNG kind", type = "text",
                            default = "Super-Duper")
            ),
            advanced_args = list(
              number = list(label = "Field number for plot labels", type = "numeric", default = 1)
            ),
            examples = list(
              "3 treatments, 4 reps" = list(trt = "A,B,C", r = 4, serie = 2, seed = 42, kinds = "Super-Duper"),
              "5 treatments, 3 reps" = list(trt = "T1,T2,T3,T4,T5", r = 3, serie = 2, seed = 123, kinds = "Super-Duper")
            ),
            build_call = function(args) {
              trt_vec <- trimws(strsplit(args$trt, ",")[[1]])
              extra <- character()
              if (!is.null(args$number) && as.integer(args$number) != 1)
                extra <- c(extra, sprintf("number = %d", as.integer(args$number)))
              extra_str <- if (length(extra)) paste0(", ", paste(extra, collapse = ", ")) else ""
              sprintf('agricolae::design.crd(trt = c(%s), r = %d, seed = %d%s)',
                      paste0('"', trt_vec, '"', collapse = ", "),
                      as.integer(args$r), as.integer(args$seed), extra_str)
            },
            run = function(args) {
              trt_vec <- trimws(strsplit(args$trt, ",")[[1]])
              call_args <- list(trt = trt_vec,
                                r = as.integer(args$r),
                                seed = as.integer(args$seed))
              if (!is.null(args$number) && as.integer(args$number) != 1)
                call_args$number <- as.integer(args$number)
              d <- do.call(agricolae::design.crd, call_args)
              df <- d$book
              names(df) <- gsub("^plots$", "Plot", names(df))
              names(df) <- gsub("^r$", "Rep", names(df))
              trt_col <- setdiff(names(df), c("Plot", "Rep"))
              if (length(trt_col) == 1) names(df)[names(df) == trt_col] <- "Treatment"
              df
            }
          )
        )
      ),

      "rcbd" = list(
        label = "Randomised Complete Block Design (RCBD)",
        entries = list(
          list(
            id = "agricolae_rcbd",
            package = "agricolae",
            func = "design.rcbd",
            description = "RCBD \u2014 each block contains all treatments in random order",
            doc_url = "https://www.rdocumentation.org/packages/agricolae/topics/design.rcbd",
            pkg_url = "https://cran.r-project.org/web/packages/agricolae/index.html",
            args = list(
              trt    = list(label = "Treatments (comma-separated)", type = "text",
                            default = "A,B,C"),
              r      = list(label = "Number of blocks", type = "numeric", default = 4),
              serie  = list(label = "Plot numbering series (0, 1, 2, 3)", type = "numeric",
                            default = 2),
              seed   = list(label = "Random seed", type = "numeric", default = 42),
              kinds  = list(label = "RNG kind", type = "text",
                            default = "Super-Duper"),
              first  = list(label = "Randomise first repetition", type = "logical",
                            default = TRUE)
            ),
            advanced_args = list(
              number   = list(label = "Field number for plot labels", type = "numeric", default = 1),
              continue = list(label = "Continue numbering from previous design", type = "logical", default = FALSE)
            ),
            examples = list(
              "3 treatments, 4 blocks" = list(trt = "A,B,C", r = 4, serie = 2, seed = 42,
                                               kinds = "Super-Duper", first = TRUE),
              "4 treatments, 5 blocks" = list(trt = "Low,Med,High,VHigh", r = 5, serie = 2,
                                               seed = 99, kinds = "Super-Duper", first = TRUE)
            ),
            build_call = function(args) {
              trt_vec <- trimws(strsplit(args$trt, ",")[[1]])
              extra <- character()
              if (!is.null(args$number) && as.integer(args$number) != 1)
                extra <- c(extra, sprintf("number = %d", as.integer(args$number)))
              if (isTRUE(args$continue))
                extra <- c(extra, "continue = TRUE")
              extra_str <- if (length(extra)) paste0(", ", paste(extra, collapse = ", ")) else ""
              sprintf('agricolae::design.rcbd(trt = c(%s), r = %d, serie = %d, seed = %d, first = %s%s)',
                      paste0('"', trt_vec, '"', collapse = ", "),
                      as.integer(args$r), as.integer(args$serie %||% 2),
                      as.integer(args$seed),
                      ifelse(isTRUE(args$first), "TRUE", "FALSE"), extra_str)
            },
            run = function(args) {
              trt_vec <- trimws(strsplit(args$trt, ",")[[1]])
              call_args <- list(trt = trt_vec,
                                r = as.integer(args$r),
                                serie = as.integer(args$serie %||% 2),
                                seed = as.integer(args$seed),
                                first = isTRUE(args$first))
              if (!is.null(args$number) && as.integer(args$number) != 1)
                call_args$number <- as.integer(args$number)
              if (isTRUE(args$continue))
                call_args$continue <- TRUE
              d <- do.call(agricolae::design.rcbd, call_args)
              df <- d$book
              names(df) <- gsub("^plots$", "Plot", names(df))
              names(df) <- gsub("^block$", "Block", names(df))
              trt_col <- setdiff(names(df), c("Plot", "Block"))
              if (length(trt_col) == 1) names(df)[names(df) == trt_col] <- "Treatment"
              df
            }
          )
        )
      ),

      "latin" = list(
        label = "Latin Square Design",
        entries = list(
          list(
            id = "agricolae_latin",
            package = "agricolae",
            func = "design.lsd",
            description = "Latin Square \u2014 controls two sources of variation (rows and columns)",
            doc_url = "https://cran.r-project.org/web/packages/agricolae/agricolae.pdf",
            args = list(
              trt    = list(label = "Treatments (comma-separated)", type = "text",
                            default = "A,B,C,D"),
              seed   = list(label = "Random seed", type = "numeric", default = 42)
            ),
            advanced_args = list(
              serie  = list(label = "Plot numbering series (0, 1, 2, 3)", type = "numeric", default = 2),
              first  = list(label = "Randomise first repetition", type = "logical", default = TRUE),
              kinds  = list(label = "RNG kind", type = "text", default = "Super-Duper"),
              number = list(label = "Field number for plot labels", type = "numeric", default = 1)
            ),
            examples = list(
              "4 treatments" = list(trt = "A,B,C,D", seed = 42),
              "3 treatments" = list(trt = "T1,T2,T3", seed = 99),
              "5 treatments" = list(trt = "V1,V2,V3,V4,V5", seed = 123)
            ),
            build_call = function(args) {
              trt_vec <- trimws(strsplit(args$trt, ",")[[1]])
              extra <- character()
              if (!is.null(args$serie) && as.integer(args$serie) != 2)
                extra <- c(extra, sprintf("serie = %d", as.integer(args$serie)))
              if (!is.null(args$first) && !isTRUE(args$first))
                extra <- c(extra, "first = FALSE")
              if (!is.null(args$kinds) && args$kinds != "Super-Duper")
                extra <- c(extra, sprintf('kinds = "%s"', args$kinds))
              if (!is.null(args$number) && as.integer(args$number) != 1)
                extra <- c(extra, sprintf("number = %d", as.integer(args$number)))
              extra_str <- if (length(extra)) paste0(", ", paste(extra, collapse = ", ")) else ""
              sprintf('agricolae::design.lsd(trt = c(%s), seed = %d%s)',
                      paste0('"', trt_vec, '"', collapse = ", "),
                      as.integer(args$seed), extra_str)
            },
            run = function(args) {
              trt_vec <- trimws(strsplit(args$trt, ",")[[1]])
              call_args <- list(trt = trt_vec,
                                seed = as.integer(args$seed))
              if (!is.null(args$serie) && as.integer(args$serie) != 2)
                call_args$serie <- as.integer(args$serie)
              if (!is.null(args$first) && !isTRUE(args$first))
                call_args$first <- FALSE
              if (!is.null(args$kinds) && args$kinds != "Super-Duper")
                call_args$kinds <- args$kinds
              if (!is.null(args$number) && as.integer(args$number) != 1)
                call_args$number <- as.integer(args$number)
              d <- do.call(agricolae::design.lsd, call_args)
              df <- d$book
              names(df) <- gsub("^plots$", "Plot", names(df))
              names(df) <- gsub("^row$", "Row", names(df))
              names(df) <- gsub("^col$", "Col", names(df))
              trt_col <- setdiff(names(df), c("Plot", "Row", "Col"))
              if (length(trt_col) == 1) names(df)[names(df) == trt_col] <- "Treatment"
              df
            }
          )
        )
      ),

      "bib" = list(
        label = "Balanced Incomplete Block Design (BIB)",
        entries = list(
          list(
            id = "agricolae_bib",
            package = "agricolae",
            func = "design.bib",
            description = "BIB \u2014 each block contains a subset of treatments; all pairs appear equally often",
            doc_url = "https://cran.r-project.org/web/packages/agricolae/agricolae.pdf",
            args = list(
              trt    = list(label = "Treatments (comma-separated)", type = "text",
                            default = "A,B,C,D"),
              k      = list(label = "Block size (treatments per block)", type = "numeric",
                            default = 3),
              seed   = list(label = "Random seed", type = "numeric", default = 42)
            ),
            advanced_args = list(
              r      = list(label = "Replications per pair (NULL = auto)", type = "numeric", default = NA),
              serie  = list(label = "Plot numbering series (0, 1, 2, 3)", type = "numeric", default = 2),
              kinds  = list(label = "RNG kind", type = "text", default = "Super-Duper"),
              maxRep = list(label = "Max search repetitions", type = "numeric", default = 20),
              number = list(label = "Field number for plot labels", type = "numeric", default = 1)
            ),
            examples = list(
              "4 treatments, blocks of 3" = list(trt = "A,B,C,D", k = 3, seed = 42),
              "6 treatments, blocks of 3" = list(trt = "T1,T2,T3,T4,T5,T6", k = 3, seed = 99)
            ),
            build_call = function(args) {
              trt_vec <- trimws(strsplit(args$trt, ",")[[1]])
              extra <- character()
              if (!is.null(args$r) && !is.na(args$r) && args$r > 0)
                extra <- c(extra, sprintf("r = %d", as.integer(args$r)))
              if (!is.null(args$serie) && as.integer(args$serie) != 2)
                extra <- c(extra, sprintf("serie = %d", as.integer(args$serie)))
              if (!is.null(args$kinds) && args$kinds != "Super-Duper")
                extra <- c(extra, sprintf('kinds = "%s"', args$kinds))
              if (!is.null(args$maxRep) && as.integer(args$maxRep) != 20)
                extra <- c(extra, sprintf("maxRep = %d", as.integer(args$maxRep)))
              if (!is.null(args$number) && as.integer(args$number) != 1)
                extra <- c(extra, sprintf("number = %d", as.integer(args$number)))
              extra_str <- if (length(extra)) paste0(", ", paste(extra, collapse = ", ")) else ""
              sprintf('agricolae::design.bib(trt = c(%s), k = %d, seed = %d%s)',
                      paste0('"', trt_vec, '"', collapse = ", "),
                      as.integer(args$k), as.integer(args$seed), extra_str)
            },
            run = function(args) {
              trt_vec <- trimws(strsplit(args$trt, ",")[[1]])
              call_args <- list(trt = trt_vec,
                                k = as.integer(args$k),
                                seed = as.integer(args$seed))
              if (!is.null(args$r) && !is.na(args$r) && args$r > 0)
                call_args$r <- as.integer(args$r)
              if (!is.null(args$serie) && as.integer(args$serie) != 2)
                call_args$serie <- as.integer(args$serie)
              if (!is.null(args$kinds) && args$kinds != "Super-Duper")
                call_args$kinds <- args$kinds
              if (!is.null(args$maxRep) && as.integer(args$maxRep) != 20)
                call_args$maxRep <- as.integer(args$maxRep)
              if (!is.null(args$number) && as.integer(args$number) != 1)
                call_args$number <- as.integer(args$number)
              d <- do.call(agricolae::design.bib, call_args)
              df <- d$book
              names(df) <- gsub("^plots$", "Plot", names(df))
              names(df) <- gsub("^block$", "Block", names(df))
              trt_col <- setdiff(names(df), c("Plot", "Block"))
              if (length(trt_col) == 1) names(df)[names(df) == trt_col] <- "Treatment"
              df
            }
          )
        )
      )
    )
  ),

  # =========================================================================
  # SCREENING DESIGNS
  # =========================================================================
  "Screening" = list(
    label = "Screening Designs",
    description = "Designs for identifying important factors from many candidates",
    designs = list(

      "fractional_factorial" = list(
        label = "Fractional Factorial (2-level)",
        entries = list(
          list(
            id = "FrF2_design",
            package = "FrF2",
            func = "FrF2",
            description = "Regular 2-level fractional factorial designs (Resolution III\u2013V)",
            doc_url = "https://cran.r-project.org/web/packages/FrF2/FrF2.pdf",
            args = list(
              nruns    = list(label = "Number of runs", type = "numeric", default = 8),
              nfactors = list(label = "Number of factors", type = "numeric", default = 4),
              randomize = list(label = "Randomise run order", type = "logical", default = TRUE),
              seed     = list(label = "Random seed", type = "numeric", default = 42)
            ),
            advanced_args = list(
              resolution    = list(label = "Design resolution (ignored if nruns specified)", type = "numeric", default = NA),
              generators    = list(label = "Generator columns (requires nruns; e.g. ABCD, ABE)", type = "text", default = ""),
              ncenter       = list(label = "Centre points (quantitative factors only)", type = "numeric", default = 0),
              replications  = list(label = "Replications", type = "numeric", default = 1),
              repeat.only   = list(label = "Repeat measurements (only if replications > 1)", type = "logical", default = FALSE),
              default.levels = list(label = "Default factor levels (e.g. -1,1)", type = "text", default = "-1,1"),
              MaxC2         = list(label = "Minimise aberration (maximise clear 2FIs)", type = "logical", default = FALSE),
              alias.info    = list(label = "Alias information level (2 or 3)", type = "numeric", default = 2)
            ),
            examples = list(
              "2^(4-1) = 8 runs, 4 factors" = list(nruns = 8, nfactors = 4,
                                                     randomize = TRUE, seed = 42),
              "2^(7-4) = 8 runs, 7 factors" = list(nruns = 8, nfactors = 7,
                                                     randomize = TRUE, seed = 99),
              "2^(5-1) = 16 runs, 5 factors" = list(nruns = 16, nfactors = 5,
                                                      randomize = TRUE, seed = 123)
            ),
            build_call = function(args) {
              extra <- character()
              if (!is.null(args$resolution) && !is.na(args$resolution) && args$resolution > 0)
                extra <- c(extra, sprintf("resolution = %d", as.integer(args$resolution)))
              if (!is.null(args$generators) && nzchar(args$generators))
                extra <- c(extra, sprintf('generators = c(%s)',
                  paste0('"', trimws(strsplit(args$generators, ",")[[1]]), '"', collapse = ", ")))
              if (!is.null(args$ncenter) && as.integer(args$ncenter) > 0)
                extra <- c(extra, sprintf("ncenter = %d", as.integer(args$ncenter)))
              if (!is.null(args$replications) && as.integer(args$replications) > 1)
                extra <- c(extra, sprintf("replications = %d", as.integer(args$replications)))
              if (isTRUE(args$repeat.only))
                extra <- c(extra, "repeat.only = TRUE")
              if (!is.null(args$default.levels) && args$default.levels != "-1,1") {
                lvls <- trimws(strsplit(args$default.levels, ",")[[1]])
                extra <- c(extra, sprintf("default.levels = c(%s)", paste(lvls, collapse = ", ")))
              }
              if (isTRUE(args$MaxC2))
                extra <- c(extra, "MaxC2 = TRUE")
              if (!is.null(args$alias.info) && as.integer(args$alias.info) != 2)
                extra <- c(extra, sprintf("alias.info = %d", as.integer(args$alias.info)))
              extra_str <- if (length(extra)) paste0(",\n  ", paste(extra, collapse = ",\n  ")) else ""
              sprintf('FrF2::FrF2(nruns = %d, nfactors = %d, randomize = %s, seed = %d%s)',
                      as.integer(args$nruns), as.integer(args$nfactors),
                      ifelse(isTRUE(args$randomize), "TRUE", "FALSE"),
                      as.integer(args$seed), extra_str)
            },
            run = function(args) {
              call_args <- list(nruns = as.integer(args$nruns),
                                nfactors = as.integer(args$nfactors),
                                randomize = isTRUE(args$randomize),
                                seed = as.integer(args$seed))
              if (!is.null(args$resolution) && !is.na(args$resolution) && args$resolution > 0)
                call_args$resolution <- as.integer(args$resolution)
              if (!is.null(args$generators) && nzchar(args$generators))
                call_args$generators <- trimws(strsplit(args$generators, ",")[[1]])
              if (!is.null(args$ncenter) && as.integer(args$ncenter) > 0)
                call_args$ncenter <- as.integer(args$ncenter)
              if (!is.null(args$replications) && as.integer(args$replications) > 1)
                call_args$replications <- as.integer(args$replications)
              if (isTRUE(args$repeat.only))
                call_args$repeat.only <- TRUE
              if (!is.null(args$default.levels) && args$default.levels != "-1,1") {
                call_args$default.levels <- as.numeric(trimws(strsplit(args$default.levels, ",")[[1]]))
              }
              if (isTRUE(args$MaxC2))
                call_args$MaxC2 <- TRUE
              if (!is.null(args$alias.info) && as.integer(args$alias.info) != 2)
                call_args$alias.info <- as.integer(args$alias.info)
              d <- do.call(FrF2::FrF2, call_args)
              di <- DoE.base::design.info(d)
              df <- as.data.frame(d)
              # Convert factor columns to numeric -1/+1
              for (cn in names(df)) {
                if (is.factor(df[[cn]])) {
                  df[[cn]] <- as.numeric(as.character(df[[cn]]))
                }
              }
              df$RunOrder <- seq_len(nrow(df))
              # Attach design metadata
              attr(df, "design_resolution") <- if (!is.null(di$catlg.entry))
                di$catlg.entry[[1]]$res else NA
              attr(df, "design_aliased") <- di$aliased
              df
            }
          )
        )
      ),

      "plackett_burman" = list(
        label = "Plackett-Burman",
        entries = list(
          list(
            id = "PB_design",
            package = "FrF2",
            func = "pb",
            description = "Plackett-Burman screening designs (Resolution III, run count a multiple of 4)",
            doc_url = "https://cran.r-project.org/web/packages/FrF2/FrF2.pdf",
            args = list(
              nruns    = list(label = "Number of runs (multiple of 4)", type = "numeric", default = 12),
              nfactors = list(label = "Number of factors", type = "numeric", default = 11),
              randomize = list(label = "Randomise run order", type = "logical", default = TRUE),
              seed     = list(label = "Random seed", type = "numeric", default = 42)
            ),
            advanced_args = list(
              ncenter       = list(label = "Centre points (quantitative factors only)", type = "numeric", default = 0),
              replications  = list(label = "Replications", type = "numeric", default = 1),
              repeat.only   = list(label = "Repeat measurements (only if replications > 1)", type = "logical", default = FALSE),
              default.levels = list(label = "Default factor levels (e.g. -1,1)", type = "text", default = "-1,1")
            ),
            examples = list(
              "12 runs, 11 factors" = list(nruns = 12, nfactors = 11,
                                            randomize = TRUE, seed = 42),
              "20 runs, 19 factors" = list(nruns = 20, nfactors = 19,
                                            randomize = TRUE, seed = 99),
              "12 runs, 6 factors" = list(nruns = 12, nfactors = 6,
                                           randomize = TRUE, seed = 123)
            ),
            build_call = function(args) {
              extra <- character()
              if (!is.null(args$ncenter) && as.integer(args$ncenter) > 0)
                extra <- c(extra, sprintf("ncenter = %d", as.integer(args$ncenter)))
              if (!is.null(args$replications) && as.integer(args$replications) > 1)
                extra <- c(extra, sprintf("replications = %d", as.integer(args$replications)))
              if (isTRUE(args$repeat.only))
                extra <- c(extra, "repeat.only = TRUE")
              if (!is.null(args$default.levels) && args$default.levels != "-1,1") {
                lvls <- trimws(strsplit(args$default.levels, ",")[[1]])
                extra <- c(extra, sprintf("default.levels = c(%s)", paste(lvls, collapse = ", ")))
              }
              extra_str <- if (length(extra)) paste0(", ", paste(extra, collapse = ", ")) else ""
              sprintf('FrF2::pb(nruns = %d, nfactors = %d, randomize = %s, seed = %d%s)',
                      as.integer(args$nruns), as.integer(args$nfactors),
                      ifelse(isTRUE(args$randomize), "TRUE", "FALSE"),
                      as.integer(args$seed), extra_str)
            },
            run = function(args) {
              call_args <- list(nruns = as.integer(args$nruns),
                                nfactors = as.integer(args$nfactors),
                                randomize = isTRUE(args$randomize),
                                seed = as.integer(args$seed))
              if (!is.null(args$ncenter) && as.integer(args$ncenter) > 0)
                call_args$ncenter <- as.integer(args$ncenter)
              if (!is.null(args$replications) && as.integer(args$replications) > 1)
                call_args$replications <- as.integer(args$replications)
              if (isTRUE(args$repeat.only))
                call_args$repeat.only <- TRUE
              if (!is.null(args$default.levels) && args$default.levels != "-1,1")
                call_args$default.levels <- as.numeric(trimws(strsplit(args$default.levels, ",")[[1]]))
              d <- do.call(FrF2::pb, call_args)
              df <- as.data.frame(d)
              for (cn in names(df)) {
                if (is.factor(df[[cn]])) {
                  df[[cn]] <- as.numeric(as.character(df[[cn]]))
                }
              }
              df$RunOrder <- seq_len(nrow(df))
              # PB is Resolution III by construction
              attr(df, "design_resolution") <- 3
              attr(df, "design_aliased") <- NULL
              df
            }
          )
        )
      )
    )
  ),

  # =========================================================================
  # OPTIMISATION DESIGNS (RSM)
  # =========================================================================
  "Optimisation" = list(
    label = "Optimisation Designs (RSM)",
    description = "Response surface designs for fitting quadratic models and finding optima",
    designs = list(

      "ccd" = list(
        label = "Central Composite Design (CCD)",
        entries = list(
          list(
            id = "rsm_ccd",
            package = "rsm",
            func = "ccd",
            description = "Central composite design with factorial + axial + centre points",
            doc_url = "https://cran.r-project.org/web/packages/rsm/rsm.pdf",
            args = list(
              basis     = list(label = "Number of factors", type = "numeric", default = 2),
              n0        = list(label = "Number of centre points", type = "numeric", default = 3),
              alpha     = list(label = 'Alpha type ("rotatable", "orthogonal", "faces")',
                                type = "text", default = "rotatable"),
              randomize = list(label = "Randomise run order", type = "logical", default = TRUE)
            ),
            advanced_args = list(
              inscribed  = list(label = "Inscribed (axial at \u00b11, cube inside)", type = "logical", default = FALSE),
              wbreps     = list(label = "Within-block replicates (factorial portion)", type = "numeric", default = 1),
              bbreps     = list(label = "Between-block replicates", type = "numeric", default = 1),
              oneblock   = list(label = "Force single block (merge cube + star)", type = "logical", default = FALSE),
              generators = list(label = "Fractional generator formula (e.g. x3 ~ x1*x2)", type = "text", default = "")
            ),
            examples = list(
              "2 factors, rotatable, 3 centres" = list(basis = 2, n0 = 3,
                                                        alpha = "rotatable", randomize = TRUE),
              "3 factors, face-centred, 4 centres" = list(basis = 3, n0 = 4,
                                                           alpha = "faces", randomize = TRUE),
              "2 factors, orthogonal, 5 centres" = list(basis = 2, n0 = 5,
                                                         alpha = "orthogonal", randomize = TRUE)
            ),
            build_call = function(args) {
              extra <- character()
              if (isTRUE(args$inscribed))
                extra <- c(extra, "inscribed = TRUE")
              if (!is.null(args$wbreps) && as.integer(args$wbreps) > 1)
                extra <- c(extra, sprintf("wbreps = %d", as.integer(args$wbreps)))
              if (!is.null(args$bbreps) && as.integer(args$bbreps) > 1)
                extra <- c(extra, sprintf("bbreps = %d", as.integer(args$bbreps)))
              if (isTRUE(args$oneblock))
                extra <- c(extra, "oneblock = TRUE")
              if (!is.null(args$generators) && nzchar(args$generators))
                extra <- c(extra, sprintf('generators = %s', args$generators))
              extra_str <- if (length(extra)) paste0(",\n  ", paste(extra, collapse = ",\n  ")) else ""
              sprintf('rsm::ccd(basis = %d, n0 = %d, alpha = "%s", randomize = %s%s)',
                      as.integer(args$basis), as.integer(args$n0),
                      args$alpha,
                      ifelse(isTRUE(args$randomize), "TRUE", "FALSE"), extra_str)
            },
            run = function(args) {
              call_args <- list(basis = as.integer(args$basis),
                                n0 = as.integer(args$n0),
                                alpha = args$alpha,
                                randomize = isTRUE(args$randomize))
              if (isTRUE(args$inscribed))
                call_args$inscribed <- TRUE
              if (!is.null(args$wbreps) && as.integer(args$wbreps) > 1)
                call_args$wbreps <- as.integer(args$wbreps)
              if (!is.null(args$bbreps) && as.integer(args$bbreps) > 1)
                call_args$bbreps <- as.integer(args$bbreps)
              if (isTRUE(args$oneblock))
                call_args$oneblock <- TRUE
              if (!is.null(args$generators) && nzchar(args$generators))
                call_args$generators <- as.formula(args$generators)
              d <- do.call(rsm::ccd, call_args)
              df <- as.data.frame(d)
              # Remove rsm-specific columns if present
              drop_cols <- c("run.order", "std.order", "Block")
              for (dc in drop_cols) {
                if (dc %in% names(df)) {
                  if (dc == "run.order") {
                    df$RunOrder <- df[[dc]]
                  }
                  df[[dc]] <- NULL
                }
              }
              if (!"RunOrder" %in% names(df)) df$RunOrder <- seq_len(nrow(df))
              attr(df, "design_type") <- "rsm"
              df
            }
          )
        )
      ),

      "bbd" = list(
        label = "Box-Behnken Design (BBD)",
        entries = list(
          list(
            id = "rsm_bbd",
            package = "rsm",
            func = "bbd",
            description = "Box-Behnken design \u2014 3-level design without extreme corners",
            doc_url = "https://cran.r-project.org/web/packages/rsm/rsm.pdf",
            args = list(
              k         = list(label = "Number of factors", type = "numeric", default = 3),
              n0        = list(label = "Number of centre points", type = "numeric", default = 3),
              randomize = list(label = "Randomise run order", type = "logical", default = TRUE)
            ),
            advanced_args = list(
              block = list(label = "Include blocking (supported for 4\u20135 factors)", type = "logical", default = TRUE)
            ),
            examples = list(
              "3 factors, 3 centres" = list(k = 3, n0 = 3, randomize = TRUE),
              "4 factors, 4 centres" = list(k = 4, n0 = 4, randomize = TRUE)
            ),
            build_call = function(args) {
              extra <- character()
              if (!is.null(args$block) && !isTRUE(args$block))
                extra <- c(extra, "block = FALSE")
              extra_str <- if (length(extra)) paste0(", ", paste(extra, collapse = ", ")) else ""
              sprintf('rsm::bbd(k = %d, n0 = %d, randomize = %s%s)',
                      as.integer(args$k), as.integer(args$n0),
                      ifelse(isTRUE(args$randomize), "TRUE", "FALSE"), extra_str)
            },
            run = function(args) {
              call_args <- list(k = as.integer(args$k),
                                n0 = as.integer(args$n0),
                                randomize = isTRUE(args$randomize))
              if (!is.null(args$block) && !isTRUE(args$block))
                call_args$block <- FALSE
              d <- do.call(rsm::bbd, call_args)
              df <- as.data.frame(d)
              drop_cols <- c("run.order", "std.order", "Block")
              for (dc in drop_cols) {
                if (dc %in% names(df)) {
                  if (dc == "run.order") df$RunOrder <- df[[dc]]
                  df[[dc]] <- NULL
                }
              }
              if (!"RunOrder" %in% names(df)) df$RunOrder <- seq_len(nrow(df))
              attr(df, "design_type") <- "rsm"
              df
            }
          )
        )
      )
    )
  ),

  # =========================================================================
  # OPTIMAL DESIGNS
  # =========================================================================
  "Optimal" = list(
    label = "Optimal Designs",
    description = "Computer-generated designs optimising a statistical criterion (D, I, A, ...)",
    designs = list(

      "d_optimal" = list(
        label = "D-Optimal Design",
        entries = list(
          list(
            id = "algdesign_dopt",
            package = "AlgDesign",
            func = "optFederov",
            description = "D-optimal design \u2014 maximises |X'X| for a given model and candidate set",
            doc_url = "https://cran.r-project.org/web/packages/AlgDesign/AlgDesign.pdf",
            args = list(
              formula   = list(label = "Model formula (RHS only, e.g. ~quad(.))", type = "text",
                                default = "~quad(.)"),
              nfactors  = list(label = "Number of factors", type = "numeric", default = 2),
              nlevels   = list(label = "Levels per factor", type = "numeric", default = 3),
              ntrials   = list(label = "Number of runs (design points)", type = "numeric",
                                default = 12),
              criterion = list(label = "Optimality criterion (D, I, A)", type = "text",
                                default = "D"),
              seed      = list(label = "Random seed", type = "numeric", default = 42)
            ),
            advanced_args = list(
              center       = list(label = "Include centre point", type = "logical", default = FALSE),
              approximate  = list(label = "Approximate (continuous) design", type = "logical", default = FALSE),
              maxIteration = list(label = "Max iterations per search", type = "numeric", default = 100),
              nRepeats     = list(label = "Number of random restarts", type = "numeric", default = 5),
              evaluateI    = list(label = "Evaluate I-optimality", type = "logical", default = FALSE),
              augment      = list(label = "Augment existing design", type = "logical", default = FALSE)
            ),
            examples = list(
              "2 factors, quadratic, 12 runs" = list(formula = "~quad(.)", nfactors = 2,
                                                      nlevels = 5, ntrials = 12,
                                                      criterion = "D", seed = 42),
              "3 factors, linear + 2FI, 15 runs" = list(formula = "~.^2", nfactors = 3,
                                                         nlevels = 5, ntrials = 15,
                                                         criterion = "D", seed = 99)
            ),
            build_call = function(args) {
              extra <- character()
              if (isTRUE(args$center))
                extra <- c(extra, "center = TRUE")
              if (isTRUE(args$approximate))
                extra <- c(extra, "approximate = TRUE")
              if (!is.null(args$maxIteration) && as.integer(args$maxIteration) != 100)
                extra <- c(extra, sprintf("maxIteration = %d", as.integer(args$maxIteration)))
              if (!is.null(args$nRepeats) && as.integer(args$nRepeats) != 5)
                extra <- c(extra, sprintf("nRepeats = %d", as.integer(args$nRepeats)))
              if (isTRUE(args$evaluateI))
                extra <- c(extra, "evaluateI = TRUE")
              if (isTRUE(args$augment))
                extra <- c(extra, "augment = TRUE")
              extra_str <- if (length(extra)) paste0(",\n  ", paste(extra, collapse = ",\n  ")) else ""
              sprintf(paste0('AlgDesign::optFederov(\n',
                             '  frml = %s,\n',
                             '  data = AlgDesign::gen.factorial(%d, %d, varNames = paste0("x", 1:%d)),\n',
                             '  nTrials = %d,\n',
                             '  criterion = "%s"%s\n)'),
                      args$formula,
                      as.integer(args$nlevels), as.integer(args$nfactors),
                      as.integer(args$nfactors),
                      as.integer(args$ntrials), args$criterion, extra_str)
            },
            run = function(args) {
              set.seed(as.integer(args$seed))
              nf <- as.integer(args$nfactors)
              nl <- as.integer(args$nlevels)
              candidates <- AlgDesign::gen.factorial(nl, nf,
                                                      varNames = paste0("x", seq_len(nf)))
              frml <- as.formula(args$formula)
              call_args <- list(frml = frml,
                                data = candidates,
                                nTrials = as.integer(args$ntrials),
                                criterion = args$criterion)
              if (isTRUE(args$center))
                call_args$center <- TRUE
              if (isTRUE(args$approximate))
                call_args$approximate <- TRUE
              if (!is.null(args$maxIteration) && as.integer(args$maxIteration) != 100)
                call_args$maxIteration <- as.integer(args$maxIteration)
              if (!is.null(args$nRepeats) && as.integer(args$nRepeats) != 5)
                call_args$nRepeats <- as.integer(args$nRepeats)
              if (isTRUE(args$evaluateI))
                call_args$evaluateI <- TRUE
              if (isTRUE(args$augment))
                call_args$augment <- TRUE
              d <- do.call(AlgDesign::optFederov, call_args)
              df <- d$design
              df$RunOrder <- seq_len(nrow(df))
              rownames(df) <- NULL
              df
            }
          )
        )
      )
    )
  ),

  # =========================================================================
  # SPACE FILLING DESIGNS
  # =========================================================================
  "Space Filling" = list(
    label = "Space Filling Designs",
    description = "Designs that spread points uniformly across the design space (for computer experiments)",
    designs = list(

      "lhs" = list(
        label = "Latin Hypercube Sampling (LHS)",
        entries = list(
          list(
            id = "lhs_random",
            package = "lhs",
            func = "randomLHS",
            description = "Random Latin Hypercube \u2014 ensures each factor has equally spaced marginals",
            doc_url = "https://cran.r-project.org/web/packages/lhs/lhs.pdf",
            args = list(
              n = list(label = "Number of runs", type = "numeric", default = 20),
              k = list(label = "Number of factors", type = "numeric", default = 3),
              seed = list(label = "Random seed", type = "numeric", default = 42)
            ),
            advanced_args = list(
              preserveDraw = list(label = "Preserve random draw (reproducible augmentation)", type = "logical", default = FALSE)
            ),
            examples = list(
              "20 runs, 3 factors" = list(n = 20, k = 3, seed = 42),
              "30 runs, 5 factors" = list(n = 30, k = 5, seed = 99)
            ),
            build_call = function(args) {
              extra <- character()
              if (isTRUE(args$preserveDraw))
                extra <- c(extra, "preserveDraw = TRUE")
              extra_str <- if (length(extra)) paste0(", ", paste(extra, collapse = ", ")) else ""
              sprintf('lhs::randomLHS(n = %d, k = %d%s)',
                      as.integer(args$n), as.integer(args$k), extra_str)
            },
            run = function(args) {
              set.seed(as.integer(args$seed))
              call_args <- list(n = as.integer(args$n),
                                k = as.integer(args$k))
              if (isTRUE(args$preserveDraw))
                call_args$preserveDraw <- TRUE
              mat <- do.call(lhs::randomLHS, call_args)
              df <- as.data.frame(mat)
              names(df) <- paste0("x", seq_len(ncol(df)))
              df$RunOrder <- seq_len(nrow(df))
              df
            }
          ),
          list(
            id = "lhs_maximin",
            package = "lhs",
            func = "maximinLHS",
            description = "Maximin LHS \u2014 maximises minimum distance between points",
            doc_url = "https://cran.r-project.org/web/packages/lhs/lhs.pdf",
            args = list(
              n = list(label = "Number of runs", type = "numeric", default = 20),
              k = list(label = "Number of factors", type = "numeric", default = 3),
              seed = list(label = "Random seed", type = "numeric", default = 42)
            ),
            advanced_args = list(
              method      = list(label = "Optimisation method", type = "select",
                                  default = "build", choices = c("build", "iterative")),
              dup         = list(label = "Duplication factor (build method only)", type = "numeric", default = 1),
              eps         = list(label = "Convergence tolerance (iterative only)", type = "numeric", default = 0.05),
              maxIter     = list(label = "Max iterations (iterative only)", type = "numeric", default = 100),
              optimize.on = list(label = "Optimise on distance metric", type = "select",
                                  default = "grid", choices = c("grid", "result"))
            ),
            examples = list(
              "20 runs, 3 factors" = list(n = 20, k = 3, seed = 42),
              "30 runs, 5 factors" = list(n = 30, k = 5, seed = 99)
            ),
            build_call = function(args) {
              extra <- character()
              if (!is.null(args$method) && args$method != "build")
                extra <- c(extra, sprintf('method = "%s"', args$method))
              if (!is.null(args$dup) && as.integer(args$dup) != 1)
                extra <- c(extra, sprintf("dup = %d", as.integer(args$dup)))
              if (!is.null(args$eps) && as.numeric(args$eps) != 0.05)
                extra <- c(extra, sprintf("eps = %s", args$eps))
              if (!is.null(args$maxIter) && as.integer(args$maxIter) != 100)
                extra <- c(extra, sprintf("maxIter = %d", as.integer(args$maxIter)))
              if (!is.null(args$optimize.on) && args$optimize.on != "grid")
                extra <- c(extra, sprintf('optimize.on = "%s"', args$optimize.on))
              extra_str <- if (length(extra)) paste0(", ", paste(extra, collapse = ", ")) else ""
              sprintf('lhs::maximinLHS(n = %d, k = %d%s)',
                      as.integer(args$n), as.integer(args$k), extra_str)
            },
            run = function(args) {
              set.seed(as.integer(args$seed))
              call_args <- list(n = as.integer(args$n),
                                k = as.integer(args$k))
              if (!is.null(args$method) && args$method != "build")
                call_args$method <- args$method
              if (!is.null(args$dup) && as.integer(args$dup) != 1)
                call_args$dup <- as.integer(args$dup)
              if (!is.null(args$eps) && as.numeric(args$eps) != 0.05)
                call_args$eps <- as.numeric(args$eps)
              if (!is.null(args$maxIter) && as.integer(args$maxIter) != 100)
                call_args$maxIter <- as.integer(args$maxIter)
              if (!is.null(args$optimize.on) && args$optimize.on != "grid")
                call_args$optimize.on <- args$optimize.on
              mat <- do.call(lhs::maximinLHS, call_args)
              df <- as.data.frame(mat)
              names(df) <- paste0("x", seq_len(ncol(df)))
              df$RunOrder <- seq_len(nrow(df))
              df
            }
          )
        )
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Helper: flatten the registry into a lookup by entry ID
# ---------------------------------------------------------------------------
doe_entry_lookup <- function() {
  lookup <- list()
  for (cat_name in names(doe_registry)) {
    cat <- doe_registry[[cat_name]]
    for (des_name in names(cat$designs)) {
      des <- cat$designs[[des_name]]
      for (entry in des$entries) {
        lookup[[entry$id]] <- list(
          category = cat_name,
          design = des_name,
          design_label = des$label,
          entry = entry
        )
      }
    }
  }
  lookup
}

# Helper: get category choices for the UI
doe_category_choices <- function() {
  setNames(names(doe_registry),
           sapply(doe_registry, function(x) x$label))
}

# Helper: get design choices for a category
doe_design_choices <- function(category) {
  if (is.null(category) || !category %in% names(doe_registry)) return(NULL)
  designs <- doe_registry[[category]]$designs
  setNames(names(designs), sapply(designs, function(x) x$label))
}

# Helper: get entry choices for a design within a category
doe_entry_choices <- function(category, design) {
  if (is.null(category) || is.null(design)) return(NULL)
  if (!category %in% names(doe_registry)) return(NULL)
  designs <- doe_registry[[category]]$designs
  if (!design %in% names(designs)) return(NULL)
  entries <- designs[[design]]$entries
  choices <- setNames(
    sapply(entries, function(e) e$id),
    sapply(entries, function(e) paste0(e$package, "::", e$func))
  )
  choices
}

# Helper: get example choices for an entry
doe_example_choices <- function(entry_id) {
  lookup <- doe_entry_lookup()
  if (!entry_id %in% names(lookup)) return(NULL)
  entry <- lookup[[entry_id]]$entry
  if (length(entry$examples) == 0) return(NULL)
  setNames(names(entry$examples), names(entry$examples))
}
