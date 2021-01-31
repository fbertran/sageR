devtools::check(
  manual = TRUE,
  remote = TRUE,
  incoming = TRUE
)

devtools::test()
devtools::test_coverage()




update_yaml <- function(mypkg, overwrite = FALSE) {
  require(yaml)
  #   _____________________________________________________________________
  #   Find currently missing functions in yml file                    ####
  curr_yaml     <- yaml.load_file("_pkgdown.yml")
  curr_yaml_ref <- curr_yaml[["reference"]]
  curr_funcs <- unlist(lapply(curr_yaml_ref,
                              FUN = function(x) (x$contents))) %>%
    gsub('`', "", .)
  all_pkgfuncs <- ls(paste0("package:", mypkg))
  miss_funcs   <- setdiff(pkg_funcs, curr_funcs)

  if (length(miss_funcs) == 0) {
    message("All functions are already in _pkgdown.yml")
  } else {

    #   _________________________________________________________________
    #   Look if an "Other" section already exists                     ####

    titles     <- unlist(lapply(curr_yaml_ref, FUN = function(x) (x$title)))
    other_sect <- which(titles == "Other")

    if (!length(other_sect) == 0) {
      #   _________________________________________________________________
      #   If the "Other" sect already exists, append missing functions ####

      message(strwrap(paste(
        "Adding ", paste0("`", miss_funcs, "` ", collapse = ""),
        "to _pkgdown.yml")))
      curr_yaml_ref[[other_sect]] = list(
        title = "Other",
        desc  = "Other Functions",
        contents = c(curr_yaml_ref[[other_sect]]$contents,
                     paste0("`", miss_funcs, "`"))
      )

    } else {

      #   _____________________________________________________________
      #   Otherwise, create the "other" section and add            ####

      message("Creating the \"Others\" section")
      message(strwrap(paste(
        "Adding ", paste0("`", miss_funcs, "` ", collapse = ""),
        "to _pkgdown.yml")))
      curr_yaml_ref[[length(curr_yaml_ref) + 1]] = list(
        title = "Other",
        desc  = "Other Functions",
        contents = paste0("`", miss_funcs, "`"))
    }
    curr_yaml[["reference"]] <- curr_yaml_ref
    if (overwrite) {
      write(as.yaml(curr_yaml), "_pkgdown.yml")
    } else {
      write(as.yaml(curr_yaml), "_pkgdown_new.yml")
    }
  }
}

update_yaml("SEGDR", overwrite = F)






library(purrr)
library(pkgdown)
data_reference_index_missing <- function(pkg = ".", depth = 1L) {
  pkg <- pkgdown:::as_pkgdown(pkg)

  meta <- pkg$meta[["reference"]] %||% default_reference_index(pkg)
  if (length(meta) == 0) {
    return(list())
  }

  # Cross-reference complete list of topics vs. topics found in index page
  all_topics <- meta %>%
    map(~ pkgdown:::select_topics(.$contents, pkg$topics)) %>%
    reduce(union)
  in_index <- seq_along(pkg$topics$name) %in% all_topics

  missing <- !in_index & !pkg$topics$internal
  pkg$topics$name[missing]
}

