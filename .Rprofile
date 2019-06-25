local({
  r         <- getOption("repos")
  r["CRAN"] <- "https://cran.rstudio.com/"
  auth      <- paste('person("Zhian N.", "Kamvar",', 
                     'email = "zkamvar@gmail.com",', 
                     'role = c("aut", "cre"),',
                     'comment = c(ORCID = "0000-0003-1458-7108"))'
                     )
  me       <- eval(parse(text = paste0('utils::', auth)))
  my_name  <- format(me, c('given', 'family'))
  my_email <- format(me, c('email'), braces = list(email = ''))
  desc     <- list(`Authors@R` = auth,
                   License     = "MIT + file LICENSE",
                   Version     = "0.0.0.9000"
                   )
  # Setting options ------------------------------------------------------------
  options(repos                   = r)
  options(editor                  = "vim")

  options(usethis.full_name       = my_name)
  options(usethis.description     = desc)
  options(usethis.protocol        = "ssh")

  options(blogdown.author         = my_name)
  options(blogdown.subdir         = "blog")

  # https://github.com/randy3k/radian
  options(radian.editing_mode     = "vi")
  options(radian.auto_indentation = TRUE)
  options(radian.insert_new_line  = FALSE)
  options(radian.color_scheme     = "bw")
  options(radian.tab_size         = 2)
  options(radian.prompt           = "\033[0;34m>\033[0m ")
  options(radian.shell_prompt     = "\033[0;31m$\033[0m ")

  # Checking CRAN package status -----------------------------------------------
  if (interactive()) {
    # based off of BrodieG's tinyverse version, but the notification has gotten
    # a bit noisy and cchecks seems to be fairly stable
    # 
    # I've also made some modifications to cache based on email, not just
    # assuming that it's one person.
    #
    # For posterity, here's the original function:
    # https://gist.github.com/brodieG/e60c94d4036f45018530ea504258bcf3
    .check_cran <- function(email      = my_email,
                            cache      = '~/.%s-R-cran-status.RDS',
                            cache.life = 24 * 3600
                           ) {

      cache_pat   <- cache
      cache       <- sprintf(cache, email)
      renew.cache <- TRUE

      display_check <- function(x, extra=NULL) {
        pkgs    <- format(c('package', x$data$table$package))
        package <- pkgs[1]
        pkgs    <- pkgs[-1]
        oops    <- x$data$table$any

        err <- x$data$table$error
        wrn <- x$data$table$warn
        nte <- x$data$table$note

        fe <- function(i) format(i, width = nchar("errors"))
        fw <- function(i) format(i, width = nchar("warnings"))
        fn <- function(i) format(i, width = nchar("notes"))

        cat(***
            ***
            ***
            ***
            "\n",
            sep = ***
           )
        # This needs to be a for loop because we can't print the decorated
        # output as a data frame or it will show all the escape values.
        for (i in seq_along(pkgs)) {
          e <- err[i]
          w <- wrn[i]
          n <- nte[i]
          p <- pkgs[i]

          # tallying errors
          ee <- e > 0
          we <- w > 0
          ne <- n > 0

          # formatting
          n <- fn(n)
          w <- fw(w)
          e <- fe(e)
          if (oops[i]) {
            e <- if (ee > 0) ***
            w <- if (we > 0) ***
            n <- if (ne > 0) ***
            p <- if (ee) ***
          } else {
            p <- ***
            e <- ***
            w <- ***
            n <- ***
          }
          writeLines(paste(p, n, w, e))
        }
        err.cols <- err > 0 | wrn > 0
        if (sum(as.numeric(err.cols), na.rm=TRUE))
          writeLines(c(***
        writeLines(c(***
      }


      if (file.exists(cache)) {
        cache.dat <- readRDS(cache)
        cache.age <- Sys.time() - cache.dat[[1]]
        if (as.double(cache.age, 'secs') < cache.life) {
          renew.cache <- FALSE
          aged_cache  <- format(round(cache.age))
          res         <- cache.dat[[2]]
          display_check(res,
                        sprintf("\ncached CRAN status (%s old).", aged_cache) 
                       ) 
        } 
      }

      if (renew.cache) {
        cat("connecting to CRAN...\n")
        res <- cchecks::cch_maintainers(sub("@", "_at_", email))
        saveRDS(list(Sys.time(), res), cache)
        display_check(res)
      }
      return(invisible(res))
    }


    # Display the header at startup --------------------------------------------
    cat("Default R library:", ***
    .check_cran(my_email)
    assign('.check_cran', .check_cran, env = .GlobalEnv)
    unloadNamespace(asNamespace('crayon'))
    unloadNamespace(asNamespace('cchecks'))
  }
})
