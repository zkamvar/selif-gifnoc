# Set up my default options
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
                   License = "MIT + file LICENSE",
                   Version = "0.0.0.9000"
                   )
  options(repos               = r)
  options(usethis.full_name   = my_name)
  options(usethis.description = desc)
  options(usethis.protocol    = "ssh")
  options(blogdown.author     = my_name)
  options(blogdown.subdir     = "blog")
  options(editor              = "vim")
})


# based off of BrodieG's tinyverse version, but the notification has gotten
# a bit noisy and cchecks seems to be fairly stable
# 
# I've also made some modifications to cache based on email, not just assuming
# that it's one person
.check_cran <- function(email      = 'zkamvar@gmail.com',
                        cache      = '~/.%s-R-cran-status.RDS',
                        cache.life = 24 * 3600
                       ) {

  cache_pat <- cache
  cache <- sprintf(cache, email)
  renew.cache <- TRUE

  display_check <- function(x, extra=NULL) {
    pkgs <- format(c('package', x$data$table$package))
    package <- pkgs[1]
    pkgs <- pkgs[-1]
    oops <- x$data$table$any
    err  <- x$data$table$error
    wrn  <- x$data$table$warn
    nte  <- x$data$table$note

    cat(***
        ***
        ***
        ***
        "\n",
        sep = ***
       )
    for (i in seq_along(pkgs)) {
      e <- err[i]
      w <- wrn[i]
      n <- nte[i]
      p <- pkgs[i]
      if (oops[i]) {
        ew <- e > 0 | w > 0
        e  <- if (e > 0) ***
        w  <- if (w > 0) ***
        n  <- if (n > 0) ***
        p  <- if (ew)    ***
      } else {
        p <- ***
        e <- ***
        w <- ***
        n <- ***
      }
      cat(paste(p, n, w, e, "\n"))
    }
    err.cols <- x$data$table$warn > 0 | x$data$table$error > 0
    if(sum(as.numeric(err.cols), na.rm=TRUE))
      writeLines(c(***
    writeLines(c(***
  }


  if (file.exists(cache)) {
    cache.dat <- readRDS(cache)
    cache.age <- Sys.time() - cache.dat[[1]]
    if (as.double(cache.age, 'secs') < cache.life) {
      renew.cache <- FALSE
      aged_cache  <- format(round(cache.age))
      display_check(cache.dat[[2]],
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
}

if (interactive()) {
  .check_cran('zkamvar@gmail.com')

  cat("Default R library set to", ***
  
}


