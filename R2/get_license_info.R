
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

To output the licenses to a csv file:
  write.csv(lisc[,c(1,10)], "RPackageLicenses.csv")

loadedNamespaces()
packageVersion()

.read_authors_at_R_field  <- function (x) 
{
  out <- if ((Encoding(x) == "UTF-8") && !l10n_info()$"UTF-8") {
    con <- file()
    on.exit(close(con))
    writeLines(x, con, useBytes = TRUE)
    eval(parse(con, encoding = "UTF-8"))
  }
  else {
    eval(str2expression(x))
  }
  if (!inherits(out, "person")) 
    out <- do.call("c", lapply(x, as.person))
  out
}

get_author <- function (package = "base", lib.loc = NULL, auto = NULL) 
{
  if (!is.null(auto) && !is.logical(auto) && !any(is.na(match(c("Package", 
                                                                "Version", "Title"), names(meta <- as.list(auto))))) && 
      !all(is.na(match(c("Authors@R", "Author"), 
                       names(meta))))) {
    auto_was_meta <- TRUE
    package <- meta$Package
  }
  else {
    auto_was_meta <- FALSE
    dir <- system.file(package = package, lib.loc = lib.loc)
    if (dir == "") 
      stop(packageNotFoundError(package, lib.loc, sys.call()))
    meta <- packageDescription(pkg = package, lib.loc = dirname(dir))
    citfile <- file.path(dir, "CITATION")
    test <- file_test("-f", citfile)
    if (!test) {
      citfile <- file.path(dir, "inst", "CITATION")
      test <- file_test("-f", citfile)
    }
    if (is.null(auto)) 
      auto <- !test
  }
  year <- sub("-.*", "", meta$`Date/Publication`)
  if (!length(year)) {
    if (is.null(meta$Date)) {
      warning(gettextf("no date field in DESCRIPTION file of package %s", 
                       sQuote(package)), domain = NA)
    }
    else {
      date <- trimws(as.vector(meta$Date))[1L]
      date <- strptime(date, "%Y-%m-%d", tz = "GMT")
      if (!is.na(date)) 
        year <- format(date, "%Y")
    }
  }
  if (!length(year)) {
    date <- as.POSIXlt(sub(";.*", "", trimws(meta$Packaged)[1L]))
    if (!is.na(date)) 
      year <- format(date, "%Y")
  }
  if (!length(year)) {
    warning(gettextf("could not determine year for %s from package DESCRIPTION file", 
                     sQuote(package)), domain = NA)
    year <- NA_character_
  }
  author <- meta$`Authors@R`
  if (length(author)) {
    aar <- .read_authors_at_R_field(author)
    author <- Filter(function(e) {
      !(is.null(e$given) && is.null(e$family)) && !is.na(match("aut", 
                                                               e$role))
    }, aar)
    if (!length(author)) 
      author <- Filter(function(e) {
        !(is.null(e$given) && is.null(e$family)) && !is.na(match("cre", 
                                                                 e$role))
      }, aar)
  }
  if (length(author)) {
    has_authors_at_R_field <- TRUE
  }
  else {
    has_authors_at_R_field <- FALSE
    author <- as.personList(meta$Author)
  }
  z <- list(title = paste0(package, ": ", meta$Title), 
            author = author, year = year, note = paste("R package version", 
                                                       meta$Version))
  if (identical(meta$Repository, "CRAN")) 
    z$url <- sprintf("https://CRAN.R-project.org/package=%s", 
                     package)
  if (identical(meta$Repository, "R-Forge")) {
    z$url <- if (!is.null(rfp <- meta$"Repository/R-Forge/Project")) 
      sprintf("https://R-Forge.R-project.org/projects/%s/", 
              rfp)
    else "https://R-Forge.R-project.org/"
    if (!is.null(rfr <- meta$"Repository/R-Forge/Revision")) 
      z$note <- paste(z$note, rfr, sep = "/r")
  }
  if (!length(z$url) && !is.null(url <- meta$URL)) {
    if (grepl("[, ]", url)) 
      z$note <- url
    else z$url <- url
  }
  header <- if (!auto_was_meta) {
    gettextf("To cite package %s in publications use:", 
             sQuote(package))
  }
  else NULL
  footer <- if (!has_authors_at_R_field && !auto_was_meta) {
    gettextf("ATTENTION: This citation information has been auto-generated from the package DESCRIPTION file and may need manual editing, see %s.", 
             sQuote("help(\"citation\")"))
  }
  else NULL
  author <- format(z$author, include = c("given", "family"))
  if (length(author) > 1L) 
    author <- paste(paste(head(author, -1L), collapse = ", "), 
                    tail(author, 1L), sep = " and ")
  
  
  return(author)

}

packages <- loadedNamespaces()
lisc <- installed.packages(fields = "License")
test <- installed.packages(fields = "author")

package_info <- lisc[lisc[, 1] %in% packages, ]

authors <- rep('', nrow(package_info))
for(i in 1:nrow(package_info)) {
  authors[i] <- get_author(package_info[i, 'Package'])
}

df <- package_info[, c('Package', 'License', 'License_restricts_use', 'Built')] %>% as.data.frame
df$author <- authors
write.table(df, col.names=  TRUE, row.names=  FALSE, sep = ';',
            file = 'package_license.csv')


meta <- packageDescription(pkg = packages[1])
