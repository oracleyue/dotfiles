### Set the default repository
cat(".Rprofile: Setting France repository\n")
repoaddr = getOption("repos")
repoaddr["CRAN"] = "https://cran.univ-paris1.fr"
options(repos = repoaddr)
rm(repoaddr)

### Change to the default working directory
cat(".Rprofile: Setting default directory\n")
defwd = "~/tmp"
setwd(defwd)
cat(paste("[Current directory: ", defwd, "]\n"))
rm(defwd)
