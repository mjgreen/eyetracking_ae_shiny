# Alex's Shiny App for Eye-tracking data analysis

## How to sync to the server (method using git)

* First, login to the server: `ssh -p 22 mgreen@paraguay.bournemouth.ac.uk`
* `cd ShinyApps`
* if for the first time: `git clone --depth=1 --branch=main git@github.com:mjgreen/eyetracking_ae_shiny.git`
* `cd eyetracking_ae_shiny`
* `git pull`
* `R`
* `renv::restore()`
   * or, if that isn't sufficient then
   * `renv::rebuild()`
* page is avialable at: https://paraguay.bournemouth.ac.uk/shiny/users/mgreen/eyetracking_ae_shiny/

### Methods for syncing to the server that I abandoned:
* scp works but scp can't ignore directories (like renv/library/; .git/) - `scp -r /home/matt/gits/eyetracking_uruguay mgreen@uruguay.bournemouth.ac.uk:/home/mgreen/ShinyApps/`
* rsync would be able to ignore directories but needs rsync on the server (and the server doesn't have rsync)
* `rsync -arv --exclude=renv/library --exclude=.git /home/matt/gits/eyetracking_ae_shiny/ mgreen@uruguay.bournemouth.ac.uk:/home/mgreen/ShinyApps/eyetracking_ae_shiny/`

## How to run as a user, if the user has RStudio and all required packages already

This method would suit an external collaborator who can't vpn into BU, until IT make the uruguay service outward-facing
```
shiny::runGitHub("eyetracking_ae_shiny", "mjgreen")
```

## Gow to sync to the shared space on the server (method above uses my user-space)
* uploads to server's own space (no sudo so for packages that need to be built from source that have dependencies that aren't available on the server, we need to use renv to supply those packages within the app using renv)
* `scp -r /home/matt/gits/eyetracking_uruguay mgreen@uruguay.bournemouth.ac.uk:/srv/shiny-server/sample-apps/`
* `cd /srv/shiny-server/sample-apps/eyetracking_ae_shiny`
* `R`
* `renv::restore()` or `renv::rebuild()`
* page is available at: https://uruguay.bournemouth.ac.uk/shiny/sample-apps/eyetracking_uruguay/

## How to sign in to RStudio on the server:
https://uruguay.bournemouth.ac.uk/server/auth-sign-in?appUri=%2F
