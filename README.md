# Alex's Shiny App for Eye-tracking data analysis

## How to sync to the server

* First, login to the server
  * >ssh -p 22 mgreen@uruguay.bournemouth.ac.uk
  

## How to run
```
shiny::runGitHub("eyetracking_ae_shiny", "mjgreen")
```

## Works to upload and run an app into my user space

scp -r /home/matt/gits/eyetracking_uruguay mgreen@uruguay.bournemouth.ac.uk:/home/mgreen/ShinyApps/

https://uruguay.bournemouth.ac.uk/shiny/users/mgreen/eyetracking_uruguay/

## uploads to server's own space (no sudo so for packages that need to be built from source that have dependencies that aren't available on the server, we need to use renv to supply those packages within the app)

scp -r /home/matt/gits/eyetracking_uruguay mgreen@uruguay.bournemouth.ac.uk:/srv/shiny-server/sample-apps/

https://uruguay.bournemouth.ac.uk/shiny/sample-apps/eyetracking_uruguay/

## To rebuild from the venv lockfile, use `renv::rebuild()` on the server after having logged in using
>ssh -p 22 mgreen@uruguay.bournemouth.ac.uk

## Sign in to RStudio on the server:
https://uruguay.bournemouth.ac.uk/server/auth-sign-in?appUri=%2F


## Sync -- Method using git
ssh to server
ssh -p 22 mgreen@uruguay.bournemouth.ac.uk
cd ShinyApps
git clone --depth=1 --branch=main git@github.com:mjgreen/eyetracking_ae_shiny.git
cd eyetracking_ae_shiny
R CMD renv::rebuild()
https://uruguay.bournemouth.ac.uk/shiny/users/mgreen/eyetracking_ae_shiny/



https://github.com/mjgreen/eyetracking_ae_shiny/blob/6c1e3b79a7235fbcbb29389126012bc2bbd1e931/inputs/fixation_report.csv
