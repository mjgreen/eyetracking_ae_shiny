# eyetracking_ae_shiny
Alex's Shiny App

# How to run
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