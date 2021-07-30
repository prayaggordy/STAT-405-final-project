# STAT-405-final-project
*Prayag Gordy, Ankit Patel, Hallie Trial*

To run our code, either source `main.R` or knit `paper.Rmd`. 

`main.R` first loads the requisite libraries, then sources the R scripts in the `R/` folder (see line 3, `sapply(list.files(path = "R", full.names = T), source, .GlobalEnv)`). Each of these scripts contains only functions, which are then loaded into the environment. These functions are later used in `main.R` to create objects, which are used in further functions. Finally, we create our SQL database.

In functions that load data, you'll see an `update` parameter whose default value is false. If set to true, the function will pull the data again from the original source; if false, the function will check for an already-processed copy of the data or at least an already-downloaded raw copy, and it will use that instead of downloading again. *The first time you run the script, all data will have to download; this will take a little while!*

`paper.Rmd` first sources `main.R` to load everything into the environment. Then it knits the children documents—one for each section of the paper—found in the `Rmd/` folder. These documents are in the same environment as `paper.Rmd` and therefore have access to all the variables and functions loaded in `main.R`.

Almost every single function in this repository makes use of our `config.yaml` file. In our config file, we store paths, filenames, URLs, and other hard-coded values in a YAML format. This strengthens our code by holding all hard-coded values in one location instead of spread throughout a large codebase.
