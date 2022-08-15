# README 

This repo was created for a series of teaching sessions in 2022.

## The Demo Application 

Downloading the repo as is should allow for the generation of the demo app. 

After downloading the repo, run the demo application as follows:

1. Open RStudio (If you need installation instructions, contact Amy (or ITS)) 
2. (optional) Create an R project inside the main folder with the same name as the main folder (this should be ALessonInShiny). Do this by going File > New Project, and follow the prompts. This will generate an `.RProj` file in the main directory, and is mostly just a tidiness measure for any new objects you create and want saved. 
3. If you created a project in the directory, ignore this step. Else, set your working directory to the folder containing the app code. Do this by going to Session > Set Working Directory > Choose Directory, and navigate to the main directory. 
4. Open the `app.R` file 
5. Check the script for any lines containing the `library()` function 
6. Try run those lines (ctrl + Enter or Cmd + Enter) and if they fail, install the required package. i.e. if `library(shiny)` returns a `package not found` error, then run `install.packages("shiny")` in the console. 
7. Go back to the app.R script and click `Run App` in the top right corner. 
8. Voila 

## Other Files 

There are other files contained within the repo that formed part of the second teaching session in the R Shiny series. The `.Rmd` file contains the slides used (instructions on compiling this can be found below), and then there are two folders containing the more basic demo applications. 

The basic demo applications can be run in the same way as the main demo application, just ignore the optional step. 

Any other files are just images that are used within the app or slides. Note that the `www` directory contains these, as required by the shiny framework. Any new images you'd like to add in should be stored here.  

### Generating the Slides 

The slides were generating using a package called `rmarkdown`. Try load it using `library(rmarkdown)` and if this fails, install it using `install.packages("rmarkdown")` 

Prior to knitting the document, so a search for any other `library()` commands and make sure these packages are installed as well, in the same way as above. 

Once all the appropraite packages have been installed, and the source script is open, you should see a "Knit" button in the tool bar above the source script. Click this and wait for the doc to generate. 

If there are any errors, feel free to contact Amy. 

Enjoy! 

