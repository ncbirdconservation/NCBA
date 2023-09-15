# Notes on Development Folder

Scott Anderson
Nate Tarr
Elsa Chen

# Git workflow

Development of the shiny app (in the shinyapp folder), and the "development" folder requires us to keep better branch organization. RMarkdown development will be in the "species-stats" branch, while shiny app development will be in the "shiny-dev" branch. When a change is made, the branch should be merged with the "develop" branch. Once both branches are up-to-date and functioning, the "develop" branch can be merged with the master.

master -- develop -- species-stats
                  -- shiny-dev

"species-stats": Development of Rmarkdown scripts and updating the ncba_functions. These edits should be merged with the develop branch. Workflow:

    git pull origin develop
    git checkout shiny-dev
    git merge develop
    # do editing
    git add .
    git commit -m "making changes to species-stats"
    git checkout develop
    git merge species-stats


"shiny-dev": Development of the shiny app and incorporating species-stats changes. Workflow:
 
    git pull origin develop
    git checkout shiny-dev
    git merge develop
    # do editing
    git add .
    git commit -m "making changes to shiny-dev"
    git checkout develop
    git merge shiny-dev
