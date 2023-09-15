# Git functions

# Git Flow
Development of the shiny app (in the shinyapp folder), and the "development"
folder requires us to keep better branch organization.

master -- develop -- species-stats
                  -- shiny-dev

"species-stats": Development of rmarkdown scripts and updating the ncba_functions. These edits should be merged with the develop branch. Workflow:

    git checkout shiny-dev
    git merge develop
    # do editing
    git add .
    git commit -m "making changes to species-stats"
    checkout develop
    merge species-stats


"shiny-dev": Development of the shiny app and incorporating species-stats changes. Workflow:
 
    git checkout shiny-dev
    git merge develop
    # do editing
    git add .
    git commit -m "making changes to shiny-dev"
    checkout develop
    merge shiny-dev

    



## Resources

- Helpful website: [Oh Sh!t, Git!] (https://ohshitgit.com)

## Clone the main repo
This makes a local copy to work from

```
git clone https://github.com/nmtarr/NCBA.git
```

## Create a feature branch, get on it

```
git checkout -b your-feature master
```

## Add your changes

```
git add .
```

## Commit changes

```
git commit -m "Include a short message here"
```

## Push branch to main repo

```
git push -u origin your-feature
```

## Pull down someone else's feature branch (for review)

```
git fetch origin their-feature
git checkout origin their-feature
```

## Pull updated master branch

```
git pull origin master
```

# Other Functions

## Update branch from master

```
get checkout your-feature
git merge master
```
