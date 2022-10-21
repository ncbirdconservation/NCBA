# Git functions

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
git pull origin their-feature
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
