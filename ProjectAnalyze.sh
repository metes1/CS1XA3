#!/bin/bash

# Checks if local repo is up to date with the remote repo
if git remote show origin | grep "out of date" --silent
then
   echo "Local repo out of date"
   echo "Would you like to update your local repo with the remote repo? (Y/N)"
   read ans
   if $ans = Y
   then
      git pull
   fi
else
   echo "Local repo up to date"
fi

# Puts all uncommitted changes in a file
git diff > changes.log

# Checks all haskell files for syntax errors and puts the results into a file
