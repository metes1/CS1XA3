#!/bin/bash

# Checks if local repo is up to date with the remote repo
if git remote show origin | grep "out of date" --silent
then
   echo "Local repo out of date"
   read -p  "Would you like to update your local repo with the remote repo? (Y/n) " ans
   if [ "$ans" = "Y" ]
   then
      git pull
      echo "Local repo updated"
   else
      echo "Local repo not updated"
   fi
else
   echo "Local repo up to date"
fi

# Puts all uncommitted changes in a file
git diff -- . ":(exclude)Assign1/changes.log" > Assign1/changes.log

# Puts each line from every file in project with the tag TODO into a file
grep -r --exclude="todo.log" --exclude="changes.log"  "#TODO" Assign1 > Assign1/todo.log
 
# Checks all haskell files for syntax errors and puts the results into a file
rm Assign1/error.log
find . -name "*.hs" |
    while read file
    do
	ghc -fno-code "$file" &>> Assign1/error.log
    done
