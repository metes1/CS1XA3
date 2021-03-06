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
git diff -- . ":(exclude)changes.log" > changes.log

# Puts each line from every file in project with the tag TODO into a file
grep -r --exclude="*.log" --exclude="ProjectAnalyze.sh"  "#TODO" . > todo.log
 
# Checks all haskell files for syntax errors and puts the results into a file
if [ -f "error.log" ] # Removes old log if it exits
then
   rm error.log
fi

find . -name "*.hs" |
    while read file
    do
      ghc -fno-code "$file" &> tmpErr.txt
      if grep -q "The IO action" "tmpErr.txt"
      then
         sed -i "1i main = undefined --tmp" "$file" #Bypasses main not defined error
	 ghc -fno-code "$file" &>> error.log
      else
         ghc -fno-code "$file" &>> error.log
      fi
    done

find . -name "*.hs" | #Changes file back to original state
    while read fileFix
    do
      sed -i "/main = undefined --tmp/d" $fileFix
    done
rm tmpErr.txt

# Checks if any changes have been made to local repo by looking in changes.log. If yes, it gives user the option to commit and push those changes to github
if [ -s "changes.log" ]
then
    echo "Unstaged changes within your local repo have been detected."
    read -p "Would you like to view these changes? (Y/n) " ans1
    if [ "$ans1" = "Y" ]
    then
        cat "changes.log"
    fi
    read -p "Would you like to stage and commit all of these changes? (Y/n) " ans2
    if [ "$ans2" = "Y" ]
    then
        echo "Please enter your commit message in quotes and then press enter to finalize commit: "
	read message
	git add -A
	git commit -m "$message"
	read -p "Would you now like to push to github? (Y/n) " ans3
        if [ "$ans3" = "Y" ]
	then
	git push
	fi
    fi
fi
