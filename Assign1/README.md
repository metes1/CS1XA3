## Assignment 1: Project Analyze

1) First feature checks if local repo is up to date with remote repo. I found the git command to do this on this website: http://mikefrobbins.com/2016/02/18/git-status-doesnt-know-if-your-local-repository-is-out-of-date/. Added feature: If local repo is not up to date, user is given the option to update or not.

2) Second feature puts all uncommitted changes into a file changes.log. I excluded any changes that occur in the changes.log file from being written into changes.log. I found out how to to do this here: https://stackoverflow.com/questions/10415100/want-to-exclude-file-from-git-diff.

3) Third feature puts each line with the tag TODO, from every file in into a file todo.log. The todo.log exludes any log files as well as ProjectAnalyze.sh.

4) Fourth feature checks all haskell files for syntax errors and puts the results into error.log. The function doesn't consider 'main' not being defined as an error. In the case that this error appears, "main = not defined" will temporarily be written in to the file in order to bypass the error. The file will then be changed back to its original state.

5) Extra added feature alerts user if there are any unstaged changes. Then gives the user the option to view those changes in the changes.log file, stage all, commit, and push to github.
