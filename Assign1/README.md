Assign1

1) First function checks if local repo is up to date with remote repo. I found the git command to do this on this website: http://mikefrobbins.com/2016/02/18/git-status-doesnt-know-if-your-local-repository-is-out-of-date/. Added feature: If local repo is not up to date, user is given the option to update or not.

2) Second function puts all uncommitted changes into a file changes.log. I excluded any changes that occur in the changes.log file from being written into changes.log. I found out how to to do this here: https://stackoverflow.com/questions/10415100/want-to-exclude-file-from-git-diff.

3) Third function puts each line with the tag TODO, from every file in the current folder(Assign1) into a file todo.log. The todo.log file is placed in the Assign1 folder.

4) Fourth function checks all haskell files for syntax errors and puts the results into error.log. The file is placed in Assign1 folder
