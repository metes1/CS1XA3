Assign1

1) First function checks if local repo is up to date with remote repo. I found the git command to do this on this website: http://mikefrobbins.com/2016/02/18/git-status-doesnt-know-if-your-local-repository-is-out-of-date/. If local repo is not up to date, user is given the option to update or not.

2) Second function puts all uncommitted changes into a file changes.log (file is only in Assign1 folder, not root). Note: ProjectAnalyze.sh must be executed using the version that is in the repo root, not the one that is in the Assign1 folder. The path for changes.log is "Assign1/changes.log", therefore if the project is executed from the Assign1 folder the function won't work because that filepath doesn't exist within the Assign1 folder.

3) Third function puts each line with the tag TODO, from every file in the Assign1 folder into a file todo.log. The "Note:" of the above function applies to this function as well.
