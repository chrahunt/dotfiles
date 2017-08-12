# Backup helpers
alias lss="du -sh * .* | sort -h"

# Store upstream repositories somewhere else
bgit_repo_repo=~/git-upstream
bgit() {
    bgit__dir="$1"
    mv "$bgit__dir" "$bgit_repo_repo/$bgit__dir" && \
       ln -s "$bgit_repo_repo/$bgit__dir" "$bgit__dir"
}
