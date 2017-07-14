#!/bin/bash
#
# This script traverses all commits of a git repository in the current directory
# and counts the number of words that are changed, i.e. added or deleted for all
# TeX files. The output contains a time-stamp (YYYY-MM-DD). Next, there are some
# counts, viz. the number of added, deleted, and total words.

branch="master"
words=0

echo 'date, added, deleted, words'

for commit in $(git rev-list --reverse $branch)
do
  date=$(git show -s --format=%cd --date=iso $commit)
  added=$(git show -p --word-diff=porcelain $commit "Writing/*.md" | grep -e '^+[^+]' | wc -w)
  deleted=$(git show -p --word-diff=porcelain $commit "Writing/*.md" | grep -e '^-[^-]' | wc -w)
  
  words=$(($words+$added))
  words=$(($words-$deleted))
  
  echo $date, $added, $deleted, $words
done
