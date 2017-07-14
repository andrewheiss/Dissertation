#!/bin/bash
#
# This script traverses all commits of a git repository in the current directory
# and counts the number of words that are changed, i.e. added or deleted for all
# TeX files. It outputs a CSV with an ISO 8601 timestamp, number of words addded,
# deleted, and running total
#
# Script originally by Bastian Rieck 
# http://bastian.rieck.ru/blog/posts/2017/counting_words_git/

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
