# Magic diffs

`latexdiff` has the cool ability to produce beautiful diffed output from two LaTeX files. It does not work well with files that use includes, though—it works best with flattened, standalone TeX files.

Here's how to generate a diffed PDF based on a previous version of the dissertation (like the one I submitted to my committee for the defense, or https://github.com/andrewheiss/Dissertation/commit/2a0650901f4a88319fab8e1dca2e3ede4999cc8b):

- Go back in time in the repo: `git checkout 2a0650901f4a88319fab8e1dca2e3ede4999cc8b`
- Generate the TeX file and PDF: `cd Writing && make duke`
- Flatten the TeX file: `latexpand --makeatletter dissertation.tex > dissertation_orig.tex`
- Move `dissertation_orig.tex` somewhere temporary
- Go back to HEAD of git repo: `git checkout master`
- Generate the TeX file and PDF of the current version: `make duke`
- Flatten the current TeX file: `latexpand --makeatletter dissertation.tex > dissertation_flat.tex`
- Move the original `dissertation_orig.tex` back to the main writing directory
- Compare the two versions: `latexdiff dissertation_orig.tex dissertation_flat.tex > dissertation_diff.tex`
- Build the new diffed version: `latexmk -xelatex -quiet dissertation_diff.tex`
- Voila!
