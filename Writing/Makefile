# ----------------------
# Modifiable variables
# ----------------------
# Location of Pandoc support files.
PREFIX = /Users/andrew/.pandoc

TEX_DIR = compiled
DUKE_DIR = duke
FILENAME = compiled
APPENDIX_FILENAME = appendix

FRONTMATTER = metadata.md
FILES = 0_introduction/introduction.md 
FILES += 1_cs_regime_stability/cs_regime_stability.md
FILES += 2_country_case_studies/country_case_studies.md
FILES += 3_ingo_mechanisms/ingo_mechanisms.md
FILES += 4_gatekeeping/gatekeeping.md
FILES += 5_program_capture/program_capture.md
FILES += 6_conclusion/conclusion.md

APPENDIX = 7_methods_appendix/methods_appendix.md
APPENDIX += 8_results_appendix/results_appendix.md


FILES_TMP = $(FILES:.md=.md.tmp)
APPENDIX_TMP = $(APPENDIX:.md=.md.tmp)

# Word and HTML can choke on PDF images, so those targets use a helper script
# named replace_pdfs to replace all references to PDFs with PNGs and convert
# existing PDFs to PNG using sips. However, there are times when it's better to
# *not* convert to PNG on the fly, like when using high resolution PNGs exprted
# from R with ggsave+Cairo. To disable on-the-fly conversion and supply your
# own PNGs, uncomment PNG_CONVERT below. The script will still replace
# references to PDFs with PNGs, but will not convert the PDFs
PNG_CONVERT = --no-convert

# Location of your working bibliography file
BIB_FILE = /Users/andrew/Dropbox/Readings/Papers.bib

# CSL stylesheet (located in the csl folder of the PREFIX directory).
# Common CSLs:
#	* american-political-science-association
#   * chicago-fullnote-bibliography
#	* chicago-fullnote-no-bib
#   * chicago-syllabus-no-bib
#   * apa
#   * apsa-no-bib
CSL = american-political-science-association

# LaTeX doesn't use pandoc-citeproc + CSL and instead lets biblatex handle the
# heavy lifting. There are three possible styles built in to the template:
#   * bibstyle-chicago-notes
#   * bibstyle-chicago-authordate
#   * bibstyle-apa
TEX_REF = bibstyle-chicago-authordate

# Cross reference options
CROSSREF = --filter pandoc-crossref -M figPrefix:"Figure" -M eqnPrefix:"Equation" -M tblPrefix:"Table"

# To add version control footer support in PDFs:
#   1. Run vcinit in the directory
#   2. Place `./vc` at the front of the formula
#   3. Add `-V vc` to the pandoc command
#   4. Change pagestyle to athgit instead of ath


#--------------------
# Color definitions
#--------------------
NO_COLOR    = \x1b[0m
BOLD_COLOR	= \x1b[37;01m
OK_COLOR    = \x1b[32;01m
WARN_COLOR  = \x1b[33;01m
ERROR_COLOR = \x1b[31;01m


# --------------------
# Target definitions
# --------------------
.PHONY: duke

all: tex

tex: 
	@echo "$(WARN_COLOR)Converting Markdown to TeX using hikma-book template...$(NO_COLOR)"
	@echo "$(WARN_COLOR)...preprocessing files...$(NO_COLOR)"
	cd $(TEX_DIR); ./vc
	$(foreach file, $(FILES), replace_includes $(file) | chapterize > $(file).tmp;)
	$(foreach file, $(FILES_TMP), pandoc --atx-headers -t markdown -o $(file) $(file) --id-prefix `uuidgen`:;)
	@echo "$(WARN_COLOR)...converting to TeX...$(NO_COLOR)"
	pandoc $(FRONTMATTER) $(FILES_TMP) \
		-r markdown+simple_tables+table_captions+yaml_metadata_block -w latex -s -S \
		$(CROSSREF) \
		--default-image-extension=pdf \
		--latex-engine=xelatex \
		--template=$(PREFIX)/templates/xelatex-book.template \
		--biblatex \
		-V $(TEX_REF) \
		--bibliography=$(BIB_FILE) \
		--base-header-level=1 \
		--top-level-division=chapter \
		-V pagestyle=athgit \
		-V vc \
		--id-prefix `uuidgen`: \
		--toc \
	-o $(TEX_DIR)/$(FILENAME).tex
	@echo "$(WARN_COLOR)...converting TeX to PDF with latexmk (prepare for lots of output)...$(NO_COLOR)"
	cd $(TEX_DIR); latexmk -xelatex -quiet $(FILENAME)
	@echo "$(WARN_COLOR)...removing preprocessed files...$(NO_COLOR)"
	$(foreach file, $(FILES_TMP), rm $(file);)
	@echo "$(OK_COLOR)All done!$(NO_COLOR)"

duke:
	@echo "$(WARN_COLOR)Converting Markdown to TeX using dukedissertation template...$(NO_COLOR)"
	@echo "$(WARN_COLOR)...preprocessing files...$(NO_COLOR)"
	$(foreach file, $(FILES), replace_includes $(file) | chapterize > $(file).tmp;)
	$(foreach file, $(APPENDIX), replace_includes $(file) | chapterize > $(file).tmp;)
	$(foreach file, $(FILES_TMP), pandoc --atx-headers -t markdown -o $(file) $(file) --id-prefix `uuidgen`:;)
	$(foreach file, $(APPENDIX_TMP), pandoc --atx-headers -t markdown -o $(file) $(file) --id-prefix `uuidgen`:;)
	@echo "$(WARN_COLOR)...converting to TeX...$(NO_COLOR)"
	pandoc $(FRONTMATTER) $(FILES_TMP) \
		-r markdown+simple_tables+table_captions+yaml_metadata_block -w latex -S \
		$(CROSSREF) \
		--default-image-extension=pdf \
		--biblatex \
		--base-header-level=1 \
		--top-level-division=chapter \
		--id-prefix `uuidgen`: \
	-o $(DUKE_DIR)/$(FILENAME).tex
	pandoc $(APPENDIX_TMP) \
		-r markdown+simple_tables+table_captions+yaml_metadata_block -w latex -S \
		$(CROSSREF) \
		--default-image-extension=pdf \
		--biblatex \
		--base-header-level=1 \
		--top-level-division=chapter \
		--id-prefix `uuidgen`: \
	-o $(DUKE_DIR)/$(APPENDIX_FILENAME).tex
	@echo "$(WARN_COLOR)Removing weird space above lists in tables...$(NO_COLOR)"
	perl -i -0777 -pe 's/\\strut\n\\begin{itemize}/\n\\begin{itemize}/gm;' $(DUKE_DIR)/$(FILENAME).tex
	perl -i -0777 -pe 's/\\strut\n\\begin{itemize}/\n\\begin{itemize}/gm;' $(DUKE_DIR)/$(APPENDIX_FILENAME).tex
	@echo "$(WARN_COLOR)...converting TeX to PDF with latexmk (prepare for lots of output)...$(NO_COLOR)"
	cd $(DUKE_DIR); latexmk -xelatex -quiet dissertation.tex
	@echo "$(WARN_COLOR)...removing preprocessed files...$(NO_COLOR)"
	$(foreach file, $(FILES_TMP), rm $(file);)
	$(foreach file, $(APPENDIX_TMP), rm $(file);)
	@echo "$(OK_COLOR)All done!$(NO_COLOR)"
