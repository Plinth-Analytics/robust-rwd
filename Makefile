
all: setup_env initial_etl

setup_env: data packages

data: setup/download.R
	Rscript setup/download.R

packages: renv.lock
	Rscript \
	-e 'if (!("renv" %in% rownames(installed.packages()))) install.packages("renv")' \
	-e 'renv::restore()'

.PHONY: clean

clean: 
	rm -rf data
