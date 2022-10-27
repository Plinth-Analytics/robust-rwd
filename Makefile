
all: setup_env etl

setup_env: packages data 

data: setup/download.R
	Rscript setup/download.R

packages: renv.lock
	Rscript \
	-e 'if (!("renv" %in% rownames(installed.packages()))) install.packages("renv")' \
	-e 'renv::restore()'

.PHONY: clean

clean: 
	rm -rf data
