
setupenv: data packages

data: setup/download.R
	Rscript setup/download.R

packages: 
	Rscript \
	-e 'if (!("renv" %in% rownames(installed.packages())) install.packages("renv")' \
	-e 'renv::restore()'

