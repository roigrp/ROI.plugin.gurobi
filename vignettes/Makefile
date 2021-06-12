

all: render rename clean

render:
	R -e 'rmarkdown::render("ROI.plugin.gurobi.Rmd", output_format = "pdf_document", clean = FALSE)'

rename:
	mv ROI.plugin.gurobi.utf8.md ROI.plugin.gurobi.md

clean:
	rm -f ROI.plugin.gurobi.knit.md


