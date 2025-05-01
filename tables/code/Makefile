all: ../output/table1.tex
	mkdir -p ../input
	ln -sf ../../enighdata/output/enighdata.dta ../input/enighdata.dta

include ../../generic.make

../output/table1.tex: descriptivetables.do ../input/enighdata.dta
	stata-se -e descriptivestatistics.do
