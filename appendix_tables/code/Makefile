all: ../output/table1.tex

include ../../generic.make

../input/enighdata.dta:
	ln -sf ../../enighdata/output/enighdata.dta ../input/enighdata.dta

../output/table1.tex: descriptivetables.do ../input/enighdata.dta | ../output
	stata-se -e descriptivetables.do