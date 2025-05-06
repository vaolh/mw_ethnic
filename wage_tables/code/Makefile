all: ../output/table2.tex ../output/table3.tex

include ../../generic.make

../input/enighdata.dta:
	ln -sf ../../enighdata/output/enighdata.dta ../input/enighdata.dta

../output/table2.tex: indspeakdid.do ../input/enighdata.dta | ../output
	stata-se -e indspeakdid.do

../output/table3.tex: indigdid.do ../input/enighdata.dta | ../output
	stata-se -e indigdid.do
