all: ../output/table4.tex ../output/table5.tex

include ../../generic.make

../input/enighdata.dta: | ../input
	ln -sf ../../enighdata/output/enighdata.dta ../input/enighdata.dta

../output/table4.tex: ZLFNincomedid.do ../input/enighdata.dta | ../output
	stata-se -e ZLFNincomedid.do

../output/table5.tex: NEGOCincomedid.do ../input/enighdata.dta | ../output
	stata-se -e NEGOCincomedid.do