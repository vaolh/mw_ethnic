all: ../output/graph5.png ../output/graph6.png

include ../../generic.make

../input/enighdata.dta: | ../input
	ln -sf ../../enigh/output/enighdata.dta ../input/enighdata.dta

../output/graph5.png ../output/graph6.png: ZLFNincomedidplot.do ../input/enighdata.dta | ../output
	stata-mp -e ZLFNincomedidplot.do