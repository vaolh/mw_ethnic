all: ../output/graph1.png ../output/graph2a.png ../output/graph2b.png ../output/graph2c.png ../output/graph2d.png \
     ../output/graph3.png ../output/graph4.png

include ../../generic.make

../input/enighdata.dta:
	ln -sf ../../enighdata/output/enighdata.dta ../input/enighdata.dta

../output/graph1.png ../output/graph2a.png ../output/graph2b.png ../output/graph2c.png ../output/graph2d.png: kernelplots.do ../input/enighdata.dta | ../output
	stata-se -e kernelplots.do

../output/graph3.png: indspeakdidplot.do ../input/enighdata.dta | ../output
	stata-se -e indspeakdidplot.do

../output/graph4.png: indigdidplot.do ../input/enighdata.dta | ../output
	stata-se -e indigdidplot.do