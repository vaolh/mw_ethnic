all: ../output/graph1.png ../output/graph2a.png ../output/graph2b.png ../output/graph2c.png ../output/graph2d.png

include ../../generic.make

../input/enighdata.dta:
	ln -sf ../../enighdata/output/enighdata.dta ../input/enighdata.dta

../output/graph1.png ../output/graph2a.png ../output/graph2b.png ../output/graph2c.png ../output/graph2d.png: kernelplots.do ../input/enighdata.dta | ../output
	stata-se -e kernelplots.do
