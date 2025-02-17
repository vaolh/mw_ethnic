all: ../../output/enighdata_output.dta

include ../../generic.make

../../output/enighdata_output.dta: enighdata.do ../input/ingresos2016.dta | ../../output
	stata-se -e enighdata.do

../input/ingresos2016.dta: ../../downloaddata/output/ingresos2016.dta | ../input
	@if [ ! -e ../input ]; then mkdir -p ../input; fi
	@if [ ! -e $@ ]; then ln -s $< $@; fi

../../output:
	mkdir -p ../../output
