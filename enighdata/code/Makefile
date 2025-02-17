all: ../output/enighdata_output.dta

include ../../generic.make

../output/enighdata_output.dta: enighdata.do ../input/ingresos2016.dta | ../output
	stata-se -e $<

../input/ingresos2016.dta: ../../downloaddata/output/ingresos2016.dta | ../input
	ln -s $< $@

../output:
	mkdir -p ../output

../input:
	mkdir -p ../input
