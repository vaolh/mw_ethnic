all: ../../enighdata/output/enighdata_output.dta

include ../../generic.make

YEARS := 2016 2018 2020 2022
INPUT_FILES := $(foreach year, $(YEARS), ../input/ingresos$(year).dta ../input/poblacion$(year).dta ../input/concentradohogar$(year).dta)

../../enighdata/output/enighdata_output.dta: enighdata.do $(INPUT_FILES) | ../../enighdata/output
	stata-se -e enighdata.do

../input/%.dta: ../../downloaddata/output/%.dta | ../input
	@if [ ! -e ../input ]; then mkdir -p ../input; fi
	@if [ ! -e $@ ]; then ln -s $< $@; fi

../../enighdata/output:
	@mkdir -p ../../enighdata/output
