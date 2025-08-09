all: ../../../data/enighdata/output/enighdata.dta

include ../../../generic.make

YEARS := 2016 2018 2020 2022
INPUT_FILES := $(foreach year, $(YEARS), ../input/ingresos$(year).dta ../input/poblacion$(year).dta ../input/concentradohogar$(year).dta)

../../../data/enighdata/output/enighdata.dta: enighdata.do $(INPUT_FILES) | ../../../data/enighdata/output
	stata-se -e enighdata.do

../input/%.dta: ../../../data/downloaddata/output/%.dta | ../input
	@if [ ! -e ../input ]; then mkdir -p ../input; fi
	@if [ ! -e $@ ]; then ln -s $< $@; fi

../../../data/enighdata/output:
	@mkdir -p ../../../data/enighdata/output
