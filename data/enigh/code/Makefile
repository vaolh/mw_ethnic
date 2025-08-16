all: ../output/enighdata.dta

include ../../../generic.make

YEARS := 2016 2018 2020 2022 2024
INPUT_FILES := $(foreach year, $(YEARS), ../input/ingresos$(year).dta ../input/poblacion$(year).dta ../input/concentradohogar$(year).dta ../input/trabajos$(year).dta)

../output/enighdata.dta: enighdata.do $(INPUT_FILES) | ../output
	stata-mp -e enighdata.do

../input/%.dta: ../../download/output/%.dta | ../input
	@if [ ! -e ../input ]; then mkdir -p ../input; fi
	@if [ ! -e $@ ]; then ln -s $< $@; fi

../output:
	@mkdir -p ../output
