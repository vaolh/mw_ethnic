TOKEN := d024134c-be50-03ee-bdf6-1cda05612673
INDICATOR_ID := 910392

API_URL := https://en.www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/$(INDICATOR_ID)/en/0700/false/BIE/2.0/$(TOKEN)?type=json

all: ../output/inpc_clean.csv
	rm -rf ../temp

../temp:
	mkdir -p ../temp

../input:
	mkdir -p ../input

../output:
	mkdir -p ../output

../input/inpc.json: | ../input
	curl -L -o ../input/inpc.json $(API_URL)

../output/inpc_clean.csv: ../input/inpc.json | ../output
	Rscript ../code/deflators.R

clean:
	rm -rf ../temp ../input/inpc.json ../output/inpc_clean.csv
