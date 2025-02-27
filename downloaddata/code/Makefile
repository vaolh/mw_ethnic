all: $(foreach year, 2016 2018 2020 2022, $(addprefix ../output/enigh$(year)_, concentradohogar.dta ingresos.dta poblacion.dta))
	rm -rf ../temp

include ../../generic.make

YEARS := 2016 2018 2020 2022
TYPES := concentradohogar ingresos poblacion

¡../temp:
	mkdir -p ../temp

../output:
	mkdir -p ../output

../temp/enigh%dta.zip: | ../temp
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$*/microdatos/enigh$*_ns_$*_dta.zip -O ../temp/enigh$*_dta.zip || { echo "Failed to download $* data"; exit 1; }

../output/enigh%_dta: ../temp/enigh%dta.zip | ../output
	unzip -o -j ../temp/enigh$*_dta.zip -d ../output
	mv ../output/$*.dta ../output/$*$*.dta
	find ../output -type f ! -name '*.dta' -delete
