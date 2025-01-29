# Target to build all files
all: $(addprefix ../output/, enigh2022_concentradohogar.dta \
enigh2020_concentradohogar.dta \
enigh2018_concentradohogar.dta \
enigh2016_concentradohogar.dta \
enigh2022_ingresos.dta)
	rm -rf ../temp

include ../../generic.make

# Ensure the temp and output directories exist
../temp:
	mkdir -p ../temp

../output:
	mkdir -p ../output

# 2022 ENIGH - Concentrado Hogar
../temp/enigh2022_dta.zip: | ../temp
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/2022/microdatos/enigh2022_ns_concentradohogar_dta.zip -O ../temp/enigh2022_dta.zip || { echo "Failed to download 2022 data"; exit 1; }

../output/enigh2022_concentradohogar.dta: ../temp/enigh2022_dta.zip | ../output
	unzip -o -j ../temp/enigh2022_dta.zip -d ../output
	mv ../output/concentradohogar.dta ../output/concentradohogar2022.dta
	# Delete all non-.dta files in ../output
	find ../output -type f ! -name '*.dta' -delete

# 2022 ENIGH - Ingresos
../temp/enigh2022_i_dta.zip: | ../temp
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/2022/microdatos/enigh2022_ns_ingresos_dta.zip -O ../temp/enigh2022_i_dta.zip || { echo "Failed to download 2022 data"; exit 1; }

../output/enigh2022_concentradohogar.dta: ../temp/enigh2022_i_dta.zip | ../output
	unzip -o -j ../temp/enigh2022_i_dta.zip -d ../output
	mv ../output/ingresos.dta ../output/ingresos2022.dta
	# Delete all non-.dta files in ../output
	find ../output -type f ! -name '*.dta' -delete

# 2020 ENIGH
../temp/enigh2020_dta.zip: | ../temp
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_concentradohogar_dta.zip -O ../temp/enigh2020_dta.zip || { echo "Failed to download 2020 data"; exit 1; }

../output/enigh2020_concentradohogar.dta: ../temp/enigh2020_dta.zip | ../output
	unzip -o -j ../temp/enigh2020_dta.zip -d ../output
	mv ../output/concentradohogar.dta ../output/concentradohogar2020.dta
	# Delete all non-.dta files in ../output
	find ../output -type f ! -name '*.dta' -delete

# 2020 ENIGH - Ingresos
../temp/enigh2020_i_dta.zip: | ../temp
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_ingresos_dta.zip -O ../temp/enigh2020_i_dta.zip || { echo "Failed to download 2020 data"; exit 1; }

../output/enigh2020_concentradohogar.dta: ../temp/enigh2020_i_dta.zip | ../output
	unzip -o -j ../temp/enigh2020_i_dta.zip -d ../output
	mv ../output/ingresos.dta ../output/ingresos2020.dta
	# Delete all non-.dta files in ../output
	find ../output -type f ! -name '*.dta' -delete

# 2018 ENIGH
../temp/enigh2018_dta.zip: | ../temp
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_concentradohogar_dta.zip -O ../temp/enigh2018_dta.zip || { echo "Failed to download 2018 data"; exit 1; }

../output/enigh2018_concentradohogar.dta: ../temp/enigh2018_dta.zip | ../output
	unzip -o -j ../temp/enigh2018_dta.zip -d ../output
	mv ../output/concentradohogar.dta ../output/concentradohogar2018.dta
	# Delete all non-.dta files in ../output
	find ../output -type f ! -name '*.dta' -delete

# 2018 ENIGH - Ingresos
../temp/enigh2018_i_dta.zip: | ../temp
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_ingresos_dta.zip -O ../temp/enigh2018_i_dta.zip || { echo "Failed to download 2018 data"; exit 1; }

../output/enigh2018_concentradohogar.dta: ../temp/enigh2018_i_dta.zip | ../output
	unzip -o -j ../temp/enigh2018_i_dta.zip -d ../output
	mv ../output/ingresos.dta ../output/ingresos2018.dta
	# Delete all non-.dta files in ../output
	find ../output -type f ! -name '*.dta' -delete

# 2016 ENIGH
../temp/enigh2016_dta.zip: | ../temp
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/2016/microdatos/enigh2016_ns_concentradohogar_dta.zip -O ../temp/enigh2016_dta.zip || { echo "Failed to download 2016 data"; exit 1; }

../output/enigh2016_concentradohogar.dta: ../temp/enigh2016_dta.zip | ../output
	unzip -o -j ../temp/enigh2016_dta.zip -d ../output
	mv ../output/concentradohogar.dta ../output/concentradohogar2016.dta
	# Delete all non-.dta files in ../output
	find ../output -type f ! -name '*.dta' -delete

# 2016 ENIGH - Ingresos
../temp/enigh2016_i_dta.zip: | ../temp
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/2016/microdatos/enigh2016_ns_ingresos_dta.zip -O ../temp/enigh2016_i_dta.zip || { echo "Failed to download 2016 data"; exit 1; }

../output/enigh2016_concentradohogar.dta: ../temp/enigh2016_i_dta.zip | ../output
	unzip -o -j ../temp/enigh2016_i_dta.zip -d ../output
	mv ../output/ingresos.dta ../output/ingresos2016.dta
	# Delete all non-.dta files in ../output
	find ../output -type f ! -name '*.dta' -delete