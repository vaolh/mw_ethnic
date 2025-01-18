all: $(addprefix ../output/, enigh2022_concentradohogar.dta)

include ../../generic.make

# Download INEGI dataset (ZIP file)
../temp/enigh2022_concentradohogar_dta.zip: | ../temp
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/2022/microdatos/enigh2022_ns_concentradohogar_dta.zip -O $@

# Extract the dataset (DTA file)
../output/concentradohogar.dta: ../temp/enigh2022_concentradohogar_dta.zip | ../output
	unzip -j $< -d ../output
	rm -rf ../temp  
	touch $@