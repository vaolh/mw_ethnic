all: $(addprefix ../output/, enigh2022_concentradohogar.dta enigh2020_concentradohogar.dta enigh2018_concentradohogar.dta enigh2016_concentradohogar.dta)

include ../../generic.make

include ../../generic.make


# 2022 ENIGH - Concentrado Hogar
../temp/enigh2022: | ../temp
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/2022/microdatos/enigh2022_ns_concentradohogar_dta.zip -O ../temp/enigh2022_dta.zip
../output/enigh2022_concentradohogar.dta: ../temp/enigh2022 | ../output
	unzip -j ../temp/enigh2022_dta.zip -d ../output
	mv ../output/concentradohogar.dta ../output/concentradohogar2022.dta

# 2020 ENIGH
../temp/enigh2020: | ../temp
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_concentradohogar_dta.zip -O ../temp/enigh2020_dta.zip
../output/enigh2020_concentradohogar.dta: ../temp/enigh2020 | ../output
	unzip -j ../temp/enigh2020_dta.zip -d ../output
	mv ../output/concentradohogar.dta ../output/concentradohogar2020.dta

# 2018 ENIGH
../temp/enigh2018: | ../temp
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_concentradohogar_dta.zip -O ../temp/enigh2018_dta.zip
../output/enigh2018_concentradohogar.dta: ../temp/enigh2018 | ../output
	unzip -j ../temp/enigh2018_dta.zip -d ../output
	mv ../output/concentradohogar.dta ../output/concentradohogar2018.dta

# 2016 ENIGH
../temp/enigh2016: | ../temp
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/2016/microdatos/enigh2016_ns_concentradohogar_dta.zip -O ../temp/enigh2016_dta.zip
../output/enigh2016_concentradohogar.dta: ../temp/enigh2016 | ../output
	unzip -j ../temp/enigh2016_dta.zip -d ../output
	mv ../output/concentradohogar.dta ../output/concentradohogar2016.dta
	rm -rf ../temp
