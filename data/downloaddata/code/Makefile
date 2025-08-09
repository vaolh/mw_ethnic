YEARS := 2024 2022 2020 2018 2016
TYPES := concentradohogar ingresos poblacion trabajos
ZIP_PATTERNS := dta i_dta p_dta

all: $(addprefix ../../output/, \
     enigh2024_concentradohogar.dta enigh2024_ingresos.dta enigh2024_poblacion.dta enigh2024_trabajos.dta \
     enigh2022_concentradohogar.dta enigh2022_ingresos.dta enigh2022_poblacion.dta enigh2022_trabajos.dta \
     enigh2020_concentradohogar.dta enigh2020_ingresos.dta enigh2020_poblacion.dta enigh2020_trabajos.dta \
     enigh2018_concentradohogar.dta enigh2018_ingresos.dta enigh2018_poblacion.dta enigh2018_trabajos.dta \
     enigh2016_concentradohogar.dta enigh2016_ingresos.dta enigh2016_poblacion.dta enigh2016_trabajos.dta)
	rm -rf ../../temp

include ../../../generic.make

../../temp:
	mkdir -p ../../temp

../../output:
	mkdir -p ../../output


define concentradohogar_rules
../../temp/enigh$(1)_dta.zip: | ../../temp
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_concentradohogar_dta.zip -O ../../temp/enigh$(1)_dta.zip || { echo "Failed to download $(1) data"; exit 1; }

../../output/enigh$(1)_concentradohogar.dta: ../../temp/enigh$(1)_dta.zip | ../../output
	unzip -o -j ../../temp/enigh$(1)_dta.zip -d ../../output
	mv ../../output/concentradohogar.dta ../../output/concentradohogar$(1).dta
	find ../../output -type f ! -name '*.dta' -delete
endef

define ingresos_rules
../../temp/enigh$(1)_i_dta.zip: | ../../temp
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_ingresos_dta.zip -O ../../temp/enigh$(1)_i_dta.zip || { echo "Failed to download $(1) data"; exit 1; }

../../output/enigh$(1)_ingresos.dta: ../../temp/enigh$(1)_i_dta.zip | ../../output
	unzip -o -j ../../temp/enigh$(1)_i_dta.zip -d ../../output
	mv ../../output/ingresos.dta ../../output/ingresos$(1).dta
	find ../../output -type f ! -name '*.dta' -delete
endef

define poblacion_rules
../../temp/enigh$(1)_p_dta.zip: | ../../temp
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_poblacion_dta.zip -O ../../temp/enigh$(1)_p_dta.zip || { echo "Failed to download $(1) data"; exit 1; }

../../output/enigh$(1)_poblacion.dta: ../../temp/enigh$(1)_p_dta.zip | ../../output
	unzip -o -j ../../temp/enigh$(1)_p_dta.zip -d ../../output
	mv ../../output/poblacion.dta ../../output/poblacion$(1).dta
	find ../../output -type f ! -name '*.dta' -delete
endef

define trabajos_rules
../../temp/enigh$(1)_t_dta.zip: | ../../temp
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_trabajos_dta.zip -O ../../temp/enigh$(1)_t_dta.zip || { echo "Failed to download $(1) data"; exit 1; }

../../output/enigh$(1)_trabajos.dta: ../../temp/enigh$(1)_t_dta.zip | ../../output
	unzip -o -j ../../temp/enigh$(1)_t_dta.zip -d ../../output
	mv ../../output/trabajos.dta ../../output/trabajos$(1).dta
	find ../../output -type f ! -name '*.dta' -delete
endef

$(foreach year,$(YEARS),$(eval $(call concentradohogar_rules,$(year))))
$(foreach year,$(YEARS),$(eval $(call ingresos_rules,$(year))))
$(foreach year,$(YEARS),$(eval $(call poblacion_rules,$(year))))
$(foreach year,$(YEARS),$(eval $(call trabajos_rules,$(year))))