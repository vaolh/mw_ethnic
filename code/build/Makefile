### REPLICATION FILE: code/build/Makefile
### VERSION: GNU Make 3.81+
### AUTHORS: Matías Carrasco, Victor Ortega Le Hénanff
### DATE: 2026-05-03

#################################################
############### Configuration ###################
#################################################

DATADIR := ../../data
TEMPDIR := $(DATADIR)/temp

API_URL := https://en.www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/910392/en/0700/false/BIE-BISE/2.0/d024134c-be50-03ee-bdf6-1cda05612673?type=json

YEARS := 2024 2022 2020 2018 2016
YEARS_CONEVAL := 2022 2020 2018 2016
YEARS_AGRO := 2024 2022 2020
YEARS_JCF := 2024 2022 2020
YEARS_TARJETAS := 2024 2022 2020 2018 2016

#################################################
############### Main Targets ####################
#################################################

all: $(DATADIR)/clean/enigh/enigh-month.dta \
     $(DATADIR)/clean/enigh/enigh-year.dta \
     $(DATADIR)/clean/enigh/enigh-year.RData \
     $(DATADIR)/clean/enigh/enigh-household.dta \
     $(DATADIR)/clean/enigh/enigh-household-month.dta \
     parity

download: inpc enigh poverty
	rm -rf $(TEMPDIR)

inpc: $(DATADIR)/source/inpc/inpc.json

enigh: $(foreach yr,$(YEARS), \
     $(DATADIR)/source/enigh/concentradohogar$(yr).dta $(DATADIR)/source/enigh/ingresos$(yr).dta \
     $(DATADIR)/source/enigh/poblacion$(yr).dta $(DATADIR)/source/enigh/trabajos$(yr).dta \
     $(DATADIR)/source/enigh/viviendas$(yr).dta $(DATADIR)/source/enigh/hogares$(yr).dta \
     $(DATADIR)/source/enigh/erogaciones$(yr).dta $(DATADIR)/source/enigh/gastoshogar$(yr).dta \
     $(DATADIR)/source/enigh/gastospersona$(yr).dta $(DATADIR)/source/enigh/noagro$(yr).dta) \
     $(foreach yr,$(YEARS_TARJETAS),$(DATADIR)/source/enigh/gastotarjetas$(yr).dta) \
     $(foreach yr,$(YEARS_AGRO),$(DATADIR)/source/enigh/agroproductos$(yr).dta $(DATADIR)/source/enigh/agrogasto$(yr).dta $(DATADIR)/source/enigh/noagroimportes$(yr).dta) \
     $(foreach yr,$(YEARS_JCF),$(DATADIR)/source/enigh/ingresos_JCF$(yr).dta)

poverty: $(addprefix $(DATADIR)/source/coneval/, \
     pobreza2024.dta pobreza2022.dta pobreza2020.dta pobreza2018.dta pobreza2016.dta)

#################################################
############## Source Data Needed ###############
#################################################

SOURCE_DTA := $(foreach year, $(YEARS), \
  $(DATADIR)/source/enigh/ingresos$(year).dta \
  $(DATADIR)/source/enigh/poblacion$(year).dta \
  $(DATADIR)/source/enigh/concentradohogar$(year).dta \
  $(DATADIR)/source/enigh/viviendas$(year).dta \
  $(DATADIR)/source/enigh/trabajos$(year).dta \
  $(DATADIR)/source/enigh/gastospersona$(year).dta) \
  $(foreach year, $(YEARS), $(DATADIR)/source/coneval/pobreza$(year).dta)

SOURCE_DTA_HH := $(foreach year, $(YEARS), \
  $(DATADIR)/source/enigh/concentradohogar$(year).dta \
  $(DATADIR)/source/enigh/hogares$(year).dta \
  $(DATADIR)/source/enigh/poblacion$(year).dta)

HELPERS := _helpers.do _helpers.R

#################################################
############## Directory Creation ###############
#################################################

$(DATADIR)/source/enigh $(DATADIR)/source/coneval $(DATADIR)/source/inpc:
	mkdir -p $@

$(DATADIR)/clean/enigh $(DATADIR)/clean/inpc:
	mkdir -p $@

$(TEMPDIR):
	mkdir -p $@

log:
	mkdir -p $@

#################################################
########## Intermediate Zip Sentinels ###########
#################################################

# Declaring zip files as .INTERMEDIATE tells Make to skip building them
# (and therefore skip all downloads) when the final .dta target already exists.
.INTERMEDIATE: \
    $(foreach yr,$(YEARS), \
        $(TEMPDIR)/enigh$(yr)_dta.zip  $(TEMPDIR)/enigh$(yr)_i_dta.zip \
        $(TEMPDIR)/enigh$(yr)_p_dta.zip $(TEMPDIR)/enigh$(yr)_t_dta.zip \
        $(TEMPDIR)/enigh$(yr)_v_dta.zip $(TEMPDIR)/enigh$(yr)_h_dta.zip \
        $(TEMPDIR)/enigh$(yr)_er_dta.zip $(TEMPDIR)/enigh$(yr)_gh_dta.zip \
        $(TEMPDIR)/enigh$(yr)_gp_dta.zip $(TEMPDIR)/enigh$(yr)_na_dta.zip) \
    $(foreach yr,$(YEARS_TARJETAS),$(TEMPDIR)/enigh$(yr)_gt_dta.zip) \
    $(foreach yr,$(YEARS_AGRO), \
        $(TEMPDIR)/enigh$(yr)_ap_dta.zip $(TEMPDIR)/enigh$(yr)_ag_dta.zip \
        $(TEMPDIR)/enigh$(yr)_ni_dta.zip) \
    $(foreach yr,$(YEARS_JCF),$(TEMPDIR)/enigh$(yr)_jcf_dta.zip) \
    $(foreach yr,$(YEARS_CONEVAL),$(TEMPDIR)/STATA_MMP_$(yr).zip) \
    $(TEMPDIR)/pm_algoritmo_stata.zip

#################################################
############# INPC (Download) ###################
#################################################

$(DATADIR)/source/inpc/inpc.json: | $(DATADIR)/source/inpc
	curl -L -o $@ $(API_URL)

#################################################
############# INPC (Clean) ######################
#################################################

$(DATADIR)/clean/inpc/inpc.csv: inpc-year.R $(DATADIR)/source/inpc/inpc.json | $(DATADIR)/clean/inpc
	Rscript inpc-year.R

#################################################
######### Monthly Panel (Stata canonical) #######
#################################################

### Stata is the canonical builder for enigh-month.dta. R parity output
### is written to enigh-month-R.dta and validated by `make parity`.

$(DATADIR)/clean/enigh/enigh-month.dta: enigh-month.do $(HELPERS) $(SOURCE_DTA) \
        $(DATADIR)/clean/inpc/inpc.csv | $(DATADIR)/clean/enigh log
	stata-mp -e enigh-month.do
	@rm -f enigh-month.log

$(DATADIR)/clean/enigh/enigh-month-R.dta: enigh-month.R $(HELPERS) $(SOURCE_DTA) \
        $(DATADIR)/clean/inpc/inpc.csv | $(DATADIR)/clean/enigh
	Rscript enigh-month.R
	mv $(DATADIR)/clean/enigh/enigh-month.dta $(DATADIR)/clean/enigh/enigh-month-R.dta

#################################################
######### Yearly Cross-Section ##################
#################################################

### enigh-year aggregates from enigh-month + merges CONEVAL.
$(DATADIR)/clean/enigh/enigh-year.dta: enigh-year.do $(HELPERS) \
        $(DATADIR)/clean/enigh/enigh-month.dta | $(DATADIR)/clean/enigh log
	stata-mp -e enigh-year.do
	@rm -f enigh-year.log

$(DATADIR)/clean/enigh/enigh-year.RData $(DATADIR)/clean/enigh/enigh-year-R.dta: \
        enigh-year.R $(HELPERS) $(DATADIR)/clean/enigh/enigh-month.dta \
        | $(DATADIR)/clean/enigh
	Rscript enigh-year.R
	mv $(DATADIR)/clean/enigh/enigh-year.dta $(DATADIR)/clean/enigh/enigh-year-R.dta

#################################################
######### Household × Year ######################
#################################################

$(DATADIR)/clean/enigh/enigh-household.dta: enigh-household.do $(HELPERS) \
        $(DATADIR)/clean/enigh/enigh-month.dta $(SOURCE_DTA_HH) \
        | $(DATADIR)/clean/enigh log
	stata-mp -e enigh-household.do
	@rm -f enigh-household.log

$(DATADIR)/clean/enigh/enigh-household-R.dta: enigh-household.R $(HELPERS) \
        $(DATADIR)/clean/enigh/enigh-month.dta $(SOURCE_DTA_HH) \
        | $(DATADIR)/clean/enigh
	Rscript enigh-household.R
	mv $(DATADIR)/clean/enigh/enigh-household.dta $(DATADIR)/clean/enigh/enigh-household-R.dta

#################################################
######### Household × Month #####################
#################################################

$(DATADIR)/clean/enigh/enigh-household-month.dta: enigh-household-month.do \
        $(HELPERS) $(DATADIR)/clean/enigh/enigh-month.dta \
        | $(DATADIR)/clean/enigh log
	stata-mp -e enigh-household-month.do
	@rm -f enigh-household-month.log

$(DATADIR)/clean/enigh/enigh-household-month-R.dta: enigh-household-month.R \
        $(HELPERS) $(DATADIR)/clean/enigh/enigh-month.dta \
        | $(DATADIR)/clean/enigh
	Rscript enigh-household-month.R
	mv $(DATADIR)/clean/enigh/enigh-household-month.dta $(DATADIR)/clean/enigh/enigh-household-month-R.dta

#################################################
######### Parity Check (.do vs .R) ##############
#################################################

parity: \
    $(DATADIR)/clean/enigh/enigh-month.dta \
    $(DATADIR)/clean/enigh/enigh-month-R.dta \
    $(DATADIR)/clean/enigh/enigh-year.dta \
    $(DATADIR)/clean/enigh/enigh-year-R.dta \
    $(DATADIR)/clean/enigh/enigh-household.dta \
    $(DATADIR)/clean/enigh/enigh-household-R.dta \
    $(DATADIR)/clean/enigh/enigh-household-month.dta \
    $(DATADIR)/clean/enigh/enigh-household-month-R.dta
	Rscript compare-builds.R \
	    $(DATADIR)/clean/enigh/enigh-month.dta \
	    $(DATADIR)/clean/enigh/enigh-month-R.dta enigh-month
	Rscript compare-builds.R \
	    $(DATADIR)/clean/enigh/enigh-year.dta \
	    $(DATADIR)/clean/enigh/enigh-year-R.dta enigh-year
	Rscript compare-builds.R \
	    $(DATADIR)/clean/enigh/enigh-household.dta \
	    $(DATADIR)/clean/enigh/enigh-household-R.dta enigh-household
	Rscript compare-builds.R \
	    $(DATADIR)/clean/enigh/enigh-household-month.dta \
	    $(DATADIR)/clean/enigh/enigh-household-month-R.dta enigh-household-month

#################################################
############ ENIGH Download Rules ###############
#################################################

#  concentradohogar 
define concentradohogar_rules
$(TEMPDIR)/enigh$(1)_dta.zip: | $(TEMPDIR)
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_concentradohogar_dta.zip -O $$@ || { echo "Failed to download $(1) concentradohogar"; exit 1; }

$(DATADIR)/source/enigh/concentradohogar$(1).dta: $(TEMPDIR)/enigh$(1)_dta.zip | $(DATADIR)/source/enigh
	unzip -o -j $$< concentradohogar.dta -d $(DATADIR)/source/enigh
	mv $(DATADIR)/source/enigh/concentradohogar.dta $$@
endef

#  ingresos 
define ingresos_rules
$(TEMPDIR)/enigh$(1)_i_dta.zip: | $(TEMPDIR)
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_ingresos_dta.zip -O $$@ || { echo "Failed to download $(1) ingresos"; exit 1; }

$(DATADIR)/source/enigh/ingresos$(1).dta: $(TEMPDIR)/enigh$(1)_i_dta.zip | $(DATADIR)/source/enigh
	unzip -o -j $$< ingresos.dta -d $(DATADIR)/source/enigh
	mv $(DATADIR)/source/enigh/ingresos.dta $$@
endef

#  poblacion 
define poblacion_rules
$(TEMPDIR)/enigh$(1)_p_dta.zip: | $(TEMPDIR)
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_poblacion_dta.zip -O $$@ || { echo "Failed to download $(1) poblacion"; exit 1; }

$(DATADIR)/source/enigh/poblacion$(1).dta: $(TEMPDIR)/enigh$(1)_p_dta.zip | $(DATADIR)/source/enigh
	unzip -o -j $$< poblacion.dta -d $(DATADIR)/source/enigh
	mv $(DATADIR)/source/enigh/poblacion.dta $$@
endef

#  trabajos 
define trabajos_rules
$(TEMPDIR)/enigh$(1)_t_dta.zip: | $(TEMPDIR)
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_trabajos_dta.zip -O $$@ || { echo "Failed to download $(1) trabajos"; exit 1; }

$(DATADIR)/source/enigh/trabajos$(1).dta: $(TEMPDIR)/enigh$(1)_t_dta.zip | $(DATADIR)/source/enigh
	unzip -o -j $$< trabajos.dta -d $(DATADIR)/source/enigh
	mv $(DATADIR)/source/enigh/trabajos.dta $$@
endef

#  viviendas 
define viviendas_rules
$(TEMPDIR)/enigh$(1)_v_dta.zip: | $(TEMPDIR)
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_viviendas_dta.zip -O $$@ || { echo "Failed to download $(1) viviendas"; exit 1; }

$(DATADIR)/source/enigh/viviendas$(1).dta: $(TEMPDIR)/enigh$(1)_v_dta.zip | $(DATADIR)/source/enigh
	unzip -o -j $$< viviendas.dta -d $(DATADIR)/source/enigh
	mv $(DATADIR)/source/enigh/viviendas.dta $$@
endef

#  hogares 
define hogares_rules
$(TEMPDIR)/enigh$(1)_h_dta.zip: | $(TEMPDIR)
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_hogares_dta.zip -O $$@ || { echo "Failed to download $(1) hogares"; exit 1; }

$(DATADIR)/source/enigh/hogares$(1).dta: $(TEMPDIR)/enigh$(1)_h_dta.zip | $(DATADIR)/source/enigh
	unzip -o -j $$< hogares.dta -d $(DATADIR)/source/enigh
	mv $(DATADIR)/source/enigh/hogares.dta $$@
endef

#  erogaciones 
define erogaciones_rules
$(TEMPDIR)/enigh$(1)_er_dta.zip: | $(TEMPDIR)
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_erogaciones_dta.zip -O $$@ || { echo "Failed to download $(1) erogaciones"; exit 1; }

$(DATADIR)/source/enigh/erogaciones$(1).dta: $(TEMPDIR)/enigh$(1)_er_dta.zip | $(DATADIR)/source/enigh
	unzip -o -j $$< erogaciones.dta -d $(DATADIR)/source/enigh
	mv $(DATADIR)/source/enigh/erogaciones.dta $$@
endef

#  gastoshogar 
define gastoshogar_rules
$(TEMPDIR)/enigh$(1)_gh_dta.zip: | $(TEMPDIR)
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_gastoshogar_dta.zip -O $$@ || { echo "Failed to download $(1) gastoshogar"; exit 1; }

$(DATADIR)/source/enigh/gastoshogar$(1).dta: $(TEMPDIR)/enigh$(1)_gh_dta.zip | $(DATADIR)/source/enigh
	unzip -o -j $$< gastoshogar.dta -d $(DATADIR)/source/enigh
	mv $(DATADIR)/source/enigh/gastoshogar.dta $$@
endef

#  gastotarjetas 
define gastotarjetas_rules
$(TEMPDIR)/enigh$(1)_gt_dta.zip: | $(TEMPDIR)
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_gastotarjetas_dta.zip -O $$@ || { echo "Failed to download $(1) gastotarjetas"; exit 1; }

$(DATADIR)/source/enigh/gastotarjetas$(1).dta: $(TEMPDIR)/enigh$(1)_gt_dta.zip | $(DATADIR)/source/enigh
	unzip -o -j $$< gastotarjetas.dta -d $(DATADIR)/source/enigh
	mv $(DATADIR)/source/enigh/gastotarjetas.dta $$@
endef

#  gastospersona 
define gastospersona_rules
$(TEMPDIR)/enigh$(1)_gp_dta.zip: | $(TEMPDIR)
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_gastospersona_dta.zip -O $$@ || { echo "Failed to download $(1) gastospersona"; exit 1; }

$(DATADIR)/source/enigh/gastospersona$(1).dta: $(TEMPDIR)/enigh$(1)_gp_dta.zip | $(DATADIR)/source/enigh
	unzip -o -j $$< gastospersona.dta -d $(DATADIR)/source/enigh
	mv $(DATADIR)/source/enigh/gastospersona.dta $$@
endef

#  noagro 
define noagro_rules
$(TEMPDIR)/enigh$(1)_na_dta.zip: | $(TEMPDIR)
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_noagro_dta.zip -O $$@ || { echo "Failed to download $(1) noagro"; exit 1; }

$(DATADIR)/source/enigh/noagro$(1).dta: $(TEMPDIR)/enigh$(1)_na_dta.zip | $(DATADIR)/source/enigh
	unzip -o -j $$< noagro.dta -d $(DATADIR)/source/enigh
	mv $(DATADIR)/source/enigh/noagro.dta $$@
endef

#  agroproductos 
define agroproductos_rules
$(TEMPDIR)/enigh$(1)_ap_dta.zip: | $(TEMPDIR)
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_agroproductos_dta.zip -O $$@ || { echo "Failed to download $(1) agroproductos"; exit 1; }

$(DATADIR)/source/enigh/agroproductos$(1).dta: $(TEMPDIR)/enigh$(1)_ap_dta.zip | $(DATADIR)/source/enigh
	unzip -o -j $$< agroproductos.dta -d $(DATADIR)/source/enigh
	mv $(DATADIR)/source/enigh/agroproductos.dta $$@
endef

#  agrogasto 
define agrogasto_rules
$(TEMPDIR)/enigh$(1)_ag_dta.zip: | $(TEMPDIR)
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_agrogasto_dta.zip -O $$@ || { echo "Failed to download $(1) agrogasto"; exit 1; }

$(DATADIR)/source/enigh/agrogasto$(1).dta: $(TEMPDIR)/enigh$(1)_ag_dta.zip | $(DATADIR)/source/enigh
	unzip -o -j $$< agrogasto.dta -d $(DATADIR)/source/enigh
	mv $(DATADIR)/source/enigh/agrogasto.dta $$@
endef

#  noagroimportes 
define noagroimportes_rules
$(TEMPDIR)/enigh$(1)_ni_dta.zip: | $(TEMPDIR)
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_noagroimportes_dta.zip -O $$@ || { echo "Failed to download $(1) noagroimportes"; exit 1; }

$(DATADIR)/source/enigh/noagroimportes$(1).dta: $(TEMPDIR)/enigh$(1)_ni_dta.zip | $(DATADIR)/source/enigh
	unzip -o -j $$< noagroimportes.dta -d $(DATADIR)/source/enigh
	mv $(DATADIR)/source/enigh/noagroimportes.dta $$@
endef

#  ingresos_JCF 
define ingresos_JCF_rules
$(TEMPDIR)/enigh$(1)_jcf_dta.zip: | $(TEMPDIR)
	wget https://www.inegi.org.mx/contenidos/programas/enigh/nc/$(1)/microdatos/enigh$(1)_ns_ingresos_JCF_dta.zip -O $$@ || { echo "Failed to download $(1) ingresos_JCF"; exit 1; }

$(DATADIR)/source/enigh/ingresos_JCF$(1).dta: $(TEMPDIR)/enigh$(1)_jcf_dta.zip | $(DATADIR)/source/enigh
	unzip -o -j $$< ingresos_jcf.dta -d $(DATADIR)/source/enigh
	mv $(DATADIR)/source/enigh/ingresos_jcf.dta $$@
endef

#################################################
############# Generate Year Rules ###############
#################################################

$(foreach year,$(YEARS),$(eval $(call concentradohogar_rules,$(year))))
$(foreach year,$(YEARS),$(eval $(call ingresos_rules,$(year))))
$(foreach year,$(YEARS),$(eval $(call poblacion_rules,$(year))))
$(foreach year,$(YEARS),$(eval $(call trabajos_rules,$(year))))
$(foreach year,$(YEARS),$(eval $(call viviendas_rules,$(year))))
$(foreach year,$(YEARS),$(eval $(call hogares_rules,$(year))))
$(foreach year,$(YEARS),$(eval $(call erogaciones_rules,$(year))))
$(foreach year,$(YEARS),$(eval $(call gastoshogar_rules,$(year))))
$(foreach year,$(YEARS),$(eval $(call gastospersona_rules,$(year))))
$(foreach year,$(YEARS),$(eval $(call noagro_rules,$(year))))
$(foreach year,$(YEARS_AGRO),$(eval $(call agroproductos_rules,$(year))))
$(foreach year,$(YEARS_AGRO),$(eval $(call agrogasto_rules,$(year))))
$(foreach year,$(YEARS_AGRO),$(eval $(call noagroimportes_rules,$(year))))
$(foreach year,$(YEARS_TARJETAS),$(eval $(call gastotarjetas_rules,$(year))))
$(foreach year,$(YEARS_JCF),$(eval $(call ingresos_JCF_rules,$(year))))

#################################################
############ Poverty Download Rules #############
#################################################

# CONEVAL poverty data (2016-2022)
define poverty_coneval_rules
$(TEMPDIR)/STATA_MMP_$(1).zip: | $(TEMPDIR)
	@if [ "$(1)" = "2022" ]; then \
		wget -c -O $$@ https://www.coneval.org.mx/Medicion/MP/Documents/MMP_2022/Programas_calculo/STATA_MMP_2022.zip; \
	elif [ "$(1)" = "2020" ]; then \
		wget -c -O $$@ https://www.coneval.org.mx/Medicion/MP/Documents/Programas_calculo_pobreza_MMP_20/STATA_MMP_2020.zip; \
	elif [ "$(1)" = "2018" ]; then \
		wget -c -O $$@ https://www.coneval.org.mx/Medicion/MP/Documents/Programas_calculo_pobreza_MMP_18/STATA_MMP_2018.zip; \
	elif [ "$(1)" = "2016" ]; then \
		wget -c -O $$@ https://www.coneval.org.mx/Medicion/MP/Documents/Programas_calculo_pobreza_MMP_16/STATA_MMP_2016.zip; \
	fi

$(DATADIR)/source/coneval/pobreza$(1).dta: $(TEMPDIR)/STATA_MMP_$(1).zip | $(DATADIR)/source/coneval
	unzip -j -o $(TEMPDIR)/STATA_MMP_$(1).zip "*/pobreza_*.dta" -d $(DATADIR)/source/coneval || \
		unzip -j -o $(TEMPDIR)/STATA_MMP_$(1).zip "Base final/pobreza_*.dta" -d $(DATADIR)/source/coneval
	mv $(DATADIR)/source/coneval/pobreza_*.dta $$@
endef

$(foreach year,$(YEARS_CONEVAL),$(eval $(call poverty_coneval_rules,$(year))))

# INEGI poverty data for 2024
$(TEMPDIR)/pm_algoritmo_stata.zip: | $(TEMPDIR)
	wget -c -O $@ https://www.inegi.org.mx/contenidos/desarrollosocial/pm/doc/pm_algoritmo_stata.zip

$(DATADIR)/source/coneval/pobreza2024.dta: $(TEMPDIR)/pm_algoritmo_stata.zip | $(DATADIR)/source/coneval
	unzip -j -o $< "*/pobreza_*.dta" -d $(DATADIR)/source/coneval || \
		unzip -j -o $< "pobreza_*.dta" -d $(DATADIR)/source/coneval || \
		unzip -j -o $< "*/pobreza*.dta" -d $(DATADIR)/source/coneval
	mv $(DATADIR)/source/coneval/pobreza*.dta $@ 2>/dev/null || mv $(DATADIR)/source/coneval/pobreza_*.dta $@

#################################################
############## Phony Targets ####################
#################################################

.PHONY: all download inpc enigh poverty parity clean help

clean:
	rm -rf $(TEMPDIR)
	@rm -f *.log Rplots.pdf

help:
	@echo "Available targets:"
	@echo "  all      - Build all four cleaned datasets in Stata + R + parity"
	@echo "  download - Download all source data (INPC, ENIGH, poverty)"
	@echo "  parity   - Build R-side intermediates and run compare-builds.R"
	@echo "  clean    - Remove temp files"
	@echo ""
	@echo "Datasets produced:"
	@echo "  enigh-month.dta            (individual x month panel)"
	@echo "  enigh-year.dta             (individual x year cross-section)"
	@echo "  enigh-household.dta        (HH x year cross-section)"
	@echo "  enigh-household-month.dta  (HH x month panel)"
