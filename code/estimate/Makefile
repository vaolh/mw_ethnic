### REPLICATION FILE: code/estimate/Makefile
### VERSION: GNU Make 3.81+
### AUTHORS: Matías Carrasco, Victor Ortega Le Hénanff
### DATE: 2026-05-07

DATADIR  := ../../data
PAPERDIR := ../../paper
PYTHON   := ../../.venv/bin/python

#################################################
############### Clean Datasets ##################
#################################################

DATA_INDLEVEL_YEAR  := $(DATADIR)/clean/enigh/enigh-indlevel-inc-year.dta
DATA_INDLEVEL_MONTH := $(DATADIR)/clean/enigh/enigh-indlevel-inc-month.dta
DATA_HHLEVEL_INC_YEAR  := $(DATADIR)/clean/enigh/enigh-hhlevel-inc-year.dta
DATA_HHLEVEL_INC_MONTH := $(DATADIR)/clean/enigh/enigh-hhlevel-inc-month.dta
DATA_HHLEVEL_EXP_MONTH := $(DATADIR)/clean/enigh/enigh-hhlevel-exp-month.dta

#################################################
################ Readers + helpers ##############
#################################################

HELPERS              := _helpers.do
READ_INDLEVEL_YEAR   := read-indlevel-inc-year.do
READ_INDLEVEL_MONTH  := read-indlevel-inc-month.do
READ_HHLEVEL_INC_Y   := read-hhlevel-inc-year.do
READ_HHLEVEL_INC_M   := read-hhlevel-inc-month.do
READ_HHLEVEL_EXP_M   := read-hhlevel-exp-month.do

#################################################
######## Pre-DiD Exp figure variable defs ########
#################################################
# Defined here (before STATA_FIGS) so prereqs of stata_figs see them.

PLOT_EXP_EVENT_FIGS := \
	$(PAPERDIR)/figures/event/plot-event-exp-hli-nonind-year.png \
	$(PAPERDIR)/figures/event/plot-event-exp-hli-ind-year.png \
	$(PAPERDIR)/figures/event/plot-event-exp-indig-nonind-year.png \
	$(PAPERDIR)/figures/event/plot-event-exp-indig-ind-year.png \
	$(PAPERDIR)/figures/event/plot-event-exp-hli-nonind-month.png \
	$(PAPERDIR)/figures/event/plot-event-exp-hli-ind-month.png \
	$(PAPERDIR)/figures/event/plot-event-exp-indig-nonind-month.png \
	$(PAPERDIR)/figures/event/plot-event-exp-indig-ind-month.png
PLOT_EXP_EVENT_REST := $(filter-out $(PAPERDIR)/figures/event/plot-event-exp-hli-nonind-year.png,$(PLOT_EXP_EVENT_FIGS))

PLOT_EXP_DENS_FIGS := \
	$(PAPERDIR)/figures/density/plot-density-exp-hli-pre.png \
	$(PAPERDIR)/figures/density/plot-density-exp-hli-post.png \
	$(PAPERDIR)/figures/density/plot-density-exp-indig-pre.png \
	$(PAPERDIR)/figures/density/plot-density-exp-indig-post.png
PLOT_EXP_DENS_REST := $(filter-out $(PAPERDIR)/figures/density/plot-density-exp-hli-pre.png,$(PLOT_EXP_DENS_FIGS))

#################################################
############### Targets — Tables ################
#################################################

all: tables figures

# One DiD .do per cleaned dataset; each script writes multiple tables.
TABLES_INDLEVEL_YEAR := \
	$(PAPERDIR)/tables/did-wage-ethnic-year.tex          \
	$(PAPERDIR)/tables/did-income-ethnic-year.tex        \
	$(PAPERDIR)/tables/did-wage-deciles-year.tex         \
	$(PAPERDIR)/tables/did-wage-informality-year.tex     \
	$(PAPERDIR)/tables/did-wage-sector-year.tex          \
	$(PAPERDIR)/tables/sum-sector-shares-year.tex

TABLES_INDLEVEL_MONTH := \
	$(PAPERDIR)/tables/did-wage-ethnic-month.tex         \
	$(PAPERDIR)/tables/did-income-ethnic-month.tex       \
	$(PAPERDIR)/tables/did-wage-deciles-month.tex        \
	$(PAPERDIR)/tables/did-wage-informality-month.tex    \
	$(PAPERDIR)/tables/did-wage-sector-month.tex         \
	$(PAPERDIR)/tables/sum-sector-shares-month.tex

TABLES_HHLEVEL_INC_Y := \
	$(PAPERDIR)/tables/did-wage-hhlevel-year.tex         \
	$(PAPERDIR)/tables/did-income-hhlevel-year.tex

TABLES_HHLEVEL_INC_M := \
	$(PAPERDIR)/tables/did-wage-hhlevel-month.tex        \
	$(PAPERDIR)/tables/did-income-hhlevel-month.tex

TABLES_HHLEVEL_EXP_M := \
	$(PAPERDIR)/tables/did-exp-hhlevel-month.tex

TABLES := \
	$(PAPERDIR)/tables/sum-descriptive.tex   \
	$(TABLES_INDLEVEL_YEAR)                  \
	$(TABLES_INDLEVEL_MONTH)                 \
	$(TABLES_HHLEVEL_INC_Y)                  \
	$(TABLES_HHLEVEL_INC_M)                  \
	$(TABLES_HHLEVEL_EXP_M)

tables: $(TABLES)

#################################################
########## Directory Creation Stamps ############
#################################################

$(PAPERDIR)/figures $(PAPERDIR)/figures/density $(PAPERDIR)/figures/event \
$(PAPERDIR)/figures/did $(PAPERDIR)/figures/sum:
	mkdir -p $@

$(PAPERDIR)/tables:
	mkdir -p $@

log:
	mkdir -p $@

#################################################
############# Stata DiD recipes #################
#################################################

# sum-descriptive (single .do)
$(PAPERDIR)/tables/sum-descriptive.tex: sum-descriptive.do $(READ_INDLEVEL_YEAR) $(DATA_INDLEVEL_YEAR) | $(PAPERDIR)/tables log
	stata-mp -e $<
	@rm -f $(<:.do=.log)

# Group-by-dataset DiDs: each .do file produces several .tex tables. To avoid
# re-running the script once per output, we make the first table the "primary"
# target carrying the recipe; the rest depend on the primary as a sentinel.

# --- did-indlevel-inc-year.do (primary: did-wage-ethnic-year.tex)
TABLES_INDLEVEL_YEAR_REST := $(filter-out $(PAPERDIR)/tables/did-wage-ethnic-year.tex,$(TABLES_INDLEVEL_YEAR))

$(PAPERDIR)/tables/did-wage-ethnic-year.tex: did-indlevel-inc-year.do $(HELPERS) $(READ_INDLEVEL_YEAR) $(DATA_INDLEVEL_YEAR) | $(PAPERDIR)/tables log
	stata-mp -e $<
	@rm -f $(<:.do=.log)
$(TABLES_INDLEVEL_YEAR_REST): $(PAPERDIR)/tables/did-wage-ethnic-year.tex
	@:

# --- did-indlevel-inc-month.do (primary: did-wage-ethnic-month.tex)
TABLES_INDLEVEL_MONTH_REST := $(filter-out $(PAPERDIR)/tables/did-wage-ethnic-month.tex,$(TABLES_INDLEVEL_MONTH))

$(PAPERDIR)/tables/did-wage-ethnic-month.tex: did-indlevel-inc-month.do $(HELPERS) $(READ_INDLEVEL_MONTH) $(DATA_INDLEVEL_MONTH) | $(PAPERDIR)/tables log
	stata-mp -e $<
	@rm -f $(<:.do=.log)
$(TABLES_INDLEVEL_MONTH_REST): $(PAPERDIR)/tables/did-wage-ethnic-month.tex
	@:

# --- did-hhlevel-inc-year.do (primary: did-wage-hhlevel-year.tex)
TABLES_HHLEVEL_INC_Y_REST := $(filter-out $(PAPERDIR)/tables/did-wage-hhlevel-year.tex,$(TABLES_HHLEVEL_INC_Y))

$(PAPERDIR)/tables/did-wage-hhlevel-year.tex: did-hhlevel-inc-year.do $(HELPERS) $(READ_HHLEVEL_INC_Y) $(DATA_HHLEVEL_INC_YEAR) | $(PAPERDIR)/tables log
	stata-mp -e $<
	@rm -f $(<:.do=.log)
$(TABLES_HHLEVEL_INC_Y_REST): $(PAPERDIR)/tables/did-wage-hhlevel-year.tex
	@:

# --- did-hhlevel-inc-month.do (primary: did-wage-hhlevel-month.tex)
TABLES_HHLEVEL_INC_M_REST := $(filter-out $(PAPERDIR)/tables/did-wage-hhlevel-month.tex,$(TABLES_HHLEVEL_INC_M))

$(PAPERDIR)/tables/did-wage-hhlevel-month.tex: did-hhlevel-inc-month.do $(HELPERS) $(READ_HHLEVEL_INC_M) $(DATA_HHLEVEL_INC_MONTH) | $(PAPERDIR)/tables log
	stata-mp -e $<
	@rm -f $(<:.do=.log)
$(TABLES_HHLEVEL_INC_M_REST): $(PAPERDIR)/tables/did-wage-hhlevel-month.tex
	@:

# --- did-hhlevel-exp-month.do (single output)
$(PAPERDIR)/tables/did-exp-hhlevel-month.tex: did-hhlevel-exp-month.do $(HELPERS) $(READ_HHLEVEL_EXP_M) \
                         $(DATA_HHLEVEL_EXP_MONTH) $(DATA_HHLEVEL_INC_MONTH) | $(PAPERDIR)/tables log
	stata-mp -e $<
	@rm -f $(<:.do=.log)

#################################################
################ Targets — Figures ##############
#################################################

figures: stata_figs r_figs python_figs

STATA_FIGS = \
	$(PAPERDIR)/figures/event/plot-event-wage-hli-nonind-year.png    \
	$(PAPERDIR)/figures/event/plot-event-wage-hli-ind-year.png       \
	$(PAPERDIR)/figures/event/plot-event-wage-indig-nonind-year.png  \
	$(PAPERDIR)/figures/event/plot-event-wage-indig-ind-year.png     \
	$(PAPERDIR)/figures/event/plot-event-wage-hli-nonind-month.png   \
	$(PAPERDIR)/figures/event/plot-event-wage-hli-ind-month.png      \
	$(PAPERDIR)/figures/event/plot-event-wage-indig-nonind-month.png \
	$(PAPERDIR)/figures/event/plot-event-wage-indig-ind-month.png    \
	$(PAPERDIR)/figures/event/plot-event-income-hli-nonind-year.png  \
	$(PAPERDIR)/figures/event/plot-event-income-hli-ind-year.png     \
	$(PAPERDIR)/figures/event/plot-event-income-indig-nonind-year.png \
	$(PAPERDIR)/figures/event/plot-event-income-indig-ind-year.png   \
	$(PAPERDIR)/figures/event/plot-event-income-hli-nonind-month.png \
	$(PAPERDIR)/figures/event/plot-event-income-hli-ind-month.png    \
	$(PAPERDIR)/figures/event/plot-event-income-indig-nonind-month.png \
	$(PAPERDIR)/figures/event/plot-event-income-indig-ind-month.png  \
	$(PAPERDIR)/figures/did/plot-did-wage-centiles-year.png        \
	$(PAPERDIR)/figures/did/plot-did-wage-centiles-month.png       \
	$(PAPERDIR)/figures/density/plot-density-wages-ethnic-pretreat.png \
	$(PLOT_EXP_EVENT_FIGS) \
	$(PAPERDIR)/figures/sum/sum-exp-trends-hli.png \
	$(PAPERDIR)/figures/sum/sum-exp-trends-indig.png \
	$(PLOT_EXP_DENS_FIGS) \
	$(PAPERDIR)/figures/sum/sum-exp-composition.png

stata_figs: $(STATA_FIGS)

# Each plot script writes both year + month versions of its plot family.
# Same primary-target sentinel pattern as for the table recipes above.

# --- plot-event-wage-ethnic.do (primary: hli-nonind-year)
PLOT_WAGE_ETHNIC_FIGS := \
	$(PAPERDIR)/figures/event/plot-event-wage-hli-nonind-year.png \
	$(PAPERDIR)/figures/event/plot-event-wage-hli-ind-year.png \
	$(PAPERDIR)/figures/event/plot-event-wage-indig-nonind-year.png \
	$(PAPERDIR)/figures/event/plot-event-wage-indig-ind-year.png \
	$(PAPERDIR)/figures/event/plot-event-wage-hli-nonind-month.png \
	$(PAPERDIR)/figures/event/plot-event-wage-hli-ind-month.png \
	$(PAPERDIR)/figures/event/plot-event-wage-indig-nonind-month.png \
	$(PAPERDIR)/figures/event/plot-event-wage-indig-ind-month.png
PLOT_WAGE_ETHNIC_REST := $(filter-out $(PAPERDIR)/figures/event/plot-event-wage-hli-nonind-year.png,$(PLOT_WAGE_ETHNIC_FIGS))

$(PAPERDIR)/figures/event/plot-event-wage-hli-nonind-year.png: \
        plot-event-wage-ethnic.do $(HELPERS) $(READ_INDLEVEL_YEAR) $(READ_INDLEVEL_MONTH) \
        $(DATA_INDLEVEL_YEAR) $(DATA_INDLEVEL_MONTH) | $(PAPERDIR)/figures/event log
	stata-mp -e $<
	@rm -f $(<:.do=.log)
$(PLOT_WAGE_ETHNIC_REST): $(PAPERDIR)/figures/event/plot-event-wage-hli-nonind-year.png
	@:

# --- plot-event-income-ethnic.do (primary: hli-nonind-year)
PLOT_INCOME_ETHNIC_FIGS := \
	$(PAPERDIR)/figures/event/plot-event-income-hli-nonind-year.png \
	$(PAPERDIR)/figures/event/plot-event-income-hli-ind-year.png \
	$(PAPERDIR)/figures/event/plot-event-income-indig-nonind-year.png \
	$(PAPERDIR)/figures/event/plot-event-income-indig-ind-year.png \
	$(PAPERDIR)/figures/event/plot-event-income-hli-nonind-month.png \
	$(PAPERDIR)/figures/event/plot-event-income-hli-ind-month.png \
	$(PAPERDIR)/figures/event/plot-event-income-indig-nonind-month.png \
	$(PAPERDIR)/figures/event/plot-event-income-indig-ind-month.png
PLOT_INCOME_ETHNIC_REST := $(filter-out $(PAPERDIR)/figures/event/plot-event-income-hli-nonind-year.png,$(PLOT_INCOME_ETHNIC_FIGS))

$(PAPERDIR)/figures/event/plot-event-income-hli-nonind-year.png: \
        plot-event-income-ethnic.do $(HELPERS) $(READ_INDLEVEL_YEAR) $(READ_INDLEVEL_MONTH) \
        $(DATA_INDLEVEL_YEAR) $(DATA_INDLEVEL_MONTH) | $(PAPERDIR)/figures/event log
	stata-mp -e $<
	@rm -f $(<:.do=.log)
$(PLOT_INCOME_ETHNIC_REST): $(PAPERDIR)/figures/event/plot-event-income-hli-nonind-year.png
	@:

# --- plot-event-wage-centiles.do (primary: year)
$(PAPERDIR)/figures/did/plot-did-wage-centiles-year.png: \
        plot-event-wage-centiles.do $(READ_INDLEVEL_YEAR) $(READ_INDLEVEL_MONTH) \
        $(DATA_INDLEVEL_YEAR) $(DATA_INDLEVEL_MONTH) | $(PAPERDIR)/figures/did log
	stata-mp -e $<
	@rm -f $(<:.do=.log)
$(PAPERDIR)/figures/did/plot-did-wage-centiles-month.png: $(PAPERDIR)/figures/did/plot-did-wage-centiles-year.png
	@:

$(PAPERDIR)/figures/density/plot-density-wages-ethnic-pretreat.png: plot-densities.do $(READ_INDLEVEL_YEAR) $(DATA_INDLEVEL_YEAR) | $(PAPERDIR)/figures/density log
	stata-mp -e $<
	@rm -f $(<:.do=.log)

#################################################
########## Pre-DiD Expenditure Figures ##########
#################################################

# plot-event-exp-ethnic.do — 8 event-study figures (year + month × 4 strata)
$(PAPERDIR)/figures/event/plot-event-exp-hli-nonind-year.png: \
        plot-event-exp-ethnic.do $(HELPERS) $(READ_HHLEVEL_EXP_M) \
        $(DATA_HHLEVEL_EXP_MONTH) $(DATA_HHLEVEL_INC_MONTH) | $(PAPERDIR)/figures/event log
	stata-mp -e $<
	@rm -f $(<:.do=.log)
$(PLOT_EXP_EVENT_REST): $(PAPERDIR)/figures/event/plot-event-exp-hli-nonind-year.png
	@:

# sum-exp-trends.do — 2 trend-line figures
$(PAPERDIR)/figures/sum/sum-exp-trends-hli.png: sum-exp-trends.do $(HELPERS) $(READ_HHLEVEL_EXP_M) \
        $(DATA_HHLEVEL_EXP_MONTH) $(DATA_HHLEVEL_INC_MONTH) | $(PAPERDIR)/figures/sum log
	stata-mp -e $<
	@rm -f $(<:.do=.log)
$(PAPERDIR)/figures/sum/sum-exp-trends-indig.png: $(PAPERDIR)/figures/sum/sum-exp-trends-hli.png
	@:

# plot-density-exp.do — 4 kernel-density figures
$(PAPERDIR)/figures/density/plot-density-exp-hli-pre.png: plot-density-exp.do $(HELPERS) $(READ_HHLEVEL_EXP_M) \
        $(DATA_HHLEVEL_EXP_MONTH) $(DATA_HHLEVEL_INC_MONTH) | $(PAPERDIR)/figures/density log
	stata-mp -e $<
	@rm -f $(<:.do=.log)
$(PLOT_EXP_DENS_REST): $(PAPERDIR)/figures/density/plot-density-exp-hli-pre.png
	@:

# sum-exp-composition.do — 1 stacked-bar figure
$(PAPERDIR)/figures/sum/sum-exp-composition.png: sum-exp-composition.do $(HELPERS) \
        $(DATA_HHLEVEL_EXP_MONTH) $(DATA_HHLEVEL_INC_MONTH) | $(PAPERDIR)/figures/sum log
	stata-mp -e $<
	@rm -f $(<:.do=.log)

#################################################
################ R figures ######################
#################################################

r_figs: $(PAPERDIR)/figures/sum/sum-centile-ictpc.png

$(PAPERDIR)/figures/sum/sum-centile-ictpc.png: sum-inequality.R $(DATADIR)/clean/enigh/enigh-indlevel-inc-year.RData | $(PAPERDIR)/figures/sum
	Rscript $<
	@rm -f Rplots.pdf

#################################################
################ Python figures #################
#################################################

python_figs: $(PAPERDIR)/figures/sum/sum-gini-state-change.png

$(PAPERDIR)/figures/sum/sum-gini-state-change.png: sum-mapgini.py $(DATA_INDLEVEL_YEAR) | $(PAPERDIR)/figures/sum
	$(PYTHON) $<

#################################################
################# Phony / Clean #################
#################################################

.PHONY: all tables figures stata_figs r_figs python_figs clean clean-logs

clean:
	@rm -f *.log Rplots.pdf

clean-logs:
	@find . -maxdepth 1 -name "*.log" -delete
