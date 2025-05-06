all:
	make -C downloaddata/code
	make -C enighdata/code
	make -C wage_tables/code
	make -C wage_plots/code
	make -C income_tables/code
	make -C income_plots/code