all:
	make -C data/deflators/code
	make -C data/download/code
	make -C data/enigh/code
	make -C tables/code
	make -C figs/code