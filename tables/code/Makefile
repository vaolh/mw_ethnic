all: \
	../output/ZLFNincomedid.tex \
	../output/NEGOincomedid.tex \
	../output/descriptivetables.tex \
	../output/indigdid.tex \
	../output/indspeakdid.tex

include ../../generic.make

# symlink to enighdata output
../input/enighdata.dta: | ../input
	ln -sf ../../../data/enighdata/output/enighdata.dta ../input/enighdata.dta

# run any .do to .tex, given its .dta deps
../output/%.tex: %.do ../input/enighdata.dta | ../output
	stata-se -e $<

../output/ZLFNincomedid.tex: ZLFNincomedid.do
../output/NEGOincomedid.tex: NEGOCincomedid.do
../output/descriptivetables.tex: descriptivetables.do
../output/indigdid.tex: indigdid.do
../output/indspeakdid.tex: indspeakdid.do
