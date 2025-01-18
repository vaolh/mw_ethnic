../input ../output ../temp ../report:
	mkdir $@

.PRECIOUS: ../../%
../../%:
	$(MAKE) -C $(subst output/,code/,$(dir $@)) ../output/$(notdir $@)