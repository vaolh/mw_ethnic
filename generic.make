../input ../output ../temp ../report:
	mkdir -p $@

.PRECIOUS: ../../%
../../%:
	$(MAKE) -C $(subst output/,code/,$(dir $@)) ../output/$(notdir $@)