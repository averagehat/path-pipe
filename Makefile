MINICONDA=
PRICESOURCE=PriceSource140408.tar.gz 
BIN=$(MINICONDA)/bin

$(BIN)/conda:
	wget https://repo.continuum.io/miniconda/Miniconda2-latest-Linux-x86_64.sh 
	bash Miniconda2-latest-Linux-x86_64.sh -b -p $(MINICONDA)

$(BIN)/PriceSeqFilter:
	wget http://derisilab.ucsf.edu/software/price/$(PRICESOURCE)
	tar -xvf $(PRICESOURCE)
	cd $(PRICESOURCE) && $(MAKE) && ln -s $(PWD)/PriceSeqFilter $@

$(BIN)/gmap:
	conda install gmap

$(BIN)/RepeatMasker:
	conda install repeatmasker

$(BIN)/STAR:
	conda install star

$(BIN)/rapsearch:
	git clone https://github.com/zhaoyanswill/RAPSearch2
	cd RAPSearch2
	./install
	ln -s $(PWD)/bin/rapsearch $(BIN)/rapsearch
	
$(BIN)/cd-hit-dup:
	git clone https://github.com/weizhongli/cdhit
	cd cdhit/cd-hit-auxtools/ && $(MAKE) && ln -s $(PWD)/cd-hit-dup $(BIN)/cd-hit-dup
