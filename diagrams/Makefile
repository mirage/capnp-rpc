# Requires https://github.com/blampe/goat

all: ping.svg callback1.svg callback2.svg callback3.svg vats.svg pipeline.svg three_vats.svg

%.svg: %.txt
	goat $^ > $@
