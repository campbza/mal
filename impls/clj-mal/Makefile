deps:
	lein deps

clj-mal.jar: src/clj-mal/*.clj
	lein uberjar

SHELL := bash
mal: clj-mal.jar
	cat <(echo -e '#!/bin/sh\nexec java -jar "$$0" "$$@"') clj-mal.jar > $@
	chmod +x clj-mal

src/clj-mal/%.clj:
	@true

target/%.jar: src/clj-mal/%.clj src/clj-mal/*.clj
	lein with-profile $(word 1,$(subst _, ,$*)) uberjar

clean:
	rm -rf target/ mal.jar mal
