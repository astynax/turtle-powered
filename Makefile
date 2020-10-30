.PHONY: all
all:
	cabal build

.PHONY: install
install:
	cabal install \
		--installdir=$$HOME/.local/bin \
		--install-method=copy \
		--overwrite-policy=always
