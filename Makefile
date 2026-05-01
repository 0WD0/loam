BB ?= bb
NPM ?= npm
EDN_DIR ?= build/edn
SITE_DIR ?= build/ox-edn-site
URL_PREFIX ?= /notes/
PORT ?= 8070
OX_EDN_DIR ?= /home/disk/Dev/ox-edn/build/edn

.PHONY: test site site-ox-edn serve clean

test:
	$(BB) test

site: node_modules/.package-lock
	$(BB) -cp src -m loam.site \
	  --edn-dir "$(EDN_DIR)" \
	  --output-dir "$(SITE_DIR)" \
	  --url-prefix "$(URL_PREFIX)"

site-ox-edn:
	$(MAKE) site EDN_DIR="$(OX_EDN_DIR)"

serve:
	cd "$(SITE_DIR)" && python3 -m http.server "$(PORT)"

clean:
	rm -rf build
