BB ?= bb
NPM ?= npm
EDN_DIR ?= build/edn
SITE_DIR ?= build/ox-edn-site
URL_PREFIX ?= /notes/
PORT ?= 8070
OX_EDN_DIR ?= /home/disk/Dev/ox-edn/build/edn

.PHONY: test site site-ox-edn serve clean npm-install

node_modules/.package-lock: package-lock.json
	$(NPM) install

npm-install: node_modules/.package-lock

test: node_modules/.package-lock
	$(BB) test

site: node_modules/.package-lock
	$(NPM) run prepare-assets
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
