BB ?= bb
NPM ?= npm
EDN_DIR ?= build/edn
SITE_DIR ?= build/site
URL_PREFIX ?= /notes/
PORT ?= 8080

.PHONY: test site serve clean

test:
	$(BB) test

site: node_modules/.package-lock
	$(BB) -cp src -m loam.site \
	  --edn-dir "$(EDN_DIR)" \
	  --output-dir "$(SITE_DIR)" \
	  --url-prefix "$(URL_PREFIX)"

serve:
	cd "$(SITE_DIR)" && python3 -m http.server "$(PORT)"

clean:
	rm -rf build
