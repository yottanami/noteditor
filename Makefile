my_dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

.PHONY: update
update:
	@git pull origin master

.PHONY: install
install:
	/bin/sh $(my_dir)/scripts/install_files.sh runners
	@echo " "
	@echo "------------------------------------------------------------------------------------"
	@echo "Make sure to install external dependencies. For more info checkout README.org"

.PHONY: install-extras
install-extras:
	/bin/sh $(my_dir)/scripts/install_files.sh extras
