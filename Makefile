#######################################################
# Configure this.
################################################# # # #

# IMPORTANT:
# This Makefile is just a thin wrapper around the 'stack' command.
# It is intended as a shortcut for developers only.

# Directories for building and outputs
SRC_DIR := .

# Log file, will be deleted before every make run
LOGFILE := local/make.log

# FIXME: tool locations should be autodetected or configured in /local
TOOL_STACK := $(shell which stack)

#######################################################
# Don't change this
################################################# # # #

# OS detection and info
OSNAME := $(shell uname -s)

ifeq ($(OS),Windows_NT)
    ifneq (,$(findstring CYGWIN,$(OSNAME)))
        # Detected cygwin
        MY_OS := cygwin
    else
        MY_OS := winnt
    endif
else
    ifneq (,$(findstring Darwin,$(OSNAME)))
        MY_OS := osx
    else
    	# Default
        MY_OS := linux
    endif
endif
# $(info Detected OS: $(MY_OS))

# Delete the make logfile
$(shell rm -f $(LOGFILE) >/dev/null)

#######################################################
# Targets
################################################# # # #

.PHONY: default build doc release bench-all info check-tools clean distclean

default: build

build:
	stack build --fast

doc:
	stack build --fast --haddock --open

release:
	stack build --test --haddock

bench-all:
	stack bench --benchmark-arguments '--output=$$benchmark.html'

info:
		@echo "--------------------------------------------------------------------------------"
		@echo "Directories"
		@echo "    SRC_DIR         : $(SRC_DIR)"
		@echo "--------------------------------------------------------------------------------"
		@echo "Output files"
		@$(foreach file,$(DOC_OUTPUTS),echo "    $(file)";)
		@echo "--------------------------------------------------------------------------------"
# 		@echo "System and tools"
# 		@echo "    $(OS)"
# 		@echo "    $(OSNAME)"
# 		@echo "--------------------------------------------------------------------------------"

check-tools:
		$(TOOL_STACK) --version | head -n 1

clean:
		$(TOOL_STACK) clean

distclean:
		$(TOOL_STACK) clean --full
		# Remove benchmarking & profiling reports
		-@rm nondet/nondet-benchmarks.html
