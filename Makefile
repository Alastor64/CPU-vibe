SHELL := /bin/bash

SCALA_CLI ?= $(shell command -v scala-cli 2>/dev/null || printf '%s' "$(HOME)/.local/bin/scala-cli")
APPIMAGE ?= ../cpu2026-tools-x86_64.AppImage
GENERATED_RTL ?= ../CPU-submit/verilog/generated/student_top.sv

SCALA_CLI_FLAGS := \
	--server=false \
	--ttl Inf \
	--suppress-outdated-dependency-warning \
	--suppress-directives-in-multiple-files-warning

.PHONY: compile generate lint clean

compile:
	env COURSIER_MODE=offline "$(SCALA_CLI)" compile . $(SCALA_CLI_FLAGS)

generate:
	env COURSIER_MODE=offline "$(SCALA_CLI)" run . $(SCALA_CLI_FLAGS) \
		--main-class cpu2026.Generate -- "$(GENERATED_RTL)"

lint: generate
	APPIMAGE_EXTRACT_AND_RUN=1 "$(APPIMAGE)" verilator \
		--lint-only --top-module student_top -Wall -Wno-fatal "$(GENERATED_RTL)"

clean:
	rm -rf -- build .scala-build
