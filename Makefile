SHELL := /bin/bash

SCALA_CLI ?= $(shell command -v scala-cli 2>/dev/null || printf '%s' "$(HOME)/.local/bin/scala-cli")
APPIMAGE ?= ../cpu2026-tools-x86_64.AppImage
GENERATED_RTL ?= ../CPU-submit/verilog/generated/student_top.sv
ISSUE_WIDTH ?= 4
ROB_DEPTH ?= 16
PHYSICAL_REGISTERS ?= 64
ISSUE_QUEUE_DEPTH ?= 8
ICACHE_BYTES ?= 4096
DCACHE_BYTES ?= 4096
CACHE_WAYS ?= 2
CACHE_LINE_BYTES ?= 64

SCALA_CLI_FLAGS := \
	--server=false \
	--ttl Inf \
	--suppress-outdated-dependency-warning \
	--suppress-directives-in-multiple-files-warning

.PHONY: compile generate lint clean

compile:
	env COURSIER_MODE=offline "$(SCALA_CLI)" compile . $(SCALA_CLI_FLAGS)

generate:
	env COURSIER_MODE=offline \
		CPU_ISSUE_WIDTH="$(ISSUE_WIDTH)" \
		CPU_ROB_DEPTH="$(ROB_DEPTH)" \
		CPU_PHYSICAL_REGISTERS="$(PHYSICAL_REGISTERS)" \
		CPU_ISSUE_QUEUE_DEPTH="$(ISSUE_QUEUE_DEPTH)" \
		CPU_ICACHE_BYTES="$(ICACHE_BYTES)" \
		CPU_DCACHE_BYTES="$(DCACHE_BYTES)" \
		CPU_CACHE_WAYS="$(CACHE_WAYS)" \
		CPU_CACHE_LINE_BYTES="$(CACHE_LINE_BYTES)" \
		"$(SCALA_CLI)" run . $(SCALA_CLI_FLAGS) \
		--main-class cpu2026.Generate -- "$(GENERATED_RTL)"

lint: generate
	APPIMAGE_EXTRACT_AND_RUN=1 "$(APPIMAGE)" verilator \
		--lint-only --top-module student_top -Wall -Wno-fatal "$(GENERATED_RTL)"

clean:
	rm -rf -- build .scala-build
