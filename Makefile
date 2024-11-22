SHELL := /bin/bash

ERLC_USE_SERVER ?= true
export ERLC_USE_SERVER

ERLC=erlc
ERLC_FLAGS= +debug_info +compressed 

BEAM=$(patsubst %.erl,%.beam,$(wildcard src/*.erl))
TBEAM=$(patsubst %.erl,%.beam,$(wildcard test/*.erl))

REBAR3_URL=https://s3.amazonaws.com/rebar3/rebar3
WGET=$(shell which wget)
CURL=$(shell which curl)

.PHONY: all old test clean
all: rebar3 compile
old: old-get-deps $(BEAM)
test: $(TBEAM) unit

unit:
	erl -noshell  -s test_yaws_appmod_router test -s init stop

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o ebin $<

clean:
	rm -f ebin/*.beam
	rm -rf _build

# -----------------------
# D E P E N D E N C I E S
# -----------------------
.PHONY: compile get-deps old_deps yaws_record-dep rm-deps

ifeq ($(REBAR3),)
REBAR3=./rebar3
else
REBAR3=$(REBAR3)
endif
compile:
	./rebar3 compile

get-deps: rebar3 old_deps 

old-get-deps: old_deps yaws_record-dep

ifeq ($(WGET),)
rebar3:
	$(CURL) -O $(REBAR3_URL) && chmod +x rebar3
else
rebar3:
	$(WGET) $(REBAR3_URL) && chmod +x rebar3
endif

old_deps:
	if [ ! -d deps ]; then \
	  mkdir deps; \
	fi

yaws_record-dep:
	if [ ! -d deps/yaws ]; then \
	  cd deps; \
	  git clone https://github.com/erlyaws/yaws.git; \
	  make -C yaws; \
	fi

rm-deps:
	rm -rf deps rebar3

-PHONY: docs
docs:
	rebar3 ex_doc
