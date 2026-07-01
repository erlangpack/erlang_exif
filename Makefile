REBAR := ./rebar3
REBAR_URL := https://s3.amazonaws.com/rebar3/rebar3
ERL       ?= erl

.PHONY: all compile shell test clean clean_doc xref dialyzer doc doc_private

all: compile

compile: $(REBAR)
	$(REBAR) compile

shell: $(REBAR)
	$(REBAR) shell

test: $(REBAR)
	$(REBAR) as test ct --readable true -c

clean: $(REBAR) clean_doc
	$(REBAR) clean

clean_doc:
	@rm -rf doc

xref: $(REBAR)
	$(REBAR) as test xref

dialyzer: $(REBAR)
	$(REBAR) as check dialyzer

doc: $(REBAR)
	$(REBAR) ex_doc

doc_private: $(REBAR)
	$(REBAR) as doc_private edoc

./rebar3:
	$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar3"}])' \
	  -s init stop
	chmod +x ./rebar3
