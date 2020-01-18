REBAR = rebar3

deploy: compile run

compile:
	@$(REBAR) compile

run:
	@$(REBAR) shell

doc:
	@$(REBAR) edoc

dialyzer:
	@$(REBAR) dialyzer

test:
	@$(REBAR) eunit

publish:
	@./publi.sh

clean:
	@rm -rf _build
	@rm -rf rebar3.crashdump
	@rm -rf steamroller.crashdump
	@rm -rf erl_crash.dump
	@rm -rf doc
