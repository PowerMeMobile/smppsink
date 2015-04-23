NAME=smppsink
REBAR=./rebar
OTP_PLT=~/.otp.plt
PRJ_PLT=$(NAME).plt

HAPROXY=./haproxy
HAPROXY_SRC_DIR=haproxy-1.5.11
HAPROXY_SRC_TAR_HREF=http://www.haproxy.org/download/1.5/src/haproxy-1.5.11.tar.gz
HAPROXY_SRC_TAR=haproxy-1.5.11.tar.gz
HAPROXY_PEM=haproxy.pem
HAPROXY_PID=haproxy.pid

.PHONY: test

all: generate

generate: compile xref
	@rm -rf ./rel/$(NAME)
	@$(REBAR) generate

compile: get-deps
	@$(REBAR) compile

xref: compile
	@$(REBAR) xref skip_deps=true

get-deps:
	@$(REBAR) get-deps

update-deps:
	@$(REBAR) update-deps

clean:
	@$(REBAR) clean

test: generate $(HAPROXY) xref
	# Run unit tests
	@$(REBAR) eunit skip_deps=true

	# Start haproxy
	./haproxy -f test/test.haproxy.cfg

	# Start tests with proxy
	export PORT=2773 && ./test/test.sh

	# Stop haproxy
	kill -9 $$(cat $(HAPROXY_PID))

	# Start tests without proxy
	./test/test.sh

$(HAPROXY): $(HAPROXY_SRC_DIR) $(HAPROXY_PEM)
	make -C $(HAPROXY_SRC_DIR) TARGET=generic USE_PCRE=1 USE_OPENSSL=1 USE_ZLIB=1 USE_CRYPT_H=1
	cp $(HAPROXY_SRC_DIR)/$@ .

$(HAPROXY_SRC_DIR): $(HAPROXY_SRC_TAR)
	tar xzf $(HAPROXY_SRC_TAR)

$(HAPROXY_SRC_TAR):
	wget $(HAPROXY_SRC_TAR_HREF)

$(HAPROXY_PEM):
	openssl req -new -x509 -nodes -out haproxy.crt -keyout haproxy.key -batch
	cat haproxy.key > $@
	cat haproxy.crt >> $@

dialyze: $(OTP_PLT) compile $(PRJ_PLT)
	@dialyzer --plt $(PRJ_PLT) -r ./subapps/*/ebin

$(OTP_PLT):
	@dialyzer --build_plt --output_plt $(OTP_PLT) --apps erts \
		kernel stdlib crypto mnesia sasl common_test eunit ssl \
		asn1 compiler syntax_tools inets

$(PRJ_PLT):
	@dialyzer --add_to_plt --plt $(OTP_PLT) --output_plt $(PRJ_PLT) \
	-r ./deps/*/ebin ./subapps/*/ebin

console:
	@./rel/$(NAME)/bin/$(NAME) console

develop:
	@./rel/$(NAME)/bin/$(NAME) develop

tags:
	@find . -name "*.[e,h]rl" -print | etags -
