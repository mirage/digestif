ifneq (, $(shell command -v opam || which opam))
PKG_CONFIG_PATH := $(shell opam config var prefix)/lib/pkgconfig
endif

EXISTS := $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --exists ocaml-freestanding; echo $$?)

.PHONY: all clean
all: libdigestif_freestanding_stubs.a

ifeq ($(EXISTS), 1)
libdigestif_freestanding_stubs.a:
	touch $@
else
CC ?= cc
FREESTANDING_CFLAGS := $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --cflags ocaml-freestanding)
CFLAGS := -I../src-c/native -O3 $(FREESTANDING_CFLAGS)

OBJS= blake2b.o blake2s.o md5.o ripemd160.o sha1.o sha256.o sha512.o sha3.o whirlpool.o misc.o stubs.o

libdigestif_freestanding_stubs.a: $(OBJS)
	$(AR) r $@ $^
endif

clean:
	$(RM) $(OBJS) libdigestif_freestanding_stubs.a
