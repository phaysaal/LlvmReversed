SRC_DIR:=src

TOOLS:=llvmrev

#TARGET:=native
TARGET:=byte

LLVM_VERSION := 3.8
CLANG := clang-$(LLVM_VERSION)

OCAMLBUILDFLAGS:=-classic-display -j 0 #-cflags -w,@a-4

export OCAMLPATH=/usr/lib/ocaml/llvm-$(LLVM_VERSION)

llvmrev_OCAMLBUILDFLAGS:=-use-ocamlfind -pkgs llvm,llvm.bitreader,ppx_sexp_conv,ppx_deriving.show,sexplib -lflags -ccopt,-L/usr/lib/llvm-$(LLVM_VERSION)/lib

OCAMLBUILD:=ocamlbuild

CLEAN_RULES:=$(patsubst %,%-clean,$(TOOLS))

.PHONY: $(TOOLS) # clean $(CLEAN_RULES) default run

default: $(TOOLS)

$(TOOLS):
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) $($@_OCAMLBUILDFLAGS) $($@_OCAMLBUILDFLAGS_$(TARGET)) -I $(SRC_DIR) -build-dir build/$@ $@.$(TARGET)
ifneq ($(shell test -f $(TOOLS) && echo -n yes),yes)
	ln -s build/$@/src/$@.$(TARGET) $@
endif


#run: $(TOOLS) hello.bc
#	CAML_LD_LIBRARY_PATH=/usr/lib/ocaml/llvm-$(LLVM_VERSION) ./build/tutorial02/src/tutorial02.byte hello.bc

#clean: $(CLEAN_RULES)
#	-rm -f hello.bc

#$(CLEAN_RULES):
#	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) -I $(SRC_DIR) -build-dir build/$(patsubst %-clean,%,$@) -clean $(patsubst %-clean,%,$@).$(TARGET)

#hello.bc: hello.c
#	$(CLANG) -c -emit-llvm $<
