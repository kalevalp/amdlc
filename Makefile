OCAMLMAKEFILE = OCamlMakefile

SOURCES = src/amdl_parser.mly \
	src/ast.mli src/ast.ml \
	src/utils.mli src/utils.ml \
	src/amdl_tokens.mll \
	src/project.ml \
	src/dlog_types.mli \
	src/dlog.mli src/dlog.ml \
	src/dlog_init.mli src/dlog_init.ml \
	src/dlog_send.mli src/dlog_send.ml \
	src/dlog_update.mli src/dlog_update.ml \
	src/amdlc.ml 

RESULT  = build/amdlc
LIBS = unix str 
PACKS = yojson

include $(OCAMLMAKEFILE)

