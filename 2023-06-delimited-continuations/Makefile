COMPILE        :=
CPUS           := $(shell racket -e '(display (processor-count))')
RACKET_VERSION := $(shell racket -e '(display (version))')

LIB_SRCS       := color.rkt pict.rkt slideshow.rkt unicode.rkt util.rkt
ASSET_SRCS     :=

ZO_PREFIX      := compiled/$(RACKET_VERSION)/compiled
LIB_ZOS        := $(LIB_SRCS:%.rkt=lib/$(ZO_PREFIX)/%_rkt.zo)

SLIDESHOW_OPTS := --pdf --not-paper --widescreen --zero-margins --no-stretch \
                  --no-resize --progress-text

SLIDES_DEPS    := main.rkt $(addprefix assets/,$(ASSET_SRCS))
ifdef COMPILE
	SLIDES_DEPS  += compile
else
	SLIDES_DEPS  += $(LIB_ZOS)
endif

all: slides.pdf slides-uncondensed.pdf

slides.pdf: $(SLIDES_DEPS)
	slideshow $(SLIDESHOW_OPTS) --condense -o slides.pdf main.rkt
slides-uncondensed.pdf: $(SLIDES_DEPS)
	slideshow $(SLIDESHOW_OPTS) -o slides-uncondensed.pdf main.rkt

lib/$(ZO_PREFIX)/slideshow_rkt.zo: lib/$(ZO_PREFIX)/pict_rkt.zo lib/$(ZO_PREFIX)/util_rkt.zo

%_rkt.zo: ../../../%.rkt
	raco make $<

compile: main.rkt $(addprefix lib/,$(LIB_SRCS))
	raco make -v -j '$(CPUS)' main.rkt

watch:
	@watch-exec --bell -p main.rkt $(addprefix -p lib/,$(LIB_SRCS)) -- \
		'$(MAKE)' -j '$(CPUS)' CPUS='$(CPUS)' RACKET_VERSION='$(RACKET_VERSION)'

.PHONY: all compile watch
