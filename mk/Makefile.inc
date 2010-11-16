-include ../Makefile.inc

#
# Setup useful defaults
#
FPC?= fpc
FPCFLAGS+= -Mdelphi -vb
OBJDIR=	obj
EXTEXE=

FPCFLAGS+= -Fu../synapse -Fu../mbulib
FPCFLAGS+= -FE$(OBJDIR)

ifeq ($(OS),Windows_NT)
OS_TARGET= win32
else
OS_NAME:= $(shell uname)
ifeq ($(OS_NAME),FreeBSD)
OS_TARGET= freebsd
else
$(error Unknown OS, please define)
endif
endif # Windows_NT

ifeq ($(OS_TARGET),win32)
EXTEXE= .exe
endif

ifeq ($(OS_TARGET),freebsd)
FPCFLAGS+= -dFREEBSD -dDARWIN -dUseCThreads -Xm
endif

ifdef LPR
$(LPR)$(EXTEXE): $(LPR).lpr
	$(FPC) $(FPCFLAGS) $<

all: $(LPR)$(EXTEXE)

endif

clean:
	-rm $(OBJDIR)/*
