#compiler
ifeq ($(origin comp), undefined) 
comp       := pgfortran
endif

ifeq ($(origin ConfigName), undefined) 
ConfigName := Release
endif

oflags_this := $(oflags)

##########################################################
#sorce dir name
objname := AppShell

#sorce directories
ifeq ($(origin EVENTSFACTORYDIRS), undefined) 
EVENTSFACTORYDIRS := $(mcpscusor)$(Segment)EVENTSFACTORY$(Segment)sor
endif
sor  := $(EVENTSFACTORYDIRS)$(Segment)$(objname)

#include dir
ifeq ($(origin LIBDIRD), undefined)
LIBDIRD := $(mcworkspace)$(Segment)LIB$(Segment)$(ConfigName)
endif
incdir := $(LIBDIRD)

#target directories
tgt  := $(LIBDIRD)

#target lib name
libname  := lib_$(objname).$(LIB_EXT)

#######################################################
nlist :=   Simulation_AppShell
                       
objects  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).o)
modules  := $(foreach n, $(mlist ), $(tgt)$(Segment)$(n).mod)
ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).f)
Ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F)
F90files := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F90)
#######################################################
$(libname) : $(objects)
	ar -rcs $(libname) $(objects)
	mv $(libname) $(tgt)


$(tgt)$(Segment)Simulation_AppShell.o : $(sor)$(Segment)Simulation_AppShell.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@
######################################################################
clean:
	-rm $(objects) $(libname) $(modules) 