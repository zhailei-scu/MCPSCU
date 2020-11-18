#compiler
ifeq ($(origin comp), undefined) 
comp       := pgfortran
endif

oflags_this := $(oflags)

###########################################################
#sorce dir name
objname := SimEventsPool

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
nlist    :=  Simulation_TYPEDEF_ObjectsEventModel	\
			 Simulation_TYPEDEF_EventsPool
             
objects  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).o)
modules  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).mod)
ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).f)
Ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F)
F90files := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F90)
#######################################################
$(libname) : $(objects)  
	ar -rcs $(libname) $(objects)
	mv $(libname) $(tgt)


$(tgt)$(Segment)Simulation_TYPEDEF_ObjectsEventModel.o : $(sor)$(Segment)Simulation_TYPEDEF_ObjectsEventModel.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)Simulation_TYPEDEF_EventsPool.o : $(sor)$(Segment)Simulation_TYPEDEF_EventsPool.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@
######################################################################
clean:
	-rm $(objects) $(libname) $(modules) 
