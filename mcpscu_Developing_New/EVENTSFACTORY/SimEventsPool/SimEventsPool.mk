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
nlist    :=  Simulation_TYPEDEF_ReadedEventsModels		\
			 Simulation_TYPEDEF_DataPool					\
			 Simulation_TYPEDEF_EventsPool				\
			 Simulation_TYPEDEF_CollectionEventManager

             
objects  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).o)
modules  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).mod)
ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).f)
Ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F)
F90files := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F90)
#######################################################
$(libname) : $(objects)  
	ar -rcs $(libname) $(objects)
	mv $(libname) $(tgt)

$(tgt)$(Segment)Simulation_TYPEDEF_ReadedEventsModels.o : $(sor)$(Segment)Simulation_TYPEDEF_ReadedEventsModels.F90
	$(comp) -E $(oflags_this) -I$(incdir) $< > $(tgt)$(Segment)Simulation_TYPEDEF_ReadedEventsModels.f90
	sed -i '/^#\ \([0-9]\)/d' $(tgt)$(Segment)Simulation_TYPEDEF_ReadedEventsModels.f90
	sed -i 's/\[ENTER\]/\n/g' $(tgt)$(Segment)Simulation_TYPEDEF_ReadedEventsModels.f90
	$(comp) -c $(oflags_this) -cpp -I$(incdir) -module $(tgt) $(tgt)$(Segment)Simulation_TYPEDEF_ReadedEventsModels.f90 -o $@

$(tgt)$(Segment)Simulation_TYPEDEF_DataRelationPool.o : $(sor)$(Segment)Simulation_TYPEDEF_DataRelationPool.F90
	$(comp) -E $(oflags_this) -I$(incdir) $< > $(tgt)$(Segment)Simulation_TYPEDEF_DataRelationPool.f90
	sed -i '/^#\ \([0-9]\)/d' $(tgt)$(Segment)Simulation_TYPEDEF_DataRelationPool.f90
	sed -i 's/\[ENTER\]/\n/g' $(tgt)$(Segment)Simulation_TYPEDEF_DataRelationPool.f90
	$(comp) -c $(oflags_this) -cpp -I$(incdir) -module $(tgt) $(tgt)$(Segment)Simulation_TYPEDEF_DataRelationPool.f90 -o $@

$(tgt)$(Segment)Simulation_TYPEDEF_CollectionEventsRegisterCenter.o : $(sor)$(Segment)Simulation_TYPEDEF_CollectionEventsRegisterCenter.F90 \
												  $(tgt)$(Segment)Simulation_TYPEDEF_DataRelationPool.o
	$(comp) -E $(oflags_this) -I$(incdir) $< > $(tgt)$(Segment)Simulation_TYPEDEF_CollectionEventsRegisterCenter.f90
	sed -i '/^#\ \([0-9]\)/d' $(tgt)$(Segment)Simulation_TYPEDEF_CollectionEventsRegisterCenter.f90
	sed -i 's/\[ENTER\]/\n/g' $(tgt)$(Segment)Simulation_TYPEDEF_CollectionEventsRegisterCenter.f90
	$(comp) -c $(oflags_this) -cpp -I$(incdir) -module $(tgt) $(tgt)$(Segment)Simulation_TYPEDEF_CollectionEventsRegisterCenter.f90 -o $@

$(tgt)$(Segment)Simulation_TYPEDEF_CollectionEventManager.o : $(sor)$(Segment)Simulation_TYPEDEF_CollectionEventManager.F90 \
															$(tgt)$(Segment)Simulation_TYPEDEF_ReadedEventsModels.o \
															$(tgt)$(Segment)Simulation_TYPEDEF_CollectionEventsRegisterCenter.o \
															$(tgt)$(Segment)Simulation_TYPEDEF_DataRelationPool.o
	$(comp) -E $(oflags_this) -I$(incdir) $< > $(tgt)$(Segment)Simulation_TYPEDEF_CollectionEventManager.f90
	sed -i '/^#\ \([0-9]\)/d' $(tgt)$(Segment)Simulation_TYPEDEF_CollectionEventManager.f90
	sed -i 's/\[ENTER\]/\n/g' $(tgt)$(Segment)Simulation_TYPEDEF_CollectionEventManager.f90
	$(comp) -c $(oflags_this) -cpp -I$(incdir) -module $(tgt) $(tgt)$(Segment)Simulation_TYPEDEF_CollectionEventManager.f90 -o $@
######################################################################
clean:
	-rm $(objects) $(libname) $(modules) 
