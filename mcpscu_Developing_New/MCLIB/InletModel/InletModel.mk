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
objname := InletModel

#sorce directories
ifeq ($(origin MCLIBDIRS), undefined) 
MCLIBDIRS := $(mcpscusor)$(Segment)MCLIB$(Segment)sor
endif
sor  := $(MCLIBDIRS)$(Segment)$(objname)

ifeq ($(origin LIBDIRD), undefined)
LIBDIRD := $(mcworkspace)$(Segment)LIB$(Segment)$(ConfigName)
endif
incdir := $(LIBDIRD)

#target directories
tgt  := $(LIBDIRD)

#target lib name
libname  := libMC_$(objname).$(LIB_EXT)

#######################################################          
nlist    :=  Inlet_TYPEDEF_ImplantSection    	\
	     Inlet_TYPEDEF_ImplantList		\
             MC_TYPEDEF_ImplantationSection
             
objects  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).o)
modules  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).mod)
ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).f)
Ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F)
F90files := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F90)
#######################################################
$(libname) : $(objects)  
	ar -rcs $(libname) $(objects)
	mv $(libname) $(tgt)

$(tgt)$(Segment)Inlet_TYPEDEF_ImplantSection.o : $(sor)$(Segment)Inlet_TYPEDEF_ImplantSection.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MC_TYPEDEF_ImplantationSection.o : $(sor)$(Segment)MC_TYPEDEF_ImplantationSection.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)Inlet_TYPEDEF_ImplantList.o : $(sor)$(Segment)Inlet_TYPEDEF_ImplantList.F90 \
						   $(tgt)$(Segment)Inlet_TYPEDEF_ImplantSection.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

######################################################################
clean:
	-rm $(objects) $(libname) $(modules) 
