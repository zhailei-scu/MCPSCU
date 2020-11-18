#compiler
ifeq ($(origin comp), undefined) 
comp       := pgfortran
endif

oflags_this := $(oflags)

###########################################################
#sorce dir name
objname := HybridMCRTModel

#sorce directories
ifeq ($(origin HYBRIDLIBDIRS), undefined) 
HYBRIDLIBDIRS := $(mcpscusor)$(Segment)HYBRIDLIB$(Segment)sor
endif
sor  := $(HYBRIDLIBDIRS)$(Segment)$(objname)

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
nlist    :=  HybridMCRT_Evolution_GPU
             
objects  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).o)
modules  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).mod)
ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).f)
Ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F)
F90files := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F90)
#######################################################
$(libname) : $(objects)  
	ar -rcs $(libname) $(objects)
	mv $(libname) $(tgt)


$(tgt)$(Segment)HybridMCRT_Evolution_GPU.o : $(sor)$(Segment)HybridMCRT_Evolution_GPU.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@
######################################################################
clean:
	-rm $(objects) $(libname) $(modules) 
