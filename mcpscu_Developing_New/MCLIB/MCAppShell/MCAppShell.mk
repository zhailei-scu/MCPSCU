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
objname := MCAppShell

#sorce directories
ifeq ($(origin MCLIBDIRS), undefined) 
MCLIBDIRS := $(mcpscusor)$(Segment)MCLIB$(Segment)sor
endif
sor  := $(MCLIBDIRS)$(Segment)$(objname)

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
nlist :=   MC_CollectionEvent_MIGCOALE_CLUSTER_GPU \
		   MC_Method_MIGCOALE_CLUSTER_GPU  \
	       MC_MethodClass_Factory_GPU		   \
	       MC_SimBoxArray_AppShell_GPU
                       
objects  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).o)
modules  := $(foreach n, $(mlist ), $(tgt)$(Segment)$(n).mod)
ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).f)
Ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F)
F90files := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F90)
#######################################################
$(libname) : $(objects)
	ar -rcs $(libname) $(objects)
	mv $(libname) $(tgt)

$(tgt)$(Segment)MC_CollectionEvent_MIGCOALE_CLUSTER_GPU.o : $(sor)$(Segment)MC_CollectionEvent_MIGCOALE_CLUSTER_GPU.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MC_SimBoxArray_AppShell_GPU.o : $(sor)$(Segment)MC_SimBoxArray_AppShell_GPU.F90	\
				                                        $(tgt)$(Segment)MC_Method_MIGCOALE_CLUSTER_GPU.o     \
				                                        $(tgt)$(Segment)MC_MethodClass_Factory_GPU.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@


$(tgt)$(Segment)MC_MethodClass_Factory_GPU.o : $(sor)$(Segment)MC_MethodClass_Factory_GPU.F90     \
				                             		       $(tgt)$(Segment)MC_Method_MIGCOALE_CLUSTER_GPU.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MC_Method_MIGCOALE_CLUSTER_GPU.o : $(sor)$(Segment)MC_Method_MIGCOALE_CLUSTER_GPU.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

######################################################################
clean:
	-rm $(objects) $(libname) $(modules) 