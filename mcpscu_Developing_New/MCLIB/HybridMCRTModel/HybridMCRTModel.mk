#compiler
ifeq ($(origin comp), undefined)
comp := pgfortran
endif

ifeq ($(origin ConfigName), undefined)
ConfigName := Release
endif

oflags_this := $(oflags)

##########################################################
#sorce dir name
objname := HybridMCRTModel

#sorce directories
ifeq ($(origin MCLIBDIRS), undefined)
MCLIBDIRS := $(mcpscusor)$(Segment)MCLIB$(Segment)sor
endif
sor := $(MCLIBDIRS)$(Segment)$(objname)

ifeq ($(origin LIBDIRD), undefined)
LIBDIRD := $(mcworkspace)$(Segment)LIB$(Segment)$(ConfigName)
endif
incdir := $(LIBDIRD)

#target directories
tgt := $(LIBDIRD)

#target lib name
libname := libMC_$(objname).$(LIB_EXT)

#######################################################
nlist	:=	HybridMCRT_TYPEDEF_EVCCluster	\
			HybridMCRT_TYPEDEF_Collections	\
			HybridMCRT_Evolution_GPU

objects		:= 	$(foreach n, $(nlist), $(tgt)$(Segment)$(n).o)
modules		:= 	$(foreach n, $(nlist), $(tgt)$(Segment)$(n).mod)
ffiles		:=	$(foreach n, $(nlist), $(tgt)$(Segment)$(n).f)
fFiles		:=	$(foreach n, $(nlist), $(tgt)$(Segment)$(n).F)
F90Files	:=	$(foreach n, $(nlist), $(tgt)$(Segment)$(n).F90)

#######################################################
$(libname) : $(objects)
	ar -rcs $(libname) $(objects)
	mv $(libname) $(tgt)

$(tgt)$(Segment)HybridMCRT_TYPEDEF_EVCCluster.o : $(sor)$(Segment)HybridMCRT_TYPEDEF_EVCCluster.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)HybridMCRT_TYPEDEF_Collections.o : $(sor)$(Segment)HybridMCRT_TYPEDEF_Collections.F90 \
												   $(tgt)$(Segment)HybridMCRT_TYPEDEF_EVCCluster.o 
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)HybridMCRT_Evolution_GPU.o : $(sor)$(Segment)HybridMCRT_Evolution_GPU.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@