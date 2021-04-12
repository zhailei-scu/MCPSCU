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
incdir := $(LIBDDIRD)

#target directories
tgt := $(LIBDDIRD)

#target lib name
libname := libMC_$(objname).$(LIB_EXT)

