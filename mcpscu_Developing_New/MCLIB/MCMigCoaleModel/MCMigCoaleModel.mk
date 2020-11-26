#compiler
ifeq ($(origin comp), undefined) 
comp       := pgfortran
endif

ifeq ($(origin ConfigName), undefined) 
ConfigName := Release
endif

oflags_this := $(oflags)

###########################################################
#sorce dir name
objname := MCMigCoaleModel

#sorce directories
ifeq ($(origin MCLIBDIRS), undefined) 
MCLIBDIRS := $(mcpscusor)$(Segment)MCLIB$(Segment)sor
endif
sor := $(MCLIBDIRS)$(Segment)$(objname)

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
nlist := MCMigCoale_AddOnData_Host			\
	     MCMigCoale_AddOnData_Dev			\
	     MCMigCoale_TYPEDEF_SimRecord		\
	     MCMigCoale_TYPEDEF_StatisticInfo	\
	     MCMigCoale_Statistic_CPU			\
	     MCMigCoale_Statistic_GPU			\
	     MCMigCoale_TimeCtl					\
	     MCMigCoale_GlobalVars_Dev			\
	     MCMigCoale_Evolution_GPU
             
objects  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).o)
modules  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).mod)
ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).f)
Ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F)
F90files := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F90)
#######################################################
$(libname) : $(objects)  
	ar -rcs $(libname) $(objects)
	mv $(libname) $(tgt)

$(tgt)$(Segment)MCMigCoale_AddOnData_Host.o : $(sor)$(Segment)MCMigCoale_AddOnData_Host.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCMigCoale_AddOnData_Dev.o : $(sor)$(Segment)MCMigCoale_AddOnData_Dev.F90  \
			      	                             $(tgt)$(Segment)MCMigCoale_AddOnData_Host.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCMigCoale_TYPEDEF_SimRecord.o : $(sor)$(Segment)MCMigCoale_TYPEDEF_SimRecord.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCMigCoale_TYPEDEF_StatisticInfo.o : $(sor)$(Segment)MCMigCoale_TYPEDEF_StatisticInfo.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCMigCoale_Statistic_CPU.o : $(sor)$(Segment)MCMigCoale_Statistic_CPU.F90	\
				                                    $(tgt)$(Segment)MCMigCoale_TYPEDEF_StatisticInfo.o 
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCMigCoale_Statistic_GPU.o : $(sor)$(Segment)MCMigCoale_Statistic_GPU.F90	\
				                                   $(tgt)$(Segment)MCMigCoale_TYPEDEF_StatisticInfo.o 
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCMigCoale_TimeCtl.o : $(sor)$(Segment)MCMigCoale_TimeCtl.F90		  \
			                               $(tgt)$(Segment)MCMigCoale_TYPEDEF_StatisticInfo.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCMigCoale_GlobalVars_Dev.o : $(sor)$(Segment)MCMigCoale_GlobalVars_Dev.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCMigCoale_Evolution_GPU.o : $(sor)$(Segment)MCMigCoale_Evolution_GPU.F90  \
				                                   $(tgt)$(Segment)MCMigCoale_GlobalVars_Dev.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

######################################################################
clean:
	-rm $(objects) $(libname) $(modules) 
