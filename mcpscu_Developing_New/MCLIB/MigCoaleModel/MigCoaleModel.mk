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
objname := MigCoaleModel

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
libname  := libMC_$(objname).$(LIB_EXT)

#######################################################          
nlist := MigCoale_AddOnData_Host		\
	       MigCoale_AddOnData_Dev		\
	       MigCoale_TYPEDEF_SimRecord	\
	       MigCoale_TYPEDEF_StatisticInfo	\
	       MigCoale_Statistic_CPU		\
	       MigCoale_Statistic_GPU		\
	       MigCoale_TimeCtl			\
	       MigCoale_GlobalVars_Dev		\
	       MigCoale_Evolution_GPU
             
objects  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).o)
modules  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).mod)
ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).f)
Ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F)
F90files := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F90)
#######################################################
$(libname) : $(objects)  
	ar -rcs $(libname) $(objects)
	mv $(libname) $(tgt)

$(tgt)$(Segment)MigCoale_AddOnData_Host.o : $(sor)$(Segment)MigCoale_AddOnData_Host.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MigCoale_AddOnData_Dev.o : $(sor)$(Segment)MigCoale_AddOnData_Dev.F90  \
			      	                             $(tgt)$(Segment)MigCoale_AddOnData_Host.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MigCoale_TYPEDEF_SimRecord.o : $(sor)$(Segment)MigCoale_TYPEDEF_SimRecord.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MigCoale_TYPEDEF_StatisticInfo.o : $(sor)$(Segment)MigCoale_TYPEDEF_StatisticInfo.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MigCoale_Statistic_CPU.o : $(sor)$(Segment)MigCoale_Statistic_CPU.F90	\
				                                    $(tgt)$(Segment)MigCoale_TYPEDEF_StatisticInfo.o 
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MigCoale_Statistic_GPU.o : $(sor)$(Segment)MigCoale_Statistic_GPU.F90	\
				                                   $(tgt)$(Segment)MigCoale_TYPEDEF_StatisticInfo.o 
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MigCoale_TimeCtl.o : $(sor)$(Segment)MigCoale_TimeCtl.F90		  \
			                               $(tgt)$(Segment)MigCoale_TYPEDEF_StatisticInfo.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MigCoale_GlobalVars_Dev.o : $(sor)$(Segment)MigCoale_GlobalVars_Dev.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MigCoale_Evolution_GPU.o : $(sor)$(Segment)MigCoale_Evolution_GPU.F90  \
				                                   $(tgt)$(Segment)MigCoale_GlobalVars_Dev.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

######################################################################
clean:
	-rm $(objects) $(libname) $(modules) 
