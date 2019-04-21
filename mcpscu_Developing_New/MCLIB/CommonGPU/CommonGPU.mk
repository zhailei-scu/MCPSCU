#compiler
ifeq ($(origin comp), undefined) 
comp       := pgfortran
endif

oflags_this := $(oflags)

###########################################################
#sorce dir name
objname := CommonGPU

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
libname  := libMC_$(objname).$(LIB_EXT)

#######################################################          
nlist    :=  MCLIB_CONSTANTS_GPU		  \
	     MCLIB_Utilities_GPU		  \
	     MCLIB_TYPEDEF_DiffusorsDefine_GPU	  \
	     MCLIB_TYPEDEF_ReactionsDefine_GPU	  \
	     MCLIB_TYPEDEF_Geometry_GPU		  \
	     MCLIB_TYPEDEF_ClustersInfo_GPU       \
	     MCLIB_TYPEDEF_SimBoxArray_GPU	  \
	     MCLIB_TYPEDEF_RecordList_GPU	  \
	     MCLIB_GLOBAL_GPU               	  \
	     MCLIB_CAL_NEIGHBOR_LIST_GPU
             
objects  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).o)
modules  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).mod)
ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).f)
Ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F)
F90files := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F90)
#######################################################
$(libname) : $(objects)  
	ar -rcs $(libname) $(objects)
	mv $(libname) $(tgt)


$(tgt)$(Segment)MCLIB_CONSTANTS_GPU.o : $(sor)$(Segment)MCLIB_CONSTANTS_GPU.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@


$(tgt)$(Segment)MCLIB_Utilities_GPU.o : $(sor)$(Segment)MCLIB_Utilities_GPU.F90  \
			                $(tgt)$(Segment)MCLIB_CONSTANTS_GPU.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCLIB_TYPEDEF_DiffusorsDefine_GPU.o : $(sor)$(Segment)MCLIB_TYPEDEF_DiffusorsDefine_GPU.F90  \
					              $(tgt)$(Segment)MCLIB_Utilities_GPU.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCLIB_TYPEDEF_ReactionsDefine_GPU.o : $(sor)$(Segment)MCLIB_TYPEDEF_ReactionsDefine_GPU.F90  \
					              $(tgt)$(Segment)MCLIB_Utilities_GPU.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCLIB_TYPEDEF_Geometry_GPU.o : $(sor)$(Segment)MCLIB_TYPEDEF_Geometry_GPU.F90  \
			      	               $(tgt)$(Segment)MCLIB_Utilities_GPU.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@


$(tgt)$(Segment)MCLIB_TYPEDEF_ClustersInfo_GPU.o : $(sor)$(Segment)MCLIB_TYPEDEF_ClustersInfo_GPU.F90  \
					           $(tgt)$(Segment)MCLIB_Utilities_GPU.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@


$(tgt)$(Segment)MCLIB_TYPEDEF_SimBoxArray_GPU.o : $(sor)$(Segment)MCLIB_TYPEDEF_SimBoxArray_GPU.F90	\
					          $(tgt)$(Segment)MCLIB_TYPEDEF_ClustersInfo_GPU.o	\
					          $(tgt)$(Segment)MCLIB_TYPEDEF_Geometry_GPU.o		\
					          $(tgt)$(Segment)MCLIB_TYPEDEF_DiffusorsDefine_GPU.o   \
						  $(tgt)$(Segment)MCLIB_TYPEDEF_ReactionsDefine_GPU.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCLIB_TYPEDEF_RecordList_GPU.o : $(sor)$(Segment)MCLIB_TYPEDEF_RecordList_GPU.F90	\
				                 $(tgt)$(Segment)MCLIB_TYPEDEF_SimBoxArray_GPU.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@	


$(tgt)$(Segment)MCLIB_GLOBAL_GPU.o : $(sor)$(Segment)MCLIB_GLOBAL_GPU.F90		 \
			             $(tgt)$(Segment)MCLIB_TYPEDEF_ClustersInfo_GPU.o    \
			             $(tgt)$(Segment)MCLIB_TYPEDEF_SimBoxArray_GPU.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@


$(tgt)$(Segment)MCLIB_CAL_NEIGHBOR_LIST_GPU.o : $(sor)$(Segment)MCLIB_CAL_NEIGHBOR_LIST_GPU.F90  \
				                $(tgt)$(Segment)MCLIB_TYPEDEF_SimBoxArray_GPU.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@
######################################################################
clean:
	-rm $(objects) $(libname) $(modules) 
