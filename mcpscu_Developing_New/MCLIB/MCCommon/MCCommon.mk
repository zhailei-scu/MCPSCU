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
objname := MCCommon

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
libname  := lib_$(objname).$(LIB_EXT)

#######################################################          
nlist    :=  MCLIB_CONSTANTS               	 \
	     	 MCLIB_TYPEDEF_ACLUSTER        	 \
	    	 MCLIB_Utilities		 	 	 \
	     	 MCLIB_TYPEDEF_DiffusorsValue    \
             MCLIB_TYPEDEF_ReactionsValue    \
             MCLIB_TYPEDEF_DiffusorPropList  \
	    	 MCLIB_TYPEDEF_ReactionPropList  \
	    	 MCLIB_TYPEDEF_Geometry		 	 \
	    	 MCLIB_TYPEDEF_BASICRECORD		 \
	    	 MCLIB_TYPEDEF_NEIGHBOR_LIST   	 \
	    	 MCLIB_TYPEDEF_ClustersInfo_CPU  \
	    	 MCLIB_TYPEDEF_SimCtrlParam    	 \
	    	 MCLIB_TYPEDEF_SimBoxArray       \
	    	 MCLIB_GLOBAL			 		 \
	    	 MCLIB_CAL_NEIGHBOR_LIST
             
objects  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).o)
modules  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).mod)
ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).f)
Ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F)
F90files := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F90)
#######################################################
$(libname) : $(objects)  
	ar -rcs $(libname) $(objects)
	mv $(libname) $(tgt)


$(tgt)$(Segment)MCLIB_CONSTANTS.o : $(sor)$(Segment)MCLIB_CONSTANTS.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCLIB_TYPEDEF_ACLUSTER.o : $(sor)$(Segment)MCLIB_TYPEDEF_ACLUSTER.F90  \
				           $(tgt)$(Segment)MCLIB_CONSTANTS.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCLIB_Utilities.o : $(sor)$(Segment)MCLIB_Utilities.F90  \
			            $(tgt)$(Segment)MCLIB_TYPEDEF_ACLUSTER.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCLIB_TYPEDEF_DiffusorsValue.o : $(sor)$(Segment)MCLIB_TYPEDEF_DiffusorsValue.F90  \
                                                 $(tgt)$(Segment)MCLIB_CONSTANTS.o                 \
                                                 $(tgt)$(Segment)MCLIB_TYPEDEF_ACLUSTER.o	       \
				                 $(tgt)$(Segment)MCLIB_Utilities.o	
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCLIB_TYPEDEF_ReactionsValue.o : $(sor)$(Segment)MCLIB_TYPEDEF_ReactionsValue.F90  \
                                                 $(tgt)$(Segment)MCLIB_CONSTANTS.o                 \
                                                 $(tgt)$(Segment)MCLIB_TYPEDEF_ACLUSTER.o	   \
				                 $(tgt)$(Segment)MCLIB_Utilities.o	
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@


$(tgt)$(Segment)MCLIB_TYPEDEF_DiffusorPropList.o : $(sor)$(Segment)MCLIB_TYPEDEF_DiffusorPropList.F90  \
                                                   $(tgt)$(Segment)MCLIB_CONSTANTS.o                   \
                                                   $(tgt)$(Segment)MCLIB_Utilities.o                   \
                                                   $(tgt)$(Segment)MCLIB_TYPEDEF_DiffusorsValue.o 
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCLIB_TYPEDEF_ReactionPropList.o : $(sor)$(Segment)MCLIB_TYPEDEF_ReactionPropList.F90  \
                                                   $(tgt)$(Segment)MCLIB_CONSTANTS.o                   \
                                                   $(tgt)$(Segment)MCLIB_Utilities.o                   \
                                                   $(tgt)$(Segment)MCLIB_TYPEDEF_ReactionsValue.o 
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCLIB_TYPEDEF_BASICRECORD.o : $(sor)$(Segment)MCLIB_TYPEDEF_BASICRECORD.F90  \
				              $(tgt)$(Segment)MCLIB_Utilities.o		     \
					      $(tgt)$(Segment)MCLIB_CONSTANTS.o	
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCLIB_TYPEDEF_NEIGHBOR_LIST.o : $(sor)$(Segment)MCLIB_TYPEDEF_NEIGHBOR_LIST.F90  \
				                $(tgt)$(Segment)MCLIB_Utilities.o 
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCLIB_TYPEDEF_ClustersInfo_CPU.o : $(sor)$(Segment)MCLIB_TYPEDEF_ClustersInfo_CPU.F90  \
				      	           $(tgt)$(Segment)MCLIB_TYPEDEF_ACLUSTER.o		   \
					           $(tgt)$(Segment)MCLIB_TYPEDEF_NEIGHBOR_LIST.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCLIB_TYPEDEF_SimCtrlParam.o : $(sor)$(Segment)MCLIB_TYPEDEF_SimCtrlParam.F90  \
				               $(tgt)$(Segment)MCLIB_Utilities.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCLIB_TYPEDEF_Geometry.o : $(sor)$(Segment)MCLIB_TYPEDEF_Geometry.F90  \
				           $(tgt)$(Segment)MCLIB_CONSTANTS.o	       \
					   $(tgt)$(Segment)MCLIB_TYPEDEF_SimCtrlParam.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MCLIB_TYPEDEF_SimBoxArray.o : $(sor)$(Segment)MCLIB_TYPEDEF_SimBoxArray.F90     \
				              $(tgt)$(Segment)MCLIB_CONSTANTS.o		        \
				              $(tgt)$(Segment)MCLIB_TYPEDEF_ACLUSTER.o	        \
				              $(tgt)$(Segment)MCLIB_TYPEDEF_Geometry.o	        \
				              $(tgt)$(Segment)MCLIB_Utilities.o		        \
				              $(tgt)$(Segment)MCLIB_TYPEDEF_BASICRECORD.o	\
                                              $(tgt)$(Segment)MCLIB_TYPEDEF_DiffusorPropList.o  \
					      $(tgt)$(Segment)MCLIB_TYPEDEF_ReactionPropList.o  \
				              $(tgt)$(Segment)MCLIB_TYPEDEF_ClustersInfo_CPU.o  \
				              $(tgt)$(Segment)MCLIB_TYPEDEF_SimCtrlParam.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@


$(tgt)$(Segment)MCLIB_GLOBAL.o : $(sor)$(Segment)MCLIB_GLOBAL.F90              \
		                 $(tgt)$(Segment)MCLIB_CONSTANTS.o		   \
		                 $(tgt)$(Segment)MCLIB_Utilities.o		   \
		                 $(tgt)$(Segment)MCLIB_TYPEDEF_SimCtrlParam.o  \
		                 $(tgt)$(Segment)MCLIB_TYPEDEF_SimBoxArray.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@


$(tgt)$(Segment)MCLIB_CAL_NEIGHBOR_LIST.o : $(sor)$(Segment)MCLIB_CAL_NEIGHBOR_LIST.F90   \
		       		            $(tgt)$(Segment)MCLIB_CONSTANTS.o	      \
		       		            $(tgt)$(Segment)MCLIB_Utilities.o	      \
				            $(tgt)$(Segment)MCLIB_TYPEDEF_NEIGHBOR_LIST.o \
		       		            $(tgt)$(Segment)MCLIB_TYPEDEF_SimCtrlParam.o  \
		       		            $(tgt)$(Segment)MCLIB_TYPEDEF_SimBoxArray.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@


######################################################################
clean:
	-rm $(objects) $(libname) $(modules) 
