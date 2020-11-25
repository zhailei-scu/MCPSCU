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
objname := Common

#sorce directories
ifeq ($(origin COMMONLIBDIRS), undefined) 
COMMONLIBDIRS := $(mcpscusor)$(Segment)MCLIB$(Segment)sor
endif
sor  := $(COMMONLIBDIRS)$(Segment)$(objname)

ifeq ($(origin LIBDIRD), undefined)
LIBDIRD := $(mcworkspace)$(Segment)LIB$(Segment)$(ConfigName)
endif
incdir := $(LIBDIRD)

#target directories
tgt  := $(LIBDIRD)

#target lib name
libname  := lib_$(objname).$(LIB_EXT)

#######################################################          
nlist    :=  CommonLIB_CONSTANTS               	 \
			 CommonLIB_TYPEDEF_ObjectsCollection \
			 CommonLIB_TYPEDEF_EventModel		 \
			 CommonLIB_Utilities
             
objects  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).o)
modules  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).mod)
ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).f)
Ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F)
F90files := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F90)
#######################################################
$(libname) : $(objects)  
	ar -rcs $(libname) $(objects)
	mv $(libname) $(tgt)

$(tgt)$(Segment)CommonLIB_CONSTANTS.o : $(sor)$(Segment)CommonLIB_CONSTANTS.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@


$(tgt)$(Segment)CommonLIB_TYPEDEF_ObjectsCollection.o : $(sor)$(Segment)CommonLIB_TYPEDEF_ObjectsCollection.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)CommonLIB_TYPEDEF_EventModel.o : $(sor)$(Segment)CommonLIB_TYPEDEF_EventModel.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)CommonLIB_Utilities.o : $(sor)$(Segment)CommonLIB_Utilities.F90  \
			            $(tgt)$(Segment)CommonLIB_CONSTANTS.o
	$(comp) -E $(oflags_this) -I$(incdir) $< > $(tgt)$(Segment)CommonLIB_Utilities.f90
	sed -i '/^#\ \([0-9]\)/d' $(tgt)$(Segment)CommonLIB_Utilities.f90
	sed -i 's/\[ENTER\]/\n/g' $(tgt)$(Segment)CommonLIB_Utilities.f90
	$(comp) -c $(oflags_this) -cpp -I$(incdir) -module $(tgt) $(tgt)$(Segment)CommonLIB_Utilities.f90 -o $@
	#rm $(tgt)$(Segment)CommonLIB_Utilities.f90

######################################################################
clean:
	-rm $(objects) $(libname) $(modules) 
