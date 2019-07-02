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
objname := MODELDATABASE

#sorce directories
ifeq ($(origin MODELDATABASEDIRS), undefined) 
MODELDATABASEDIRS := $(mcpscusor)$(Segment)MODELDATABASEDIRS$(Segment)sor
endif
sor  := $(MODELDATABASEDIRS)

ifeq ($(origin LIBDIRD), undefined)
LIBDIRD := $(mcworkspace)$(Segment)LIB$(Segment)$(ConfigName)
endif
incdir := $(LIBDIRD)

#target directories
tgt  := $(LIBDIRD)

#target lib name
libname  := lib_$(objname).$(LIB_EXT)

#######################################################          
nlist    :=  MODEL_TYPEDEF_ATOMSLIST             \
	     MODEL_ECR_CPU			 \
	     MODEL_ECR_GPU
             
objects  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).o)
modules  := $(foreach n, $(nlist), $(tgt)$(Segment)$(n).mod)
ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).f)
Ffiles   := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F)
F90files := $(foreach n, $(nlist), $(sor)$(Segment)$(n).F90)
#######################################################
$(libname) : $(objects)  
	ar -rcs $(libname) $(objects)
	mv $(libname) $(tgt)

$(tgt)$(Segment)MODEL_TYPEDEF_ATOMSLIST.o : $(sor)$(Segment)MODEL_TYPEDEF_ATOMSLIST.F90
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MODEL_ECR_CPU.o : $(sor)$(Segment)MODEL_ECR_CPU.F90  \
				  $(tgt)$(Segment)MODEL_TYPEDEF_ATOMSLIST.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

$(tgt)$(Segment)MODEL_ECR_GPU.o : $(sor)$(Segment)MODEL_ECR_GPU.F90  \
				  $(tgt)$(Segment)MODEL_TYPEDEF_ATOMSLIST.o \
				  $(tgt)$(Segment)MODEL_ECR_CPU.o
	$(comp) -c $(oflags_this) -I$(incdir) -module $(tgt) $< -o $@

######################################################################
clean:
	-rm $(objects) $(libname) $(modules) 
