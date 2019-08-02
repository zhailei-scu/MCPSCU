#choice compiler and compiling flags
export comp       := pgfortran

ifeq ($(origin ConfigName), undefined) 
ConfigName := Release
endif

oflags_this := $(oflags) $(otherlinkoptions)

# include directeries
LIBDIRD    := $(mcworkspace)$(Segment)LIB$(Segment)$(ConfigName)
incdir     := $(mcworkspace)$(Segment)LIB$(Segment)$(ConfigName)

##########################################################

#objective name
objname := $(OBJNAME)

#sorce name
MCANALYTOOLSOR := $(mcpscusor)$(Segment)MCLIB$(Segment)sor$(Segment)ANALYTOOLS
sormain := $(MCANALYTOOLSOR)$(Segment)$(OBJNAME).F90

#target directories

tgt  := $(mcworkspace)$(Segment)ANALYTOOLS$(Segment)$(ConfigName)


#objmain := $(tgt)$(Segment)$(OBJNAME).o

#intermediate files

#executable name
exename  := $(tgt)$(Segment)$(OBJNAME).exe
##########################################################
libs     := $(foreach n, $(libnames),    $(LIBDIRD)$(Segment)lib_$(n).$(LIB_EXT))
libs	 += $(foreach n, $(tccinterlibnames), $(LIBDIRD)$(Segment)lib_$(n).$(LIB_EXT))
libs	 += $(foreach n, $(tcclibnames), $(tccpath)$(Segment)lib$(n).$(LIB_EXT))
libs	 += $(foreach n, $(msmlibnames), $(LIBDIRD)$(Segment)lib_$(n).$(LIB_EXT))
libs	 += $(foreach n, $(modeldatabasenames), $(LIBDIRD)$(Segment)lib_$(n).$(LIB_EXT))
libs     += $(foreach n, $(mclibnames),  $(LIBDIRD)$(Segment)libMC_$(n).$(LIB_EXT))

liblist  := $(foreach n, $(mclibnames),  -L$(LIBDIRD)  -lMC_$(n))
liblist  += $(foreach n, $(modeldatabasenames), -L$(LIBDIRD) -l_$(n))
liblist  += $(foreach n, $(msmlibnames), -L$(LIBDIRD)  -l_$(n))
liblist  += $(foreach n, $(tccinterlibnames), -L$(LIBDIRD)  -l_$(n))
liblist  += $(foreach n, $(tcclibnames), -L$(tccpath) -l$(n))
liblist  += $(foreach n, $(libnames),    -L$(LIBDIRD)  -l_$(n))
#######################################################
$(exename) : $(objmain) $(libs)
	$(comp)  $(oflags_this) $(sormain) $(patsubst %, -I%, $(incdir)) $(obj) $(liblist) -o $(exename)

#$(exename) : $(objmain) $(libs)
#	$(comp)  $(oflags_this) $(obj) $(liblist) -o $(exename)
#	-rm $(objmain) $(obj) $(tgt)*.mod

#$(objmain) : $(sormain) $(obj)
#	$(comp) -c $(oflags_this) $(sormain) $(patsubst %, -I%, $(incdir)) -module $(tgt)  -o $(objmain)


clean:
	-rm  $(obj) $(objmain) $(tgt)$(Segment)$(OBJNAME).mod $(exename) 
