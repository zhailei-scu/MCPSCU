#*********************************************************************************
#--- Description: 
#--- Author : Lei Zhai, Insti. of Nucle. Sci. and Tech., Sichuan University
#--- Email : zhaileiytp@163.com
#--- data: From 2017-09 to 2021-12
#--- License: MIT License (There are no limitation for anyone to use/modify/sale of this code, be happy to use this code)
#--- Please ref "Lei Zhai, Chaoqiong Ma, Jiechao Cui and Qing Hou, GPU-based acceleration of Monte Carlo simulations for migration-coalescence evolution of gas bubbles in materials
#---                        Modelling Simul. Mater. Sci. Eng. 2019. 27 055008,https://iopscience.iop.org/article/10.1088/1361-651X/ab1d14
#*********************************************************************************

#choice compiler and compiling flags
export comp       := pgfortran

ifeq ($(origin ConfigName), undefined) 
ConfigName := Release
endif

oflags_this := $(oflags)

#####################################
ifeq ($(operateSystem), CYGWIN)
  export LIBDIRS    := $(mcpscusor)$(Segment)LIB$(Segment)sor$(Segment)f
endif
ifeq ($(operateSystem), LINUX)
  export LIBDIRS    := $(mcpscusor)$(Segment)LIB$(Segment)sor$(Segment)f
endif
libnames          := RunningProfile CudaRandomC2F MiniUtilities RandGenerators

ifeq ($(operateSystem), CYGWIN)
  export TCCINTERDIRS := $(mcpscusor)$(Segment)TCCLIB$(Segment)sor
endif
ifeq ($(operateSystem), LINUX)
  export TCCINTERDIRS := $(mcpscusor)$(Segment)TCCLIB$(Segment)sor
endif
tccinterlibnames := TCCLIB

ifeq ($(operateSystem), CYGWIN)
  export MSMLIBDIRS := $(mcpscusor)$(Segment)MSMLIB$(Segment)sor
endif
ifeq ($(operateSystem), LINUX)
  export MSMLIBDIRS := $(mcpscusor)$(Segment)MSMLIB$(Segment)sor
endif
msmlibnames	  := MSMLIB

ifeq ($(operateSystem), CYGWIN)
  export MODELDATABASEDIRS  := $(mcpscusor)$(Segment)MODELDATABASE$(Segment)sor
endif
ifeq ($(operateSystem), LINUX)
  export MODELDATABASEDIRS  := $(mcpscusor)$(Segment)MODELDATABASE$(Segment)sor
endif
modeldatabasenames	  := MODELDATABASE

ifeq ($(operateSystem), CYGWIN)
  export MCLIBDIRS  := $(mcpscusor)$(Segment)MCLIB$(Segment)sor
endif
ifeq ($(operateSystem), LINUX)
  export MCLIBDIRS  := $(mcpscusor)$(Segment)MCLIB$(Segment)sor
endif
mclibnames	  := Common CommonGPU MigCoaleModel InletModel AppShell

ifeq ($(operateSystem), CYGWIN)
  export LIBDIRD    := $(mcworkspace)$(Segment)LIB$(Segment)$(ConfigName)
endif
ifeq ($(operateSystem), LINUX)
  export LIBDIRD    := $(mcworkspace)$(Segment)LIB$(Segment)$(ConfigName)
endif

all:
#the basic library
	$(MAKE) -f $(mcpscusor)$(Segment)LIB$(Segment)RunningProfile$(Segment)RunningProfile.mk
	$(MAKE) -f $(mcpscusor)$(Segment)LIB$(Segment)CudaRandomC2F$(Segment)CudaRandomC2F.mk
	$(MAKE) -f $(mcpscusor)$(Segment)LIB$(Segment)MiniUtilities$(Segment)MiniUtilities.mk
	$(MAKE) -f $(mcpscusor)$(Segment)LIB$(Segment)RandGenerators$(Segment)RandGenerators.mk
	
#tcc interface library
	$(MAKE) -f $(mcpscusor)$(Segment)TCCLIB$(Segment)TCCLIB.mk

#the MSMLIB library
	$(MAKE) -f $(mcpscusor)$(Segment)MSMLIB$(Segment)MSMLIB.mk

#the model data base library
	$(MAKE) -f $(mcpscusor)$(Segment)MODELDATABASE$(Segment)MODELDATABASE.mk

#the common mc library
	$(MAKE) -f $(mcpscusor)$(Segment)MCLIB$(Segment)Common$(Segment)Common.mk
#the commonGPU mc library
	$(MAKE) -f $(mcpscusor)$(Segment)MCLIB$(Segment)CommonGPU$(Segment)CommonGPU.mk
#the MigCoale mc library
	$(MAKE) -f $(mcpscusor)$(Segment)MCLIB$(Segment)MigCoaleModel$(Segment)MigCoaleModel.mk
#the inlet model library
	$(MAKE) -f $(mcpscusor)$(Segment)MCLIB$(Segment)InletModel$(Segment)InletModel.mk
#the BubbleGrowth mc library
	$(MAKE) -f $(mcpscusor)$(Segment)MCLIB$(Segment)AppShell$(Segment)AppShell.mk

############################################
.PHONY: clean
clean:
	$(MAKE) clean -f $(mcpscusor)$(Segment)LIB$(Segment)RunningProfile$(Segment)RunningProfile.mk
	$(MAKE) clean -f $(mcpscusor)$(Segment)LIB$(Segment)CudaRandomC2F$(Segment)CudaRandomC2F.mk
	$(MAKE) clean -f $(mcpscusor)$(Segment)LIB$(Segment)MiniUtilities$(Segment)MiniUtilities.mk
	$(MAKE) clean -f $(mcpscusor)$(Segment)LIB$(Segment)RandGenerators$(Segment)RandGenerators.mk
	$(MAKE) clean -f $(mcpscusor)$(Segment)TCCLIB$(Segment)TCCLIB.mk
	$(MAKE) clean -f $(mcpscusor)$(Segment)MSMLIB$(Segment)MSMLIB.mk
	$(MAKE) clean -f $(mcpscusor)$(Segment)MODELDATABASE$(Segment)MODELDATABASE.mk
	$(MAKE) clean -f $(mcpscusor)$(Segment)MCLIB$(Segment)Common$(Segment)Common.mk
	$(MAKE) clean -f $(mcpscusor)$(Segment)MCLIB$(Segment)CommonGPU$(Segment)CommonGPU.mk
	$(MAKE) clean -f $(mcpscusor)$(Segment)MCLIB$(Segment)MigCoaleModel$(Segment)MigCoaleModel.mk
	$(MAKE) clean -f $(mcpscusor)$(Segment)MCLIB$(Segment)InletModel$(Segment)InletModel.mk
	$(MAKE) clean -f $(mcpscusor)$(Segment)MCLIB$(Segment)AppShell$(Segment)AppShell.mk
