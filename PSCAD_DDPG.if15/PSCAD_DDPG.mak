
#------------------------------------------------------------------------------
# Project 'PSCAD_DDPG' make using the 'Intel(R) Visual Fortran Compiler 19.1.2.254 (64-bit)' compiler.
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# All project
#------------------------------------------------------------------------------

all: targets
	@echo !--Make: succeeded.



#------------------------------------------------------------------------------
# Directories, Platform, and Version
#------------------------------------------------------------------------------

Arch        = windows
EmtdcDir    = C:\PROGRA~2\PSCAD46\emtdc\if15
EmtdcInc    = $(EmtdcDir)\inc
EmtdcBin    = $(EmtdcDir)\$(Arch)
EmtdcMain   = $(EmtdcBin)\main.obj
EmtdcLib    = $(EmtdcBin)\emtdc.lib


#------------------------------------------------------------------------------
# Fortran Compiler
#------------------------------------------------------------------------------

FC_Name         = ifort.exe
FC_Suffix       = obj
FC_Args         = /nologo /c /free /real_size:64 /fpconstant /warn:declarations /iface:default /align:dcommons /fpe:0
FC_Debug        =  /O1
FC_Preprocess   = /fpp 
FC_Preproswitch = /Dfpp 
FC_Warn         = 
FC_Checks       = 
FC_Includes     = /include:"$(EmtdcInc)" /include:"$(EmtdcDir)" /include:"$(EmtdcBin)"
FC_Compile      = $(FC_Name) $(FC_Args) $(FC_Preprocess) $(FC_Preproswitch) $(FC_Includes) $(FC_Debug) $(FC_Warn) $(FC_Checks)

#------------------------------------------------------------------------------
# C Compiler
#------------------------------------------------------------------------------

CC_Name     = cl.exe
CC_Suffix   = obj
CC_Args     = /nologo /MT /W3 /EHsc /c
CC_Debug    =  /O2
CC_Includes = 
CC_Compile  = $(CC_Name) $(CC_Args) $(CC_Includes) $(CC_Debug)

#------------------------------------------------------------------------------
# Linker
#------------------------------------------------------------------------------

Link_Name   = link.exe
Link_Debug  = 
Link_Args   = /out:$@ /nologo /nodefaultlib:libc.lib /nodefaultlib:libcmtd.lib /subsystem:console
Link        = $(Link_Name) $(Link_Args) $(Link_Debug)

#------------------------------------------------------------------------------
# Build rules for generated files
#------------------------------------------------------------------------------


.f.$(FC_Suffix):
	@echo !--Compile: $<
	$(FC_Compile) $<



.c.$(CC_Suffix):
	@echo !--Compile: $<
	$(CC_Compile) $<



#------------------------------------------------------------------------------
# Build rules for file references
#------------------------------------------------------------------------------


user_source_1.$(FC_Suffix): D:\PSCAD_~1\FORTRA~2\PSCAD_~1\lib\MOD_RA~1.F90
	@echo !--Compile: "D:\PSCAD_RL_Test\Fortran_DDPG\PSCAD_DDPG\lib\mod_random.f90"
	copy "D:\PSCAD_RL_Test\Fortran_DDPG\PSCAD_DDPG\lib\mod_random.f90" .
	$(FC_Compile) "mod_random.f90"
	del "mod_random.f90"

user_source_2.$(FC_Suffix): D:\PSCAD_~1\FORTRA~2\PSCAD_~1\lib\MOD_PA~1.F90
	@echo !--Compile: "D:\PSCAD_RL_Test\Fortran_DDPG\PSCAD_DDPG\lib\mod_parallel.f90"
	copy "D:\PSCAD_RL_Test\Fortran_DDPG\PSCAD_DDPG\lib\mod_parallel.f90" .
	$(FC_Compile) "mod_parallel.f90"
	del "mod_parallel.f90"

user_source_3.$(FC_Suffix): D:\PSCAD_~1\FORTRA~2\PSCAD_~1\lib\MOD_OU~1.F90
	@echo !--Compile: "D:\PSCAD_RL_Test\Fortran_DDPG\PSCAD_DDPG\lib\mod_OUActionNoise.f90"
	copy "D:\PSCAD_RL_Test\Fortran_DDPG\PSCAD_DDPG\lib\mod_OUActionNoise.f90" .
	$(FC_Compile) "mod_OUActionNoise.f90"
	del "mod_OUActionNoise.f90"

user_source_4.$(FC_Suffix): D:\PSCAD_~1\FORTRA~2\PSCAD_~1\lib\MOD_AC~1.F90
	@echo !--Compile: "D:\PSCAD_RL_Test\Fortran_DDPG\PSCAD_DDPG\lib\mod_activation.f90"
	copy "D:\PSCAD_RL_Test\Fortran_DDPG\PSCAD_DDPG\lib\mod_activation.f90" .
	$(FC_Compile) "mod_activation.f90"
	del "mod_activation.f90"

user_source_5.$(FC_Suffix): D:\PSCAD_~1\FORTRA~2\PSCAD_~1\lib\MOD_LA~1.F90
	@echo !--Compile: "D:\PSCAD_RL_Test\Fortran_DDPG\PSCAD_DDPG\lib\mod_layer.f90"
	copy "D:\PSCAD_RL_Test\Fortran_DDPG\PSCAD_DDPG\lib\mod_layer.f90" .
	$(FC_Compile) "mod_layer.f90"
	del "mod_layer.f90"

user_source_6.$(FC_Suffix): D:\PSCAD_~1\FORTRA~2\PSCAD_~1\lib\MOD_NE~1.F90
	@echo !--Compile: "D:\PSCAD_RL_Test\Fortran_DDPG\PSCAD_DDPG\lib\mod_network.f90"
	copy "D:\PSCAD_RL_Test\Fortran_DDPG\PSCAD_DDPG\lib\mod_network.f90" .
	$(FC_Compile) "mod_network.f90"
	del "mod_network.f90"

user_source_7.$(FC_Suffix): D:\PSCAD_~1\FORTRA~2\PSCAD_~1\lib\MOD_BU~1.F90
	@echo !--Compile: "D:\PSCAD_RL_Test\Fortran_DDPG\PSCAD_DDPG\lib\mod_buffer.f90"
	copy "D:\PSCAD_RL_Test\Fortran_DDPG\PSCAD_DDPG\lib\mod_buffer.f90" .
	$(FC_Compile) "mod_buffer.f90"
	del "mod_buffer.f90"

user_source_8.$(FC_Suffix): D:\PSCAD_~1\FORTRA~2\PSCAD_~1\ddpg.f90
	@echo !--Compile: "D:\PSCAD_RL_Test\Fortran_DDPG\PSCAD_DDPG\ddpg.f90"
	copy "D:\PSCAD_RL_Test\Fortran_DDPG\PSCAD_DDPG\ddpg.f90" .
	$(FC_Compile) "ddpg.f90"
	del "ddpg.f90"

#------------------------------------------------------------------------------
# Dependencies
#------------------------------------------------------------------------------


FC_Objects = \
 Station.$(FC_Suffix) \
 Main.$(FC_Suffix) \
 user_source_1.$(FC_Suffix) \
 user_source_2.$(FC_Suffix) \
 user_source_3.$(FC_Suffix) \
 user_source_4.$(FC_Suffix) \
 user_source_5.$(FC_Suffix) \
 user_source_6.$(FC_Suffix) \
 user_source_7.$(FC_Suffix) \
 user_source_8.$(FC_Suffix)

FC_ObjectsLong = \
 "Station.$(FC_Suffix)" \
 "Main.$(FC_Suffix)" \
 "mod_random.$(FC_Suffix)" \
 "mod_parallel.$(FC_Suffix)" \
 "mod_OUActionNoise.$(FC_Suffix)" \
 "mod_activation.$(FC_Suffix)" \
 "mod_layer.$(FC_Suffix)" \
 "mod_network.$(FC_Suffix)" \
 "mod_buffer.$(FC_Suffix)" \
 "ddpg.$(FC_Suffix)"

CC_Objects =

CC_ObjectsLong =

UserLibs =

SysLibs  = ws2_32.lib

Binary   = PSCAD_DDPG.exe

$(Binary): $(FC_Objects) $(CC_Objects) $(UserLibs)
	@echo !--Link: $@
	$(Link) "$(EmtdcMain)" $(FC_ObjectsLong) $(CC_ObjectsLong) $(UserLibs) "$(EmtdcLib)" $(SysLibs)

targets: $(Binary)


clean:
	-del EMTDC_V*
	-del *.obj
	-del *.o
	-del *.exe
	@echo !--Make clean: succeeded.



