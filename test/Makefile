##===- test/Makefile ---------------------------------------*- Makefile -*-===##

#
# Relative path to the top of the source tree.
#
LEVEL = ..

#
# Include rules.
#
include $(LEVEL)/Makefile.common

# Test in all immediate subdirectories if unset.
ifdef TESTSUITE
  TESTDIRS := $(TESTSUITE:%=$(PROJ_SRC_DIR)/%)
else
  TESTDIRS ?= $(PROJ_SRC_DIR)
endif

# 'lit' wants objdir paths, so it will pick up the lit.site.cfg.
TESTDIRS := $(TESTDIRS:$(PROJ_SRC_DIR)%=$(PROJ_OBJ_DIR)%)

# Allow EXTRA_TESTDIRS to provide additional test directories.
TESTDIRS += $(EXTRA_TESTDIRS)

ifndef TESTARGS
  ifdef VERBOSE
    TESTARGS = -v
  else
    TESTARGS = -s
  endif
endif

ifdef VG
  LIT_ARGS += '--vg'
endif

all:: lit.site.cfg
	@echo "--- Running llvm-acse tests for $(TARGET_TRIPLE) ---"
	@$(PYTHON) $(LLVM_SRC_ROOT)/utils/lit/lit.py \
	 $(LIT_ARGS) $(TESTARGS) $(TESTDIRS)

check-local:: all

FORCE:

lit.site.cfg: FORCE
	@echo "Making llvm-acse 'lit.site.cfg' file..."
	@$(ECHOPATH) s=@LLVM_SOURCE_DIR@=$(LLVM_SRC_ROOT)=g > lit.tmp
	@$(ECHOPATH) s=@LLVM_BINARY_DIR@=$(LLVM_OBJ_ROOT)=g >> lit.tmp
	@$(ECHOPATH) s=@LLVM_TOOLS_DIR@=$(LLVMToolDir)=g >> lit.tmp
	@$(ECHOPATH) s=@LLVM_LIBS_DIR@=$(LLVMLibDir)=g >> lit.tmp
	@$(ECHOPATH) s=@PROJ_SOURCE_DIR@=$(PROJ_SRC_DIR)/..=g >> lit.tmp
	@$(ECHOPATH) s=@PROJ_BINARY_DIR@=$(PROJ_OBJ_DIR)/..=g >> lit.tmp
	@$(ECHOPATH) s=@PROJ_TOOLS_DIR@=$(ToolDir)=g >> lit.tmp
	@$(ECHOPATH) s=@PROJ_LIBS_DIR@=$(LibDir)=g >> lit.tmp
	@$(ECHOPATH) s=@TARGET_TRIPLE@=$(TARGET_TRIPLE)=g >> lit.tmp
	@sed -f lit.tmp $(PROJ_SRC_DIR)/lit.site.cfg.in > $@
	@rm -f lit.tmp

.PHONY: all
