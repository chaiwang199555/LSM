# LSM Makefile — Fortran (gfortran, F2008, real64) + NetCDF
FC      = gfortran
NCFFLAGS = $(shell nf-config --fflags)
NCLIBS   = $(shell nf-config --flibs)
FFLAGS  = -O2 -g -fcheck=all -Wall -std=f2008 $(NCFFLAGS)
LDFLAGS = $(NCLIBS)

SRCDIR   = src
BUILDDIR = build
BINDIR   = bin
DATADIR  = data

MODULES = mod_kinds mod_constants mod_physics mod_types mod_radiation mod_turbulence \
          mod_conductance mod_acclimation mod_planthydro mod_soil_heat mod_soil_water \
          mod_photosyn mod_soil_carbon mod_cnp mod_conservation mod_ncio mod_soil_init \
          mod_snow mod_permafrost mod_radtran mod_canopy mod_gcm_coupling \
          mod_surface mod_forcing mod_io mod_driver

MOD_SRCS = $(addprefix $(SRCDIR)/,$(addsuffix .f90,$(MODULES)))
MOD_OBJS = $(patsubst $(SRCDIR)/%.f90,$(BUILDDIR)/%.o,$(MOD_SRCS))

LSM     = $(BINDIR)/lsm
GEN     = $(BINDIR)/gen_forcing
GEN_SOIL = $(BINDIR)/gen_soil_init
TXT_FORCING = $(DATADIR)/txt/sample_forcing.txt
NC_FORCING  = $(DATADIR)/nc/sample_forcing.nc
TXT_SOIL_INIT = $(DATADIR)/txt/sample_soil_init.txt
NC_SOIL_INIT  = $(DATADIR)/nc/sample_soil_init.nc

.PHONY: all clean run forcing forcing-nc soil-init soil-init-nc run-nc

all: $(LSM) $(GEN) $(GEN_SOIL)

$(LSM): $(MOD_OBJS) $(BUILDDIR)/main.o | $(BINDIR)
	$(FC) $(FFLAGS) -J$(BUILDDIR) -o $@ $(MOD_OBJS) $(BUILDDIR)/main.o $(LDFLAGS)

$(GEN): $(BUILDDIR)/mod_kinds.o $(BUILDDIR)/mod_constants.o $(BUILDDIR)/gen_forcing.o | $(BINDIR)
	$(FC) $(FFLAGS) -J$(BUILDDIR) -o $@ \
	    $(BUILDDIR)/mod_kinds.o $(BUILDDIR)/mod_constants.o $(BUILDDIR)/gen_forcing.o $(LDFLAGS)

$(GEN_SOIL): $(BUILDDIR)/mod_kinds.o $(BUILDDIR)/gen_soil_init.o | $(BINDIR)
	$(FC) $(FFLAGS) -J$(BUILDDIR) -o $@ \
	    $(BUILDDIR)/mod_kinds.o $(BUILDDIR)/gen_soil_init.o $(LDFLAGS)

$(BUILDDIR)/%.o: $(SRCDIR)/%.f90 | $(BUILDDIR)
	$(FC) $(FFLAGS) -J$(BUILDDIR) -c $< -o $@

$(BUILDDIR) $(BINDIR) $(DATADIR)/txt $(DATADIR)/nc results/txt results/nc:
	mkdir -p $@

forcing: $(GEN) | $(DATADIR)/txt
	./$(GEN) $(TXT_FORCING)

forcing-nc: forcing
	python3 scripts/txt_to_nc_forcing.py $(TXT_FORCING) -o $(NC_FORCING) --site test_site

soil-init: $(GEN_SOIL) | $(DATADIR)/txt
	./$(GEN_SOIL) $(TXT_SOIL_INIT)

soil-init-nc: soil-init
	python3 scripts/txt_to_nc_soil_init.py $(TXT_SOIL_INIT) -o $(NC_SOIL_INIT) --site test_site

run: all forcing-nc soil-init-nc
	./$(LSM)

run-nc: run

run-txt: all forcing
	@echo "Override namelist paths to data/txt and results/txt if needed"
	./$(LSM)

clean:
	rm -rf $(BUILDDIR) $(BINDIR)