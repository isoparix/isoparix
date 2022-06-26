#
# Test Comment added by peterm, 11:29 21/07/2006
#
include flags.L580 

.SUFFIXES:
.SUFFIXES: .f .f90 .F .F90 .c .h .o

all: finance allxx allisopar alltest allda allsurf allmusic x11solo allslides allsudoku allmisc # networks andrew   

.F90.o:
	$(FC) $(FFLAGS) $(PARFLAGS) -c $<

.f90.o:
	$(FC) $(FFLAGS) $(PARFLAGS) -c $<

.f.o:
	$(FC) $(FFLAGS) $(PARFLAGS) -c $<

.c.o:
	$(CC) $(CFLAGS) -c $<

finance: $(FINANCE_JSW)/finanal.exe $(FINANCE_JSW)/fingather.exe $(FINANCE_JSW)/finsum.exe \
	$(FINANCE_JSW)/finpie.exe $(FINANCE_JSW)/finextract.exe $(FINANCE_JSW)/finsd.exe

# $(TOOLS)/FFT.exe $(TOOLS)/ip_fft.exe $(TOOLS)/ip_fft_testbed.exe \
	#
allmisc: $(TOOLS)/architrave_curve.exe $(TOOLS)/jpegtime.exe $(TOOLS)/piston.exe $(TOOLS)/piston_search.exe \
	 $(TOOLS)/section_area.exe $(TOOLS)/cake.exe $(TOOLS)/ip_master.exe $(TOOLS)/websize.exe \
	 $(TOOLS)/blood_pressure.exe $(TOOLS)/solar_master.exe $(TOOLS)/ip_slopes.exe \
	 $(TOOLS)/decimal_time.exe $(TOOLS)/solar_master.exe $(TOOLS)/rot_geom.exe \
	 $(TOOLS)/pythag.exe $(TOOLS)/test_graph.exe \
	 $(TOOLS)/join_lines.exe $(TOOLS)/survey_lines.exe $(TOOLS)/psa_anal.exe $(TOOLS)/img_mult.exe \
	 $(TOOLS)/dofor.exe $(TOOLS)/dup_txt.exe $(TOOLS)/resolution.exe $(TOOLS)/pivot_graph.exe \
	 $(TOOLS)/inch_swg.exe $(TOOLS)/parallel_track.exe $(TOOLS)/elevation.exe \
	 $(TOOLS)/declination.exe $(TOOLS)/hour_angle_protractor.exe $(TOOLS)/txnwin.exe \
	 $(TOOLS)/test_card.exe $(TOOLS)/tmp_calc.exe $(TOOLS)/elder_shifter.exe $(TOOLS)/adventdates.exe \
	 $(TOOLS)/latitude_slope.exe $(TOOLS)/multiserv.exe $(TOOLS)/sieve.exe $(TOOLS)/test_random.exe \
	 $(TOOLS)/gpx_elev.exe $(TOOLS)/lottery.exe

#andrew: $(ANDREW)/andrew_tt.exe $(ANDREW)/andrew_alldist.exe \
	$(ANDREW)/andrew_earth_dist.exe $(ANDREW)/andrew_graph.exe \
	$(ANDREW)/andrew_dfd.exe $(ANDREW)/andrew_refine.exe       \
	$(ANDREW)/andrew_venn.exe $(ANDREW)/showpoly.exe

allsudoku: $(SUDOKU)/sudoku.exe

allslides: $(SLIDES)/border_f.exe $(SLIDES)/slide_crop.exe $(SLIDES)/slide_rename.exe \
	   $(SLIDES)/read_test.exe $(SLIDES)/mask_test.exe $(SLIDES)/ip_tiler.exe $(SLIDES)/ip_greyscale.exe

allmusic: $(STEM)/easy_midi.exe $(STEM)/read_midi.exe $(STEM)/test_make_midi_time.exe $(STEM)/read_midiplay_txt.exe

allsurf: $(SURF)/surf.exe $(SURF)/shine.exe $(SURF)/shinebmp.exe $(SURF)/sf_explore.exe \
	$(SURF)/mixer.exe $(SURF)/translator.exe $(SURF)/swapper.exe $(SURF)/morph.exe \
	$(SURF)/panorama.exe $(SURF)/transform.exe

#x11solo:  $(XXSTATIX)/example.exe
x11solo: $(XXSTATIX)/svn_calib.exe $(XXSTATIX)/example.exe $(TOOLS)/cantilever.exe

allxx: \
$(XXSTATIX)/iostat.exe   $(XXSTATIX)/vmstat.exe    $(XXSTATIX)/netstat.exe $(XXSTATIX)/data_generator.exe \
$(XXSTATIX)/iostatin.exe $(XXSTATIX)/vmstatin.exe  $(XXSTATIX)/netstatin.exe \
$(XXSTATIX)/colmaps.exe  $(XXSTATIX)/iostatmon.exe $(XXSTATIX)/iowc.exe   $(XXSTATIX)/jiggler.exe \
$(XXSTATIX)/test_butt.exe  $(XXSTATIX)/wintext.exe $(XXSTATIX)/equalcol_test.exe $(XXSTATIX)/franco.exe \
$(XXSTATIX)/topmon.exe   $(XXSTATIX)/dcv_butt.exe  $(XXSTATIX)/recsum.exe $(XXSTATIX)/summan.exe $(XXSTATIX)/dcv_state.exe

allda: \
$(DATEST)/timanal.exe    $(DATEST)/datesdat.exe    $(DATEST)/spmulti.exe   $(DATEST)/fosyan.exe \
$(DATEST)/txtanl.exe     $(DATEST)/darupdate.exe   $(DATEST)/darepr.exe    $(DATEST)/fosy01.exe  \
$(DATEST)/taperepw.exe   $(DATEST)/tapereps.exe    $(DATEST)/tapetest.exe  $(DATEST)/datest.exe \
$(DATEST)/ddreps.exe     $(DATEST)/ddrepws.exe     $(DATEST)/ddrepw.exe    $(DATEST)/dareps.exe \
$(DATEST)/darepws.exe    $(DATEST)/darepw.exe      $(DATEST)/killmulti.exe $(DATEST)/inverter.exe

allisopar: \
$(ISOPARIX)/isoparix.exe $(ISOPARIX)/test_mpi.exe $(ISOPARIX)/fpi.exe


alltest:\
$(ISOPARIX)/readbmp.exe       $(ISOPARIX)/font_read.exe  \
$(ISOPARIX)/label_bmp.exe     $(ISOPARIX)/test_alloc.exe \
$(ISOPARIX)/compress_test.exe $(ISOPARIX)/array3D.exe    \
$(ISOPARIX)/showcube.exe      $(ISOPARIX)/showcubes.exe  \
$(ISOPARIX)/test_side.exe     $(ISOPARIX)/font_file.exe  \
$(ISOPARIX)/test_defmap.exe   $(ISOPARIX)/offprep.exe    \
$(ISOPARIX)/msetview.exe                                 \
$(ISOPARIX)/fractiler.exe     $(ISOPARIX)/testxod.exe    \
$(TOOLS)/barclays_csv.exe     $(TOOLS)/barclays_pdf.exe $(TOOLS)/barclays.exe

#networks: $(NETWORKS)/links.exe

isox11.o: keydefs.h


DACOMM_DEPS= darepr.f dareps.f darepw.f darepws.f darread.f darupdate.f \
             darupdt.f ddreps.f ddrepw.f ddrepws.f taperead.f tapereps.f \
             taperepw.f tapetest.f tapewrite.f

$(DACOMM_DEPS): dacomm.o

ALLCOMMS_DEPS = add_toc_item.o addutil.o amedian.o arrows.o bar.o \
                button_action.o button_setup.o buttons.o \
                close_lists.o colstat.o column_set.o convrec.o \
                draw_butt.o draw_ruler.o host_input.o summan.exe \
                io_sizer.o iostatin.o iostatmon.o iostat.o \
                lsquare.o make_button.o mem_alloc.o memsmooth.o \
                netstat.o nzxy.o nzy.o open_files.o paint_all.o \
                paint_hosts.o paint_map.o paint_prep.o plotutil.o \
                read_io_rec.o recsum.o read_top_rec.o sdev.o select_view.o \
                start_win.o summer.o test_butt.o dcv_butt.o text_line.o \
                title_rates.o topmon.o toprec.o vmstat.o winset.o \
                win_type.o locate_button.o

$(ALLCOMMS_DEPS): allcomms.o

BMP_COMMS_DEPS = array2bmp.o array3D.o compressor.o compress_test.o \
                 defmap.o isoartst.o makemap.o paint_all.o paint_hosts.o \
                 jiggler.exe showcube.o showcubes.o label_bmp.o\
                 samewrite.o diffwrite.o adintel2.o adintel4.o shinebmp.o \
		 read_cmap_params.o

$(BMP_COMMS_DEPS): bmp_comms.o

ISOCOMM_DEPS = array3D.o artist_wait.o axes.o bisbox.o bound.o box.o column.o \
               cubeadd.o cubecheck.o cubeinit.o cubelist.o cubespec.o \
               cubesplit.o datout.o deconx.o drawcube.o \
               drawface.o edge.o equalcol.o eyepoint.o filler.o \
               flooralone.o floorshow.o getmp.o grafout.o hidrem.o \
               isoartst.o isomast.o isoparix.o isoslave.o iterdist.o \
               linecheck.o liner.o linesegs.o linout.o looper.o lts.o \
               makeline.o minit.o mpiwindup.o outline.o param_list.o \
               photomap.o photospot.o picker.o prepact.o \
               rectput.o scene.o screenscale.o showcube.o showcubes.o \
               statout.o summline.o tanxy.o test_mpi.o title.o \
               tripleset.o validate.o vcheck.o visibility.o vrecord.o \
               equalcol_test.o mpirelease.o test_defmap.o read_cmap_params.o

$(ISOCOMM_DEPS): isocomm.o

EASY_MIDI_DEPS = easy_midi.o test_make_midi_time.o make_midi_time.o \
		 easy_midi_tempo.o easy_midi_xvolume.o easy_midi_timesig.o \
		 easy_midi_key.o easy_midi_numerics.o easy_midi_tvolume.o

$(EASY_MIDI_DEPS): easy_midi_comms.o

PARCOMM_DEPS = artist_wait.o bisbox.o box.o getmp.o grafout.o isoartst.o \
               isomast.o isoparix.o isoslave.o lts.o mpiwindup.o mpirelease.o

$(PARCOMM_DEPS): parcomm.o

DCV_COMMS_DEPS = dcv_activate.o dcv_butt_action.o button_setup.o \
                 assess_dcv_state.o read_dcv_state.o dcv_wintxt.o \
                 dcv_butt.o

${DCV_COMMS_DEPS}: dcv_comms.o

#ANDREW_COMMS_DEPS = andrew_tt.o andrew_code_index.o andrew_get_outcode_ll.o \
		    andrew_alldist.o andrew_refine.o,andrew_get_tt.o

#$(ANDREW_COMMS_DEPS): andrew_comms.o 

#VENN_COMMS_DEPS = andrew_venn.o

$(VENN_COMMS_DEPS): venn_comms.o 

QUEUE_COMMS_DEPS = txnwin.o multiserv.o

$(QUEUE_COMMS_DEPS): qcomm.o

SUDOKU_COMMS_DEPS = sud_disp_check.o sud_loners.o sud_process_entry.o \
		    sud_small_display.o sudoku.o sud_display.o sud_xwinger.o

$(SUDOKU_COMMS_DEPS): sudoku_comms.o 

IP_COMMS_DEPS = ip_histo.o ip_spread.o ip_brighten.o ip_floor.o ip_sharpen.o \
		ip_gamma.o  ip_contrast.o ip_master.o mask_test.o

$(IP_COMMS_DEPS): ip_comms.o

FFT_DEPS:  array2bmp.o Times.o Cffti.o Cfftf.o Cfftb.o \
	   Passf_x.o Passf_y.o Passb_x.o Passb_y.o ip_fft_display.o

FFTOBJS =  Times.o flipflop.o transpose.o Cffti.o Cfftf.o Cfftb.o xxstatmap.o\
	   Passf_x.o Passf_y.o Passb_x.o Passb_y.o ip_fft_display.o \
	   quad_transpose.o

DCV_OBJS = dcv_activate.o dcv_butt_action.o dcv_comms.o \
           assess_dcv_state.o read_dcv_state.o dcv_wintxt.o

STATOBJS = add_toc_item.o addutil.o allcomms.o bar.o close_lists.o convrec.o \
           io_sizer.o lsquare.o mem_alloc.o memsmooth.o nzxy.o stddev.o \
           nzy.o open_files.o opsys_name.o plotutil.o predict.o read_io_rec.o \
           numstrip.o read_top_rec.o remblanks.o sdev.o summer.o toprec.o  \
	   isoflush.o

COLSTATOBJS = amedian.o arrows.o button_action.o button_setup.o buttons.o \
              column_set.o ns.o ctim.o draw_butt.o draw_ruler.o host_input.o \
              isox11.o make_button.o makemap.o paint_all.o paint_hosts.o \
              paint_map.o paint_prep.o select_view.o start_win.o \
              text_line.o tim.o title_rates.o winset.o win_type.o \
              locate_button.f

BMOBJS = bmp_comms.o nanopause.o array2bmp.o compressor.o \
         label_maker.o samewrite.o diffwrite.o adintel2.o adintel4.o \
	 rgbindex.o greymap.o get_font.o text_writer.o

DAOBJS = ns.o ctim.o da_blksize.o da_close.o da_init.o da_open.o darread.o \
         darupdt.o dasread.o da_txfer.o dawrite.o ddread.o \
         ddwrite.o kxgen.o mountpt.o nextrnd.o taperead.o \
         tapewrite.o tdelta.o tim.o unremount.o isoflush.o dacomm.o

SLAVEOBJS = box.o bisbox.o linout.o minit.o filler.o rectput.o grafout.o \
            column.o liner.o looper.o

MASTOBJS = summline.o artist_wait.o side_calc.o

ARTSTOBJS = axes.o bound.o cept.o cosrule.o ns.o ctim.o cubeadd.o cubecheck.o \
            cubeinit.o cubelist.o cubespec.o cubesplit.o datout.o \
            deconx.o defmap.o drawcube.o drawface.o edge.o \
            equalcol.o eyepoint.o flooralone.o floorshow.o hidrem.o \
            intersector.o isocomm.o isox11.o iterdist.o linecheck.o \
            linesegs.o makeline.o outline.o param_list.o photomap.o \
            photospot.o picker.o prepact.o pyth.o scene.o \
            screenscale.o slope.o tanxy.o tim.o title.o \
            tripleset.o validate.o vcheck.o visibility.o vrecord.o \
            isoflush.o statout.o read_cmap_params.o

PAROBJS = lts.o getmp.o mpiwindup.o parcomm.o mpirelease.o

IPOBJS = ip_histo.o ip_spread.o ip_brighten.o ip_floor.o ip_sharpen.o \
	 ip_gamma.o ip_comms.o ip_contrast.o slope.o cept.o picdims.o \
	 ip_distribute.o

STEMOBJS = easy_midi_comms.o make_midi_time.o easy_midi_tempo.o \
	    easy_midi_xvolume.o easy_midi_timesig.o easy_midi_key.o \
	    easy_midi_numerics.o easy_midi_tvolume.o

SURFOBJS = surf_comms.o sf_solver.o sf_cmpres.o sf_coeffa.o sf_coeffb.o \
	   sf_coeffc.o sf_nc.o sf_sturgn.o read_surface.o binomial.o \
	   lightgen.o

SURF_DEPS = sf_solver.o sf_cmpres.o sf_coeffa.o sf_coeffb.o \
	   sf_coeffc.o sf_nc.o sf_sturgn.o read_surface.o binomial.o \
	   lightgen.o surf.o

$(SURF_DEPS): surf_comms.o

GRAPHOBJS = grf_header.o grf_points.o grf_trailer.o

#ANDREWOBJS = andrew_code_index.o andrew_get_outcode_ll.o sphere_dist.o \
	     andrew_clash.o andrew_get_tt.o len_file.o \
	     dfd.o andrew_comms.o
	   

GEOBJS =     standoff.o ge_input.o segsc.o cept.o slope.o pyth2d.o cosrule.o \
	     d_pointline.o  origin.o lat2y.o lon2x.o crossover.o intext.o  \
	     gridmap.o polyfill.o poly_alloc.o x2lon.o y2lat.o  \
	     polyprep.o poly_dealloc.o grid_alloc.o bmp_comms.o xxstatmap.o \
	     ip_comms.o gridbmp.o compressor.o array2bmp.o samewrite.o    \
	     diffwrite.o

VENNOBJS =   ge_input.o segsc.o poly_x.o cept.o slope.o pyth2d.o \
	     origin.o lat2y.o lon2x.o crossover.o intext.o  \
	     gridmap.o kml_prep.o polyfill.o poly_alloc.o x2lon.o y2lat.o  \
	     polyprep.o grid_alloc.o bmp_comms.o xxstatmap.o \
	     ip_comms.o gridbmp.o compressor.o array2bmp.o samewrite.o    \
	     dfd.o venn_comms.o diffwrite.o sphere_dist.o polytest.o polysplit.o

$(ISOPARIX)/isoparix.exe: isoparix.o isomast.o isoslave.o isoartst.o $(MASTOBJS) $(SLAVEOBJS) $(ARTSTOBJS) $(BMOBJS) $(PAROBJS)
	$(MPFC) $(PARFLAGS) isoparix.o isomast.o isoslave.o isoartst.o $(MASTOBJS) $(SLAVEOBJS) $(ARTSTOBJS) $(BMOBJS) $(PAROBJS) -o $@ $(LD_X11) $(LD_MPI)

$(ISOPARIX)/test_defmap.exe: test_defmap.o $(BMOBJS) $(ARTSTOBJS) 
	$(LD)   test_defmap.o $(BMOBJS) $(ARTSTOBJS) -o $@ $(LD_X11)

$(ISOPARIX)/test_side.exe: test_side.o side_calc.o 
	$(LD)   test_side.o side_calc.o -o $@

$(ISOPARIX)/fpi.exe:   fpi.o $(PAROBJS)
	$(MPFC) $(PARFLAGS) fpi.o -o $@ $(LD_MPI)

$(ISOPARIX)/test_mpi.exe:   test_mpi.o $(PAROBJS)           nanopause.o tim.o ns.o ctim.o isocomm.o isoflush.o
	$(MPFC) $(PARFLAGS) test_mpi.o $(PAROBJS) statout.o nanopause.o tim.o ns.o ctim.o isocomm.o isoflush.o  -o $@ $(LD_MPI)

$(ISOPARIX)/compress_test.exe: compress_test.o $(ARTSTOBJS) $(BMOBJS)
	$(MPFC) $(PARFLAGS) compress_test.o $(ARTSTOBJS) $(BMOBJS) -o $@ $(LD_X11)

$(ISOPARIX)/offprep.exe: offprep.o $(ARTSTOBJS) $(BMOBJS)
	$(MPFC) $(PARFLAGS) offprep.o $(ARTSTOBJS) $(BMOBJS) -o $@ $(LD_X11)

$(ISOPARIX)/msetview.exe: msetview.o $(ARTSTOBJS) $(BMOBJS)
	$(MPFC) $(PARFLAGS) msetview.o $(ARTSTOBJS) $(BMOBJS) -o $@ $(LD_X11)

$(ISOPARIX)/array3D.exe: array3D.o $(ARTSTOBJS) $(BMOBJS)
	$(MPFC) $(PARFLAGS) array3D.o $(ARTSTOBJS) $(BMOBJS) -o $@ $(LD_X11)

$(ISOPARIX)/label_bmp.exe: $(ARTSTOBJS) $(BMOBJS) label_bmp.o 
	$(MPFC) $(PARFLAGS)   $(ARTSTOBJS) $(BMOBJS) label_bmp.o -o $@ $(LD_X11) $(LD_X11)

$(ISOPARIX)/test_alloc.exe: test_alloc.o 
	$(LD)  test_alloc.o -o $@

$(ISOPARIX)/font_file.exe: font_file.o 
	$(LD)  font_file.o -o $@

$(ISOPARIX)/font_read.exe: font_read.o 
	$(LD)  font_read.o -o $@

$(ISOPARIX)/fractiler.exe: fractiler.o defmap.o read_cmap_params.o tim.o ns.o ctim.o isoflush.o isocomm.o $(BMOBJS)
	$(LD)  fractiler.o defmap.o read_cmap_params.o tim.o ns.o ctim.o isoflush.o isocomm.o $(BMOBJS) -o $@

$(ISOPARIX)/readbmp.exe: readbmp.o
	$(LD)  readbmp.o -o $@

$(ISOPARIX)/testxod.exe: testxod.o $(ARTSTOBJS) $(BMOBJS)
	$(LD)  testxod.o $(ARTSTOBJS) $(BMOBJS) -o $@ $(LD_X11)

$(ISOPARIX)/showcube.exe: showcube.o $(ARTSTOBJS) $(BMOBJS)
	$(LD)  showcube.o $(ARTSTOBJS) $(BMOBJS) -o $@ $(LD_X11)

$(ISOPARIX)/showcubes.exe: showcubes.o $(ARTSTOBJS) $(BMOBJS)
	$(LD)   showcubes.o $(ARTSTOBJS) $(BMOBJS) -o $@ $(LD_X11)

$(XXSTATIX)/equalcol_test.exe: equalcol_test.o $(ARTSTOBJS) $(BMOBJS)
	$(MPFC) $(PARFLAGS) equalcol_test.o $(ARTSTOBJS) $(BMOBJS) -o $@ $(LD_X11)

$(XXSTATIX)/netstatin.exe: netstatin.o $(STATOBJS)
	$(LD)  netstatin.o $(STATOBJS) -o $@

$(XXSTATIX)/vmstatin.exe: vmstatin.o $(STATOBJS)
	$(LD)  vmstatin.o $(STATOBJS) -o $@

$(XXSTATIX)/iostatin.exe: iostatin.o $(STATOBJS)
	$(LD)  iostatin.o $(STATOBJS) -o $@

$(XXSTATIX)/netstat.exe:  netstat.o $(STATOBJS)
	$(LD)  netstat.o $(STATOBJS) -o $@

$(XXSTATIX)/vmstat.exe:	  vmstat.o $(STATOBJS)
	$(LD)  vmstat.o $(STATOBJS) -o $@

$(XXSTATIX)/iostat.exe:   iostat.o $(STATOBJS)
	$(LD)  iostat.o $(STATOBJS) -o $@

$(XXSTATIX)/summan.exe:	summan.o $(STATOBJS)
	$(LD)  	summan.o $(STATOBJS) -o $@

$(XXSTATIX)/data_generator.exe:	isoflush.o dg.o data_generator.o tim.o ns.o ctim.o nanopause.o remblanks.o data_gen.o
	$(LD)  	isoflush.o dg.o data_generator.o tim.o ns.o ctim.o nanopause.o remblanks.o data_gen.o -o $@

$(XXSTATIX)/recsum.exe:	recsum.o $(STATOBJS)
	$(LD)  	recsum.o $(STATOBJS) -o $@

$(XXSTATIX)/topmon.exe:	topmon.o $(STATOBJS)
	$(LD)  	topmon.o $(STATOBJS) -o $@

$(XXSTATIX)/jiggler.exe: jiggler.o isox11.o
	$(LD) jiggler.o isox11.o -o $@ $(LD_X11)

$(XXSTATIX)/iostatmon.exe: iostatmon.o $(STATOBJS)
	$(LD) iostatmon.o $(STATOBJS) -o $@

$(XXSTATIX)/dcv_state.exe: dcv_state.o $(DCV_OBJS)  $(STATOBJS) $(COLSTATOBJS) $(BMOBJS)
	$(LD) dcv_state.o $(DCV_OBJS) $(STATOBJS) $(COLSTATOBJS) $(BMOBJS) -o $@ $(LD_X11)

$(XXSTATIX)/wintext.exe: wintext.o $(STATOBJS) $(COLSTATOBJS) $(BMOBJS)
	$(LD) wintext.o $(STATOBJS) $(COLSTATOBJS) $(BMOBJS) -o $@ $(LD_X11)

$(XXSTATIX)/XConsortium_test.exe: XConsortium_test.o
	$(LD) XConsortium_test.o -o $@ $(LD_X11)

$(XXSTATIX)/example.exe: example.o
	$(CC) example.o -o $@ $(LD_X11)

$(XXSTATIX)/franco.exe: franco.o
	$(CC) franco.o -o $@ $(LD_X11)

$(XXSTATIX)/svn_calib.exe: svn_calib.o isox11.o
	$(LD) svn_calib.o isox11.o  -o $@ $(LD_X11)

$(XXSTATIX)/dcv_butt.exe: dcv_butt.o $(DCV_OBJS) $(STATOBJS) $(COLSTATOBJS) $(BMOBJS)
	$(LD) dcv_butt.o $(DCV_OBJS) $(STATOBJS) $(COLSTATOBJS) $(BMOBJS) -o $@ $(LD_X11)

$(XXSTATIX)/test_butt.exe: test_butt.o $(STATOBJS) $(COLSTATOBJS) $(BMOBJS)
	$(LD) test_butt.o $(STATOBJS) $(COLSTATOBJS) $(BMOBJS) -o $@ $(LD_X11)

$(XXSTATIX)/colmaps.exe: colstat.o $(STATOBJS) $(COLSTATOBJS) $(BMOBJS)
	$(LD) colstat.o $(STATOBJS) $(COLSTATOBJS) $(BMOBJS) -o $@ $(LD_X11)

$(DATEST)/timanal.exe: timanal.o
	$(LD) timanal.o -o $@

$(DATEST)/datesdat.exe: datesdat.o $(DAOBJS)
	$(LD) datesdat.o $(DAOBJS) -o $@

$(DATEST)/spmulti.exe: spmulti.o
	$(LD) spmulti.o -o $@

$(DATEST)/fosyan.exe: fosyan.o
	$(LD) fosyan.o -o $@

$(DATEST)/txtanl.exe: txtanl.o isoflush.o
	$(LD) txtanl.o isoflush.o -o $@

$(DATEST)/darupdate.exe: darupdate.o $(DAOBJS)
	$(LD) darupdate.o $(DAOBJS) -o $@

$(DATEST)/darepr.exe: darepr.o $(DAOBJS)
	$(LD) darepr.o $(DAOBJS) -o $@

$(DATEST)/taperepw.exe: taperepw.o $(DAOBJS)
	$(LD) taperepw.o $(DAOBJS) -o $@

$(DATEST)/tapereps.exe: tapereps.o $(DAOBJS)
	$(LD) tapereps.o $(DAOBJS) -o $@

$(DATEST)/tapetest.exe: tapetest.o $(DAOBJS)
	$(LD) tapetest.o $(DAOBJS) -o $@

$(DATEST)/datest.exe: datest.o $(DAOBJS)
	$(LD) datest.o $(DAOBJS) -o $@

$(DATEST)/ddreps.exe: ddreps.o $(DAOBJS)
	$(LD) ddreps.o $(DAOBJS) -o $@

$(DATEST)/ddrepws.exe: ddrepws.o $(DAOBJS)
	$(LD) ddrepws.o $(DAOBJS) -o $@

$(DATEST)/ddrepw.exe: ddrepw.o $(DAOBJS)
	$(LD) ddrepw.o $(DAOBJS) -o $@

$(DATEST)/dareps.exe: dareps.o $(DAOBJS)
	$(LD) dareps.o $(DAOBJS) -o $@

$(DATEST)/darepws.exe: darepws.o $(DAOBJS)
	$(LD) darepws.o $(DAOBJS) -o $@

$(DATEST)/darepw.exe: darepw.o $(DAOBJS)
	$(LD) darepw.o $(DAOBJS) -o $@

$(DATEST)/killmulti.exe: killmulti.o
	$(LD) killmulti.o -o $@

$(DATEST)/inverter.exe:	inverter.o
	$(LD) inverter.o -o $@

$(DATEST)/fosy01.exe: fosy01.o ${DAOBJS}
	$(LD) fosy01.o ${DAOBJS} -o $@

$(XXSTATIX)/iowc.exe: iowc.o ns.o ctim.o tim.o
	$(LD) iowc.o ns.o ctim.o tim.o  -o $@

$(SURF)/shine.exe: shine.o 
	$(LD) shine.o -o $@

$(SURF)/shinebmp.exe: shinebmp.o $(BMOBJS) $(ARTSTOBJS)
	$(LD) shinebmp.o $(BMOBJS)  $(ARTSTOBJS) -o $@ $(LD_X11)

$(SURF)/sf_explore.exe: sf_explore.o $(SURFOBJS) tim.o ns.o ctim.o
	$(LD) sf_explore.o $(SURFOBJS) tim.o ns.o ctim.o -o $@ $(LD_X11)

$(SURF)/surf.exe: surf.o $(SURFOBJS) tim.o ns.o ctim.o
	$(LD) surf.o $(SURFOBJS) tim.o ns.o ctim.o -o $@ $(LD_X11)

$(SURF)/mixer.exe: mixer.o $(SURFOBJS) tim.o ns.o ctim.o
	$(LD) mixer.o $(SURFOBJS) tim.o ns.o ctim.o -o $@ $(LD_X11)

$(SURF)/translator.exe: translator.o $(SURFOBJS)
	$(LD) translator.o $(SURFOBJS) -o $@

$(SURF)/swapper.exe: swapper.o
	$(LD) swapper.o -o $@

$(SURF)/morph.exe: morph.o $(SURFOBJS)
	$(LD) morph.o $(SURFOBJS) -o $@

$(SURF)/panorama.exe: panorama.o $(SURFOBJS)
	$(LD) panorama.o $(SURFOBJS) -o $@

$(SURF)/transform.exe: transform.o $(SURFOBJS)
	$(LD) transform.o $(SURFOBJS) -o $@

$(SLIDES)/read_test.exe: read_test.o
	$(LD) read_test.o -o $@

$(TOOLS)/barclays_csv.exe: $(GRAPHOBJS) barclays_csv.o
	$(LD) $(GRAPHOBJS) barclays_csv.o -o $@

$(TOOLS)/barclays_pdf.exe: barclays_pdf.o isotime.o
	$(LD) barclays_pdf.o isotime.o -o $@

$(TOOLS)/barclays.exe: barclays.o
	$(LD) barclays.o -o $@

$(TOOLS)/test.exe: test.o
	$(LD) test.o -o $@

$(TOOLS)/gpx_elev.exe: gpx_elev.o $(GRAPHOBJS)
	$(LD) gpx_elev.o $(GRAPHOBJS) -o $@

$(TOOLS)/lottery.exe: lottery.o $(DAOBJS)
	$(LD) lottery.o $(DAOBJS) -o $@

$(TOOLS)/test_random.exe: test_random.o $(DAOBJS)
	$(LD) test_random.o $(DAOBJS) -o $@

$(TOOLS)/cantilever.exe: cantilever.o $(BMOBJS) $(ARTSTOBJS)
	$(LD) cantilever.o $(BMOBJS) $(ARTSTOBJS) -o $@ $(LD_X11)

$(TOOLS)/psa_anal.exe: psa_anal.o
	$(LD) psa_anal.o -o $@

$(TOOLS)/elder_shifter.exe: elder_shifter.o elder_comms.o elder_filler.o
	$(LD) elder_shifter.o elder_comms.o elder_filler.o -o $@

ELDER_DEPS  = elder_shifter.o elder_filler.o

$(ELDER_DEPS): elder_comms.o

$(TOOLS)/decimal_time.exe: decimal_time.o isotime.o
	$(LD) decimal_time.o isotime.o -o $@

$(TOOLS)/test_graph.exe: $(GRAPHOBJS) test_graph.o isotime.o
	$(LD) $(GRAPHOBJS) test_graph.o isotime.o -o $@

$(TOOLS)/blood_pressure.exe: $(GRAPHOBJS) blood_pressure.o isotime.o
	$(LD) $(GRAPHOBJS) blood_pressure.o isotime.o -o $@

$(TOOLS)/websize.exe: websize.o picdims.o
	$(LD) websize.o picdims.o -o $@

$(TOOLS)/rot_geom.exe: rot_geom.o
	$(LD) rot_geom.o -o $@

$(TOOLS)/pythag.exe: pythag.o pyth.o
	$(LD) pythag.o pyth.o -o $@

$(SLIDES)/mask_test.exe: mask_test.o tim.o ns.o ctim.o isoflush.o $(BMOBJS) $(IPOBJS)
	$(LD) mask_test.o tim.o ns.o ctim.o isoflush.o $(BMOBJS) $(IPOBJS) -o $@

$(SLIDES)/ip_tiler.exe: ip_tiler.o $(IPOBJS) ns.o ctim.o tim.o isoflush.o $(BMOBJS)
	$(LD) ip_tiler.o $(IPOBJS) ns.o ctim.o tim.o isoflush.o $(BMOBJS) -o $@

$(SLIDES)/ip_greyscale.exe: ip_greyscale.o $(IPOBJS) ns.o ctim.o tim.o isoflush.o $(BMOBJS)
	$(LD) ip_greyscale.o $(IPOBJS) ns.o ctim.o tim.o isoflush.o $(BMOBJS) -o $@

$(TOOLS)/ip_slopes.exe: ip_slopes.o $(IPOBJS) ns.o ctim.o tim.o isoflush.o $(BMOBJS)
	$(LD) ip_slopes.o $(IPOBJS) ns.o ctim.o tim.o isoflush.o $(BMOBJS) -o $@

$(TOOLS)/ip_fft_testbed.exe: ip_fft_testbed.o $(BMOBJS) $(IPOBJS) $(FFTOBJS)
	$(LD) ip_fft_testbed.o  ns.o ctim.o tim.o isoflush.o $(BMOBJS) $(IPOBJS) $(FFTOBJS) -o $@

$(TOOLS)/ip_fft.exe: ip_fft.o $(BMOBJS) $(IPOBJS) $(FFTOBJS)
	$(LD) ip_fft.o  ns.o ctim.o tim.o isoflush.o $(BMOBJS) $(IPOBJS) $(FFTOBJS) -o $@

#$(TOOLS)/FFT_control.exe: FFT_control.o $(FFTOBJS)
#	$(LD) FFT_control.o $(FFTOBJS) -o $@

$(TOOLS)/FFT.exe: FFT.o $(BMOBJS) $(IPOBJS) $(FFTOBJS)
	$(LD) FFT.o  ns.o ctim.o tim.o isoflush.o $(BMOBJS) $(IPOBJS) $(FFTOBJS) -o $@

$(TOOLS)/ip_master.exe: ip_master.o $(IPOBJS) ns.o ctim.o tim.o isoflush.o $(BMOBJS)
	$(LD) ip_master.o $(IPOBJS) ns.o ctim.o tim.o isoflush.o $(BMOBJS) -o $@

$(TOOLS)/resolution.exe: resolution.o $(IPOBJS) ns.o ctim.o tim.o isoflush.o $(BMOBJS)
	$(LD) resolution.o $(IPOBJS) ns.o ctim.o tim.o isoflush.o $(BMOBJS) -o $@

$(TOOLS)/img_mult.exe: img_mult.o $(IPOBJS) ns.o ctim.o tim.o isoflush.o $(BMOBJS)
	$(LD) img_mult.o $(IPOBJS) ns.o ctim.o tim.o isoflush.o $(BMOBJS) -o $@

$(TOOLS)/solar_master.exe: solar_master.o $(IPOBJS) xxstatmap.o ns.o ctim.o tim.o isoflush.o $(BMOBJS)
	$(LD) solar_master.o $(IPOBJS) xxstatmap.o ns.o ctim.o tim.o isoflush.o $(BMOBJS) -o $@

$(TOOLS)/dofor.exe: dofor.o 
	$(LD) dofor.o -o $@

$(SLIDES)/border_f.exe: border.o view_avg.o
	$(LD) border.o view_avg.o -o $@

$(SLIDES)/slide_rename.exe: slide_rename.o
	$(LD) slide_rename.o -o $@

$(SLIDES)/slide_crop.exe: slide_crop.o
	$(LD) slide_crop.o -o $@

$(STEM)/test_make_midi_time.exe: test_make_midi_time.o $(STEMOBJS)
	$(LD) $(FFLAGS) test_make_midi_time.o $(STEMOBJS) -o $@

$(STEM)/read_midiplay_txt.exe: read_midiplay_txt.o $(STEMOBJS)
	$(LD) $(FFLAGS) read_midiplay_txt.o $(STEMOBJS) -o $@

$(STEM)/easy_midi.exe: easy_midi.o $(STEMOBJS)
	$(LD) $(FFLAGS) easy_midi.o $(STEMOBJS) -o $@

$(STEM)/read_midi.exe: read_midi.o $(STEMOBJS)
	$(LD) $(FFLAGS) read_midi.o -o $@

$(FINANCE_JSW)/finpie.exe: finpie.o 
	$(LD) finpie.o -o $@ 

$(FINANCE_JSW)/finsd.exe: finsd.o 
	$(LD) finsd.o -o $@

$(FINANCE_JSW)/finextract.exe: finextract.o 
	$(LD) finextract.o -o $@

$(FINANCE_JSW)/finsum.exe: finsum.o 
	$(LD) finsum.o -o $@

$(FINANCE_JSW)/finanal.exe: finanal.o $(GRAPHOBJS)
	$(LD) finanal.o $(GRAPHOBJS) -o $@

$(FINANCE_JSW)/fingather.exe: fingather.o 
	$(LD) fingather.o -o $@

$(SUDOKU)/sudoku.exe: sudoku_comms.o $(SUDOKU_COMMS_DEPS)
	$(LD) sudoku_comms.o $(SUDOKU_COMMS_DEPS) -o $@

$(ANDREW)/test_kml_read.exe: test_kml_read.o isoflush.o $(ANDREWOBJS) $(GEOBJS)
	$(LD) test_kml_read.o isoflush.o $(ANDREWOBJS) $(GEOBJS) -o $@

$(ANDREW)/test_dist.exe: test_dist.o isoflush.o $(ANDREWOBJS) $(GEOBJS)
	$(LD) test_dist.o isoflush.o $(ANDREWOBJS) $(GEOBJS) -o $@

$(ANDREW)/showpoly.exe: showpoly.o  isoflush.o $(GRAPHOBJS) $(VENNOBJS)
	$(LD) showpoly.o  isoflush.o $(GRAPHOBJS) $(VENNOBJS) -o $@

$(ANDREW)/andrew_venn.exe: andrew_venn.o isoflush.o $(GRAPHOBJS) $(VENNOBJS)
	$(LD) andrew_venn.o isoflush.o $(GRAPHOBJS) $(VENNOBJS) -o $@

$(ANDREW)/andrew_refine.exe: andrew_refine.o $(GRAPHOBJS)  $(ANDREWOBJS)
	$(LD) andrew_refine.o $(GRAPHOBJS)  $(ANDREWOBJS) -o $@

$(ANDREW)/andrew_tt.exe: andrew_tt.o $(ANDREWOBJS)
	$(LD) andrew_tt.o $(ANDREWOBJS) -o $@

$(ANDREW)/andrew_dfd.exe: andrew_dfd.o $(ANDREWOBJS)
	$(LD) andrew_dfd.o $(ANDREWOBJS) -o $@

$(ANDREW)/andrew_graph.exe: andrew_graph.o $(GRAPHOBJS)  $(ANDREWOBJS)
	$(LD) andrew_graph.o $(GRAPHOBJS)  $(ANDREWOBJS) -o $@ $(LD_G95$(GRAPHOBJS) )

$(ANDREW)/andrew_alldist.exe: andrew_alldist.o $(ANDREWOBJS)
	$(LD) andrew_alldist.o $(ANDREWOBJS) -o $@

$(ANDREW)/andrew_earth_dist.exe: andrew_earth_dist.o $(ANDREWOBJS)
	$(LD) andrew_earth_dist.o $(ANDREWOBJS) -o $@

$(TOOLS)/survey_lines.exe: survey_lines.o
	$(LD) survey_lines.o -o $@

$(TOOLS)/dup_txt.exe: dup_txt.o
	$(LD) dup_txt.o -o $@

$(TOOLS)/join_lines.exe: join_lines.o
	$(LD) join_lines.o -o $@

$(TOOLS)/architrave_curve.exe: architrave_curve.o
	$(LD) architrave_curve.o -o $@

$(TOOLS)/jpegtime.exe: jpegtime.o
	$(LD) jpegtime.o -o $@

$(TOOLS)/cake.exe: cake.o
	$(LD) cake.o -o $@

$(TOOLS)/piston_search.exe: extension.o piston_search.o
	$(LD) extension.o piston_search.o -o $@ $(LD_X11)

$(TOOLS)/piston.exe: extension.o piston.o
	$(LD) extension.o piston.o -o $@ $(LD_X11)

$(TOOLS)/parallel_track.exe: extension.o cosrule.o parallel_track.o $(GRAPHOBJS) $(ANDREWOBJS)
	$(LD) extension.o cosrule.o parallel_track.o $(GRAPHOBJS) $(ANDREWOBJS) -o $@ $(LD_X11)

$(TOOLS)/pivot_graph.exe: extension.o cosrule.o pivot_graph.o $(GRAPHOBJS) $(ANDREWOBJS)
	$(LD) extension.o cosrule.o pivot_graph.o $(GRAPHOBJS) $(ANDREWOBJS) -o $@ $(LD_X11)

$(TOOLS)/elevation.exe: elevation.o $(GRAPHOBJS)  
	$(LD) elevation.o $(GRAPHOBJS) -o $@

$(TOOLS)/test_card.exe: test_card.o ns.o ctim.o isoflush.o tim.o $(BMOBJS)
	$(LD) test_card.o ns.o ctim.o isoflush.o tim.o $(BMOBJS) -o $@ $(LD_X11)

$(TOOLS)/latitude_slope.exe: latitude_slope.o ns.o ctim.o isoflush.o tim.o cept.o slope.o $(BMOBJS)
	$(LD) latitude_slope.o  ns.o ctim.o isoflush.o tim.o cept.o slope.o $(BMOBJS) -o $@ $(LD_X11)

$(TOOLS)/inch_swg.exe: inch_swg.o
	$(LD) inch_swg.o -o $@ $(LD_X11)

$(TOOLS)/tmp_calc.exe: tmp_calc.o
	$(LD) tmp_calc.o -o $@ $(LD_X11)

$(TOOLS)/txnwin.exe: txnwin.o projector.o qcomm.o
	$(LD) txnwin.o projector.o qcomm.o -o $@ $(LD_X11)

$(TOOLS)/declination.exe: declination.o ns.o ctim.o isoflush.o tim.o $(BMOBJS)
	$(LD) declination.o ns.o ctim.o isoflush.o tim.o $(BMOBJS) -o $@ $(LD_X11)

$(TOOLS)/hour_angle_protractor.exe: hour_angle_protractor.o ns.o ctim.o isoflush.o tim.o $(BMOBJS)
	$(LD) hour_angle_protractor.o ns.o ctim.o isoflush.o tim.o $(BMOBJS) -o $@ $(LD_X11)

$(TOOLS)/multiserv.exe: multiserv.o projector.o qcomm.o
	$(LD) multiserv.o projector.o qcomm.o -o $@ $(LD_X11)

$(TOOLS)/sieve.exe: sieve.o
	$(LD) sieve.o -o $@

$(TOOLS)/section_area.exe: section_area.o
	$(LD) section_area.o -o $@

$(TOOLS)/adventdates.exe: adventdates.o
	$(LD) adventdates.o -o $@


#NETWORKS_DEPS = links.o
#$(NETWORKS_DEPS): links_comms.o link_builder.o clash_check.o links_grid.o
#$(NETWORKS)/links.exe: links.o links_comms.o link_builder.o clash_check.o links_grid.o
#	$(LD) links.o links_comms.o link_builder.o clash_check.o links_grid.o -o $@

cleanx11:
	/bin/rm -f svn_calib.o isox11.o example.o $(XXSTATIX)/svn_calib.exe $(XXSTATIX)/example.exe

clean:
	/bin/rm -f *.o *.mod $(STEM)/*.exe $(SURF)/*.exe $(XXSTATIX)/*.exe $(DATEST)/*.exe $(ISOPARIX)/*.exe core* $(ANDREW)/*.exe

showflags:
	@echo "PATH     $(PATH)"
	@echo "LD       $(LD)" 
	@echo "CC       $(CC)" 
	@echo "CFLAGS   $(CFLAGS)" 
	@echo "FC       $(FC)"
	@echo "FFLAGS   $(FFLAGS)"
	@echo "STEM     $(STEM)"
	@echo "SURF     $(SURF)"
	@echo "ISOPARIX $(ISOPARIX)"
	@echo "XXSTATIX $(XXSTATIX)"
	@echo "DATEST   $(DATEST)"
	@echo "ANDREW   $(ANDREW)"
	@echo "FINANCE_JSW $(FINANCE_JSW)"
	@echo "TOOLS    $(TOOLS)"
	@echo "SLIDE_CROPPER  $(SLIDES)"
	@echo "PARFLAGS $(PARFLAGS)"
	@echo "LD_X11   $(LD_X11)"
	@echo "LD_MPI   $(LD_MPI)"
