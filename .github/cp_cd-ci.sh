cd $HOME/github
for NAME in \
./apps/.github/workflows \
./ARGS/M_args/.github/workflows \
./ARGS/M_CLA/.github/workflows \
./ARGS/M_CLI2/.github/workflows \
./ARGS/M_getopt/.github/workflows \
./ARGS/M_getopt_long/.github/workflows \
./ARGS/M_kracken/.github/workflows \
./EXPRESSIONS/M_calculator/.github/workflows \
./EXPRESSIONS/M_calculator.mini/.github/workflows \
./EXPRESSIONS/M_evaluate/.github/workflows \
./EXPRESSIONS/M_logic/.github/workflows \
./investigate/.github/workflows \
./investigate/M_OS/.github/workflows \
./M_anything/.github/workflows \
./M_attr/.github/workflows \
./M_blas/.github/workflows \
./M_calcomp/.github/workflows \
./M_color/.github/workflows \
./M_datapac/.github/workflows \
./M_display/.github/workflows \
./M_factor/.github/workflows \
./M_graph/.github/workflows \
./M_hashkeys/.github/workflows \
./M_history/.github/workflows \
./M_html/.github/workflows \
./M_io/.github/workflows \
./M_knot/.github/workflows \
./M_LA/.github/workflows \
./M_list/.github/workflows \
./M_manifest/.github/workflows \
./M_math/.github/workflows \
./M_matrix/.github/workflows \
./M_msg/.github/workflows \
./M_namelist/.github/workflows \
./M_overload/.github/workflows \
./M_path/.github/workflows \
./M_pixel/.github/workflows \
./M_pppack/.github/workflows \
./M_process/.github/workflows \
./M_random/.github/workflows \
./M_readline/.github/workflows \
./M_slices/.github/workflows \
./M_sort/.github/workflows \
./M_starpac/.github/workflows \
./M_steam67/.github/workflows \
./M_stopwatch/.github/workflows \
./M_strings/.github/workflows \
./M_system/.github/workflows \
./M_time/.github/workflows \
./M_uuid/.github/workflows \
./M_xterm/.github/workflows \
./orderpack/.github/workflows \
./PLUGINS/fpm-cdci/.github/workflows \
./PLUGINS/fpm-gdb/.github/workflows \
./PLUGINS/fpm-license/.github/workflows \
./PREPROCESS/prep/.github/workflows \
./PREPROCESS/prep/.github/workflows.20221106 \
./programs/examples/.github/workflows \
./programs/what/.github/workflows \
./REGEX/M_match/.github/workflows \
./REGEX/M_regex/.github/workflows \
./slatec/.github/workflows \
./ush/.github/workflows \
$NULL
do
   cd $HOME/github
   cp ./easy/.github/workflows/test_gfortran_macos.yml $NAME/test_gfortran_macos.yml
   cp ./easy/.github/workflows/test_gfortran_msys_windows.yml  $NAME/test_gfortran_msys_windows.yml
   cp ./easy/.github/workflows/test_intel_ubuntu.yml $NAME/test_intel_ubuntu.yml
   cp ./easy/.github/workflows/deploy_api_docs.yml $NAME/deploy_api_docs.yml
   cp ./easy/.github/workflows/docs.yml $NAME/docs.yml
   cp ./easy/.github/workflows/test_gfortran_mingw64_windows.yml $NAME/test_gfortran_mingw64_windows.yml
   cp ./easy/.github/workflows/test_gfortran_windows.yml $NAME/test_gfortran_windows.yml
   cp ./easy/.github/workflows/test_gfortran_ubuntu.yml $NAME/test_gfortran_ubuntu.yml
done
exit

42098     2 ./ush/.github/workflows/test_nvfortran_ubuntu.yml

50160     2 ./PLUGINS/fpm-search/.github/workflows/fpm.yml
