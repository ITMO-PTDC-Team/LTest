include(GNUInstallDirs)

set(LTEST_LIBRARY_DIR "${CMAKE_INSTALL_LIBDIR}/ltest")
set(LTEST_CMAKE_DIR "${CMAKE_INSTALL_LIBDIR}/cmake/LTest")
set_target_properties(runtime preload_with_main PROPERTIES INSTALL_RPATH "$ORIGIN")
set_target_properties(YieldPassExternal PROPERTIES EXPORT_NAME instrumentation)

install(TARGETS runtime preload_with_main YieldPassExternal
        EXPORT LTestTargets
        LIBRARY DESTINATION "${LTEST_LIBRARY_DIR}" COMPONENT LTest)
install(EXPORT LTestTargets NAMESPACE LTest::
        DESTINATION "${LTEST_CMAKE_DIR}" COMPONENT LTest)
install(FILES "${CMAKE_CURRENT_LIST_DIR}/LTestConfig.cmake"
        "${CMAKE_CURRENT_LIST_DIR}/LTest.cmake"
        DESTINATION "${LTEST_CMAKE_DIR}" COMPONENT LTest)
install(FILES "${PROJECT_SOURCE_DIR}/scripts/ltest_gdb.py"
        DESTINATION "${CMAKE_INSTALL_DATADIR}/ltest" COMPONENT LTest)
