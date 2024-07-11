cmake_minimum_required(VERSION 3.1)

project (pmpmeas)

enable_language(Fortran)

option(USEPAPI "Build with PAPI" ON)
#set(PAPIDIR "/usr")
set(PAPICNTMAX 3)

option(USERDTSC "Enable time measurement using RDTSC (x86 only)" ON)
option(USETIMEBASE "Enable time measurement using timebase register (POWER only)" OFF)

#set(CXX "g++-10")
#set(CXXFLAGS "-g")

add_library(pmpmeas lib/pmpmeas-api.c lib/pmpmeas.cc lib/meas.cc lib/papiinf.cc lib/perfinf.cc)
add_library(m_pmpmeas lib/m_pmpmeas.f90)
add_executable(tests_api-c.x tests/api-c.c)
add_executable(tests_api-cpp.x tests/api-cpp.cc)
add_executable(tests_api-f90.x tests/api-f90.f90)

add_definitions(-DPAPICNTMAX=${PAPICNTMAX})

if (USEPAPI)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DUSEPAPI -I${PAPIDIR}/include")
  set(CMAKE_CXX_STANDARD_LIBRARIES "${CMAKE_CXX_STANDARD_LIBRARIES} -L${PAPIDIR}/lib -Wl,-rpath ${PAPIDIR}/lib -lpapi")
  set(EXTRA_INCDIR ${EXTRA_INCDIR} ${PAPIDIR}/include)
endif()

if (USERDTSC)
  add_definitions(-DUSERDTSC)
endif()

if (USETIMEBASE)
  add_definitions(-DUSETIMEBASE)
endif()

target_include_directories(pmpmeas PUBLIC
	${CMAKE_CURRENT_SOURCE_DIR}/include ${EXTRA_LIBS_INCDIR})
target_include_directories(tests_api-c.x PUBLIC
	${CMAKE_CURRENT_SOURCE_DIR}/include
	${CMAKE_CURRENT_SOURCE_DIR}/tests
	${EXTRA_LIBS_INCDIR})
target_include_directories(tests_api-cpp.x PUBLIC
	${CMAKE_CURRENT_SOURCE_DIR}/include
	${CMAKE_CURRENT_SOURCE_DIR}/tests
	${EXTRA_LIBS_INCDIR})
target_include_directories(tests_api-f90.x PUBLIC
	${CMAKE_CURRENT_SOURCE_DIR}/include
	${CMAKE_CURRENT_SOURCE_DIR}/tests
	${EXTRA_LIBS_INCDIR})

target_link_libraries(tests_api-c.x LINK_PUBLIC pmpmeas)
target_link_libraries(tests_api-cpp.x LINK_PUBLIC pmpmeas)
target_link_libraries(tests_api-f90.x LINK_PUBLIC m_pmpmeas)
target_link_libraries(tests_api-f90.x LINK_PUBLIC pmpmeas)

install(TARGETS pmpmeas DESTINATION lib)
install(FILES include/pmpmeas-api.h DESTINATION include)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/m_pmpmeas.mod DESTINATION include)
install(TARGETS tests_api-c.x DESTINATION tests)
install(TARGETS tests_api-cpp.x DESTINATION tests)
install(TARGETS tests_api-f90.x DESTINATION tests)

message("CMAKE_CXX_FLAGS: ${CMAKE_CXX_FLAGS}")
message("CMAKE_CXX_STANDARD_LIBRARIES: ${CMAKE_CXX_STANDARD_LIBRARIES}")