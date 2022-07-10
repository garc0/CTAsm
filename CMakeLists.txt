cmake_minimum_required(VERSION 3.0.0)
project(ctasm VERSION 0.1.0)

include(CTest)
enable_testing()

include_directories("include")

add_executable(ctasm examples/example01.cpp)

target_link_libraries(ctasm)

set_property(TARGET ctasm PROPERTY CXX_STANDARD 17)

set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
include(CPack)
