cmake_minimum_required(VERSION 3.0.0)
project(headonlyx86asm VERSION 0.1.0)

include(CTest)
enable_testing()

add_subdirectory("zydis")

add_executable(headonlyx86asm headonlyx86asm.cpp)

target_link_libraries(headonlyx86asm PRIVATE "Zydis")

set_property(TARGET headonlyx86asm PROPERTY CXX_STANDARD 17)

set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
include(CPack)
