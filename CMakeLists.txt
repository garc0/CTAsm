cmake_minimum_required(VERSION 3.0.0)
project(ctasm VERSION 0.1.0)

include(CTest)
enable_testing()

add_subdirectory("zydis")
add_subdirectory("include")

add_executable(ctasm headonlyx86asm.cpp)

target_link_libraries(ctasm PRIVATE "Zydis")

set_property(TARGET ctasm PROPERTY CXX_STANDARD 17)

set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
include(CPack)
