if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})
    message(FATAL_ERROR "In-source builds are not allowed. Please create a separate build directory.")
endif()

cmake_minimum_required( VERSION 3.10 )

project( umnet C )

set( CMAKE_EXPORT_COMPILE_COMMANDS ON )

if( NOT CMAKE_BUILD_TYPE )
	set( CMAKE_BUILD_TYPE Release )
endif()
set( CMAKE_C_FLAGS "-DMLIBC ${CMAKE_C_FLAGS}" )
set( CMAKE_C_FLAGS_DEBUG "-DDEBUG" )
set( CMAKE_C_FLAGS_RELEASE "-DRELEASE" )
# set( CMAKE_VERBOSE_MAKEFILE ON )

add_library(
	umnet SHARED
	umnet.c
)

set_target_properties(
	umnet PROPERTIES
	PREFIX ""
	SUFFIX ".umi"
)

if( WIN32 )
	# message( STATUS "Build for Windows starts.." )
	add_definitions( -DPLATFORM_WINDOWS )
	set( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /GS- /GR- /EHsc /Wall /WX /permissive /wd4820 /wd4668 /wd5039 /wd4514 /wd5045 /wd4324" )
	add_library(umka STATIC IMPORTED)
    set_target_properties(umka PROPERTIES
        IMPORTED_LOCATION "${CMAKE_SOURCE_DIR}/libumka_static.lib"
    )

    add_library(mlibc STATIC IMPORTED)
    set_target_properties(mlibc PROPERTIES
        IMPORTED_LOCATION "${CMAKE_SOURCE_DIR}/mlibc.lib"
    )
	set(
		OS_LIBS
		Kernel32.lib
		Ws2_32.lib
		Winmm.lib
	)
elseif( UNIX )
	# message( STATUS "Build for Linux starts.." )
	add_definitions( -DPLATFORM_LINUX )
	set( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC -nodefaultlibs -fno-exceptions -pedantic -Wall -Wextra -Wconversion -Wreturn-type -Werror=all -Werror=extra -Werror=conversion -Werror=return-type -Wno-unused-function -L.." )
	set( CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g2" )
	set( CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3" )
	set(
		OS_LIBS
		c
		m
	)
else()
	set( OS_LIBS "" )
endif()

target_link_libraries(
	umnet
	umka
	mlibc
	${OS_LIBS}
)
set_property(
	TARGET umnet
	PROPERTY C_STANDARD 11
)
