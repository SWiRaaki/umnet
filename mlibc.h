#ifndef MLIBC_H
#define MLIBC_H

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <time.h>

/* *********************** */
/* **** MLIBC General **** */
/* *********************** */

#define MLIBC_NONULL( ptr, nonull ) (ptr ? ptr : nonull)

// Detect compiler
#if defined( _MSC_VER )
	/// @brief Macro indication that MSVC is the currently used compiler
#	define MLIBC_COMPILER_MSVC
	/// @brief Comüiler used to compile the current code: Microsoft Visual C/C++
#	define MLIBC_COMPILER "MSVC"
	/// @brief Integer representation of the used compiler
#	define MLIBC_COMPILER_VERSION _MSC_VER
	/// @brief Macro to define byte alignment
#	define MLIBC_ALIGNED( alignment ) __declspec( align( alignment ) )
#elif defined( __clang__ )
	/// @brief Macro indication that Clang is the currently used compiler
#	define MLIBC_COMPILER_CLANG
	/// @brief Comüiler used to compile the current code: Clang
#	define MLIBC_COMPILER "Clang"
	/// @brief Integer representation of the used compiler
#	define MLIBC_COMPILER_VERSION (__clang_major__ * 100 + __clang_minor__)
	/// @brief Macro to define byte alignment
#	define MLIBC_ALIGNED( alignment ) __attribute__(( aligned( alignment ) ))
#elif defined( __GNUC__ ) || defined( __GNUG__ )
#	if defined(__MINGW32__)
		/// @brief Macro indication that MinGW is the currently used compiler
#		define MLIBC_COMPILER_MINGW
		/// @brief Comüiler used to compile the current code: gcc/g++ [MinGW]
#		define MLIBC_COMPILER "MinGW"
#	else
		/// @brief Macro indication that GCC is the currently used compiler
#		define MLIBC_COMPILER_GCC
		/// @brief Comüiler used to compile the current code: gcc/g++
#		define MLIBC_COMPILER "GCC"
#	endif
	/// @brief Integer representation of the used compiler
#	define MLIBC_COMPILER_VERSION ( __GNUC__ * 100 + __GNUC_MINOR__ )
	/// @brief Macro to define byte alignment
#	define MLIBC_ALIGNED( alignment ) __attribute__(( aligned( alignment ) ))
#elif defined( __INTEL_COMPILER )
	/// @brief Macro indication that Intel is the currently used compiler
#	define MLIBC_COMPILER_INTEL
	/// @brief Comüiler used to compile the current code: Intel c++
#	define MLIBC_COMPILER "Intel"
	/// @brief Integer representation of the used compiler
#	define MLIBC_COMPILER_VERSION __INTEL_COMPILER
	/// @brief Macro to define byte alignment
#	define MLIBC_ALIGNED( alignment ) __attribute__(( aligned( alignment ) ))
#else
	/// @brief Macro indication that no (to MLib) known compiler is the currently used compiler
#	define MLIBC_COMPILER_UNKNOWN
	/// @brief Comüiler used to compile the current code: unknown
#	define MLIBC_COMPILER "Unknown"
	/// @brief Integer representation of the used compiler
#	define MLIBC_COMPILER_VERSION 0
	/// @brief Macro to define byte alignment
#	define MLIBC_ALIGNED( alignment )
#endif

//      Native       Short    Long     Named
/// @brief Signed 8-Bit integer, a signed byte
typedef int8_t       i8,      Int8,       SByte;
/// @brief Signed 16-Bit integer, a short
typedef int16_t      i16,     Int16,      Short;
/// @brief Signed 32-Bit integer, an int
typedef int32_t      i32,     Int32,      Int;
/// @brief Signed 64-Bit integer, a long
typedef int64_t      i64,     Int64,      Long;

/// @brief Unsigned 8-Bit integer, a byte
typedef uint8_t      u8,      UInt8,      Byte;
/// @brief Unsigned 16-Bit integer, a word
typedef uint16_t     u16,     UInt16,     Word;
/// @brief Unsigned 32-Bit integer, a double word
typedef uint32_t     u32,     UInt32,     DWord;
/// @brief Unsigned 64-Bit integer, a quad word
typedef uint64_t     u64,     UInt64,     QWord;

/// @brief 32-Bit floating point number, a single precision fp
typedef float        f32,     Float32,    Single;
/// @brief 64-Bit floating point number, a double precision fp
typedef double       f64,     Float64,    Double;

/// @brief Raw c-style pointer
typedef void       * ptr,   * Pointer,  * RawPointer;
/// @brief Raw c-style readonly pointer
typedef void const * cptr,  * CPointer, * RawConstPointer;

//      Native         Short     Long           Named
/// @brief 8-bit unsigned integer representing a utf8 character
typedef uint_least8_t  c8,       Char8,         Utf8Char;
/// @brief 16-bit unsigned integer representing a utf16 character
typedef uint_least16_t c16,      Char16,        Utf16Char;
/// @brief 32-bit unsigned integer representing a utf32 character
typedef uint_least32_t c32,      Char32,        Utf32Char;

/// @brief Raw c-style string pointer
typedef char	     * str,    * RString,     * RawString;
/// @brief Raw c-style readonly string pointer
typedef char const   * cstr,   * RCString,    * RawConstString;
/// @brief Raw c-style utf8 string pointer
typedef Char8        * str8,   * RStringU8,   * RawStringUtf8;
/// @brief Raw c-style readonly utf8 string pointer
typedef Char8 const  * cstr8,  * RCStringU8,  * RawConstStringUtf8;
/// @brief Raw c-style utf16 string pointer
typedef Char16       * str16,  * RStringU16,  * RawStringUtf16;
/// @brief Raw c-style readonly utf16 string pointer
typedef Char16 const * cstr16, * RCStringU16, * RawConstStringUtf16;
/// @brief Raw c-style utf32 string pointer
typedef Char32       * str32,  * RStringU32,  * RawStringUtf32;
/// @brief Raw c-style readonly utf32 string pointer
typedef Char32 const * cstr32, * RCStringU32, * RawConstStringUtf32;

/* ************************** */
/* **** MLIBC Versioning **** */
/* ************************** */

/// @brief Versioning structure that follows semantic versioning version 2.0
/// @details @see https://semver.org/
typedef struct SemVersion SemVersion;

struct SemVersion {
	/// @brief Major version, also known as 'main version'
	u64  Major;
	/// @brief Minor version, also known as 'subversion'
	u64  Minor;
	/// @brief Patch of the version
	u64  Patch;
	/// @brief String representation of the prerelease-version, prefixed with '-' in 'Full'
	cstr Prerelease;
	/// @brief String representation of the build type, prefixed with '+' in 'Full'
	cstr Build;
	/// @brief String representation of the full version including prerelease and build, if given
	cstr Full;
};

/// @brief Readonly mlibc version
extern SemVersion const mlibc_version;

/********************************/
/***** MLIBC Returnhandling *****/
/********************************/

typedef struct {
	i64  Code;
	char Message[256];
} Result;

/********************************/
/***** MLIBC Identification *****/
/********************************/

/*
typedef enum {
	UuidVariantNone  = 0,
	UuidVariantNcs1 = 1,
	UuidVariantNcs2 = 2,
	UuidVariantNcs3 = 3,
	UuidVariantNcs4 = 4,
	UuidVariantNcs5 = 5,
	UuidVariantNcs6 = 6,
	UuidVariantNcs7 = 7,
	UuidVariant1    = 8,
	UuidVariant2    = 9,
	UuidVariant3    = 10,
	UuidVariant4    = 11,
	UuidVariantMsc1 = 12,
	UuidVariantMsc2 = 13,
	UuidVariantRes1 = 14,
	UuidVariantRes2 = 15
} UuidVariant;

typedef enum {
	UuidVersionUnused      = 0,
	UuidVersionGregorian   = 1,
	UuidVersionDceReserved = 2,
	UuidVersionNamedMd5    = 3,
	UuidVersionPseudoRng   = 4,
	UuidVersionNamedSha1   = 5
} UuidVersion;

typedef union {
	u8  Octets[16];
	struct {
		u8 OctetGroup1[4];
		u8 OctetGroup2[2];
		u8 OctetGroup3[2];
		u8 OctedGroup4[2];
		u8 OctetGroup5[6];
	} Segments;
	struct {

	} V1;
	struct {

	} V2;
	struct {

	} V3;
	struct {
		
	} V4;
	struct {

	} V5;
	struct {

	} V6;
	struct {

	} V7;
	struct {

	} V8;
} Uuid;

extern Uuid const NilUuid;
extern Uuid const MaxUuid;

*/

/* ********************** */
/* **** MLIBC Memory **** */
/* ********************** */

/// @brief Factor to calculate kilobytes to bytes
#define MLIBC_KB_FACTOR (UINT64_C(1024))
/// @brief Factor to calculate megabytes to bytes
#define MLIBC_MB_FACTOR (UINT64_C(1024) * UINT64_C(1024))
/// @brief Factor to calculate gigabytes to bytes
#define MLIBC_GB_FACTOR (UINT64_C(1024)* UINT64_C(1024) * UINT64_C(1024))
/// @brief Factor to calculate terabytes to bytes
#define MLIBC_TB_FACTOR (UINT64_C(1024) * UINT64_C(1024) * UINT64_C(1024) * UINT64_C(1024))

#ifndef MLIBC_ALIGNMENT_CACHELINE
/// @brief Size of the cache line alignment (bytes in one cache bucket)
#define MLIBC_ALIGNMENT_CACHELINE 64
#endif
#ifndef MLIBC_ALIGNMENT_CACHEL1
/// @brief Size of the expected L1 cache in bytes
#define MLIBC_ALIGNMENT_CACHEL1 (16 * MLIBC_KB_FACTOR);
#endif

/// @brief Number of bits in one bitslot object
#define SLOT_BITS ((bitslot_t)sizeof( bitslot_t ) * 8)

/// @brief Policy that disables all Zero setting
#define MLIBC_ZERO_POLICY_MANUAL     0x00000000
/// @brief Policy that zeroes out new objects on functions that support it
#define MLIBC_ZERO_POLICY_ONALLOCATE 0x00000001
/// @brief Policy that zeroes out deleted objects on functions that support it
#define MLIBC_ZERO_POLICY_ONRELEASE  0x00000002
/// @brief Policy that enables all zeroing on functions that support it
#define MLIBC_ZERO_POLICY_ALL        0x7FFFFFFF

#ifndef MLIBC_ZERO_POLICY
#define MLIBC_ZERO_POLICY MLIBC_ZERO_POLICY_ONRELEASE
#endif

/// @brief Policy that disables all alignment policies
#define MLIBC_ALIGNMENT_POLICY_NONE   0x00000000
/// @brief Policy that enables memory alignment on structures
#define MLIBC_ALIGNMENT_POLICY_STRUCT 0x00000001
/// @brief Policy that enables memory alignment on stack memory allocations
#define MLIBC_ALIGNMENT_POLICY_STACK  0x00000002
/// @brief Policy that enables memory alignment on heap memory allocations
#define MLIBC_ALIGNMENT_POLICY_HEAP   0x00000004
/// @brief Policy that enables all alignment policies
#define MLIBC_ALIGNMENT_POLICY_ALL    0x7FFFFFFF

#ifndef MLIBC_ALIGNMENT_POLICY
#define MLIBC_ALIGNMENT_POLICY MLIBC_ALIGNMENT_POLICY_ALL
#endif

#if MLIBC_ALIGNMENT_POLICY & MLIBC_ALIGNMENT_POLICY_STRUCT
/// @brief Struct that is cacheline aligned if the policy is set
#define MLIBC_STRUCT struct MLIBC_ALIGNED( MLIBC_ALIGNMENT_CACHELINE )
#else
/// @brief Struct that is cacheline aligned if the policy is set
#define MLIBC_STRUCT struct
#endif

/// @brief Type for objects to contain bitslots
typedef unsigned int bitslot_t;

/// @brief Memory Pool state structure
typedef MLIBC_STRUCT MemoryPool MemoryPool;
/// @brief Memory Arena state structure
typedef MLIBC_STRUCT MemoryArena MemoryArena;

/**
 * @brief Function pointer for memory allocation functions
 * @param element_size	Size in bytes for a single element
 * @param capacity		Number of objects to preallocate
 * @return ptr			Pointer to the allocated memory block
 */
typedef ptr (*AllocateFct)( u64 const element_size, u64 const capacity );

/**
 * @brief Function pointer for memory deallocation functions
 * @param memory	Pointer to the allocated memory block
 */
typedef void (*ReleaseFct)( ptr const memory );

struct MemoryPool {
	// @brief Number of slots used in this pool
	u64 Used;
	// @brief Active cursor for searching available slots
	u64 Cursor;
	// @brief Anonymous List of slot states( USED/UNUSED )
	ptr List;
	// @brief Raw chunk of memory owned by this pool
	ptr Raw;
	// @brief Size of the element type managed by this pool in bytes
	u64 ElementSize;
	// @brief Maximum number of elements managable by this pool
	u64 Capacity;
	// @brief Pointer to a function that will be used to deallocate the memory block
	ReleaseFct Release;
};

struct MemoryArena {
	// @brief Number of bytes used in this arena
	u64 Used;
	// @brief Raw chunk of memory owned by this pool
	ptr Raw;
	// @brief Maximum number of bytes managable by this arena
	u64 Capacity;
	// @brief Pointer to a function that will be used to deallocate the memory block
	ReleaseFct Release;
};

/**
 * @brief Aligns an integer to the cacheline boundaries
 * @param unaligned	Integer to align
 * @return u64		Aligned integer
 */
static inline u64 Align( u64 const unaligned ) {
	return unaligned + (MLIBC_ALIGNMENT_CACHELINE - unaligned % MLIBC_ALIGNMENT_CACHELINE);
}

/**
 * @brief Creates a bitmask with one bit set at the specified position
 * @param bit			Position of the bit that needs to be set
 * @return bitslot_t	Bitmask with a single bit set
 */
static inline bitslot_t BitMask( u32 const bit ) {
	return 1 << ( bit % SLOT_BITS );
}

/**
 * @brief Calculates the index of the bitslot holding the specified bit
 * @param bit			Position of the bit to find the bitslot
 * @return bitslot_t	Bitslot that contains the bit
 */
static inline bitslot_t BitSlot( u32 const bit ) {
	return bit / SLOT_BITS;
}

/**
 * @brief Sets a bit in a sequential bit array
 * @param bit	Bit to set
 * @param set	Pointer to the bit array
 */
static inline void BitSet( bitslot_t * const set, u32 const bit ) {
	set[BitSlot( bit )] |= BitMask( bit );
}

/**
 * @brief Clears a bit in a sequential bit array
 * @param bit	Bit to clear
 * @param set	Pointer to the bit array
 */
static inline void BitClear( bitslot_t * const set, u32 const bit ) {
	set[BitSlot( bit )] &= ~BitMask( bit );
}

/**
 * @brief Checks if a bit is set in a sequential bit array
 * @param bit	Bit to check
 * @param set	Pointer to the bit array
 * @return int	0 if the bit is not set, otherwise 1
 */
static inline int BitTest( bitslot_t * const set, u32 const bit ) {
	return (int)( set[BitSlot( bit )] & BitMask( bit ) );
}

/**
 * @brief Calculates the required number of bitslot objects needed to hold the specified number of bits
 * @param bits	Number of bits that need to be stored
 * @return u32	Number of bitslot objects needed
 */
static inline u32 BitSlots( u32 const bits ) {
	return (bits + SLOT_BITS - 1) / SLOT_BITS;
}

/**
 * @brief Calculates kilobytes to bytes
 * @param kilobytes	kilobytes to convert
 * @return u64		bytes needed to represent given kilobytes
 */
static inline u64 KBytesToBytes( u64 const kilobytes ) {
	return kilobytes * MLIBC_KB_FACTOR;
}

/**
 * @brief Calculates kilobytes to bytes
 * @param kilobytes	kilobytes to convert
 * @return u64		bytes needed to represent given kilobytes
 */
static inline u64 KBytesToBytesF( double const kilobytes ) {
	return (u64)(kilobytes * MLIBC_KB_FACTOR);
}

/**
 * @brief Calculates megabytes to bytes
 * @param megabytes	megabytes to convert
 * @return u64		bytes needed to represent given megabytes
 */
static inline u64 MBytesToBytes( u64 const megabytes ) {
	return megabytes * MLIBC_MB_FACTOR;
}

/**
 * @brief Calculates megabytes to bytes
 * @param megabytes	megabytes to convert
 * @return u64		bytes needed to represent given megabytes
 */
static inline u64 MBytesToBytesF( double const megabytes ) {
	return (u64)(megabytes * MLIBC_MB_FACTOR);
}

/**
 * @brief Calculates gigabytes to bytes
 * @param gigabytes gigabytes to convert
 * @return u64		bytes needed to represent given gigabytes
 */
static inline u64 GBytesToBytes( u64 const gigabytes ) {
	return gigabytes * MLIBC_GB_FACTOR;
}

/**
 * @brief Calculates gigabytes to bytes
 * @param gigabytes gigabytes to convert
 * @return u64		bytes needed to represent given gigabytes
 */
static inline u64 GBytesToBytesF( double const gigabytes ) {
	return (u64)(gigabytes * MLIBC_GB_FACTOR);
}

/**
 * @brief Calculates terabytes to bytes
 * @param terabytes terabytes to convert
 * @return u64		bytes needed to represent given terabytes
 */
static inline u64 TBytesToBytes( u64 const terabytes ) {
	return terabytes > 16777215 ? UINT64_MAX : terabytes * MLIBC_TB_FACTOR;
}

/**
 * @brief Calculates terabytes to bytes
 * @param terabytes terabytes to convert
 * @return u64		bytes needed to represent given terabytes
 */
static inline u64 TBytesToBytesF( double const terabytes ) {
	return (u64)(terabytes * MLIBC_TB_FACTOR);
}

/**
 * @brief Allocates a chunk of memory
 * @details This function respects the alignment and zero policy
 * @param element_size	Size in bytes of the object
 * @param capacity		Number of objects with given size should fit
 */
ptr Allocate( u64 const element_size, u64 const capacity );

void Deallocate( ptr const memory );

void Copy( ptr const destination, cptr const source, u64 const element_size, u64 const count );

/**
 * @brief Creates a pool for elements of given size
 * @param element_size	Size of the object types in bytes
 * @param capacity		Maximum number of objects managable
 * @return MemoryPool	Clean state of the pool
 */
MemoryPool PoolCreate( u64 const element_size, u64 const capacity );

/**
 * @brief Creates a pool for elements of given size
 * @param element_size	Size of the object types in bytes
 * @param capacity		Maximum number of objects managable
 * @param allocate_fct	Pointer to the function to preallocate the pool memory
 * @param release_fct	Pointer to the function to release the pool memory
 * @return MemoryPool	Clean state of the pool
 */
MemoryPool PoolCreateEx( u64 const element_size, u64 const capacity, AllocateFct const allocate_fct, ReleaseFct const release_fct );

/**
 * @brief Releases all allocated resources of the pool to the operating system and invalidates the state
 * @param pool	Memory pool to release and invalidate
 */
void PoolDestroy( MemoryPool * const pool );

/**
 * @brief Allocates an object owned by the pool
 * @param pool		Memory pool to own and manage the object
 * @return void *	Pointer to the object allocated
 */
void * PoolAllocate( MemoryPool * const pool );

/**
 * @brief Releases the resources of an object if it is managed by the pool
 * @param pool		Owner of the object
 * @param element	Pointer to the element managed by the pool
 */
void PoolRelease( MemoryPool * const pool, ptr const element );

/**
 * @brief Releases all resources of the pool without releasing the resources to the os or invalidate it
 * @param pool	Memory pool to reset
 */
void PoolReset( MemoryPool * const pool );

/**
 * @brief Receive the number of slots used by the specified pool
 * @param pool		Memory Pool to check
 * @return u64	Number of allocated object slots
 */
static inline u64 PoolSlotsInUse( MemoryPool * const pool ) {
	return pool->Used;
}

/**
 * @brief Receive the number of slots available in the specified pool
 * @param pool		Memory Pool to check
 * @return u64	Number of unallocated object slots
 */
static inline u64 PoolSlotsAvailable( MemoryPool * const pool ) {
	return pool->Capacity - pool->Used;
}

/**
 * @brief Creates an arena of given number of bytes
 * @param capacity		Number of bytes managed by this arena
 * @return MemoryArena	Clean state of the arena
 */
MemoryArena ArenaCreate( u64 const capacity );

/**
 * @brief Creates an arena of given number of bytes
 * @param capacity		Number of bytes managed by this arena
 * @param allocate_fct	Pointer to the function to preallocate the arena memory
 * @param release_fct	Pointer to the function to release the arena memory
 * @return MemoryArena	Clean state of the arena
 */
MemoryArena ArenaCreateEx( u64 const capacity, AllocateFct const allocate_fct, ReleaseFct const release_fct );

/**
 * @brief Releases all allocated resources of the arena to the operating system and invalidates the state
 * @param arena	Memory arena to release and invalidate
 */
void ArenaDestroy( MemoryArena * const arena );

/**
 * @brief Allocates an object owned by the arena
 * @param arena		Memory pool to own and manage the object
 * @param size		Number of bytes needed for the object
 * @return void *	Pointer to the object allocated
 */
void * ArenaAllocate( MemoryArena * const arena, u64 const size );

/**
 * @brief Releases all resources of the arena without releasing the resources to the os or invalidate it
 * @param arena	Memory arena to reset
 */
void ArenaReset( MemoryArena * const arena );

/**
 * @brief Receive the number of bytes used by the specified arena
 * @param arena		Memory arena to check
 * @return u64	Number of allocated bytes
 */
static inline u64 ArenaBytesInUse( MemoryArena * const arena ) {
	return arena->Used;
}

/**
 * @brief Receive the number of bytes available in the specified arena
 * @param arena		Memory arena to check
 * @return u64	Number of available bytes
 */
static inline u64 ArenaBytesAvailable( MemoryArena * const arena ) {
	return arena->Capacity - arena->Used;
}

/* *********************** */
/* **** MLIBC Logging **** */
/* *********************** */

/// @brief Enumeration of all std log level flags
typedef enum {
	/// @brief Flag for the standard log level
	LogFlagInfo		= 0x00000001,
	/// @brief Flag for the warning log level
	LogFlagWarning	= 0X00000002,
	/// @brief Flag for the error log level
	LogFlagError	= 0X00000004,
	/// @brief Flag for the fatal error log level
	LogFlagFatal	= 0x00000008,
	/// @brief Flag for the debug log level
	LogFlagDebug	= 0x00000010,
	/// @brief Flag for all log levels excluding debug logs
	LogFlagAllNoDbg	= 0x7FFFFFEF,
	/// @brief Flag for all log levels including debug logs
	LogFlagAll		= 0x7FFFFFFF
} LogLevelFlag;

// @brief Enumeration of all std log levels
typedef enum {
	/// @brief Default standard log level -> stdout
	LogInfo    = 1,
	/// @brief Log level for unexpected states but not process-critical -> stdout
	LogWarning = 2,
	/// @brief Default error log level -> stderr
	LogError   = 3,
	/// @brief Critical error log level that should stop the process -> stderr
	LogFatal   = 4,
	/// @brief Default debug log level -> stdout
	LogDebug   = 5
} LogLevel;

typedef enum {
	LogColorBlack         = 0,
	LogColorRed           = 1,
	LogColorGreen         = 2,
	LogColorYellow        = 3,
	LogColorBlue          = 4,
	LogColorMagenta       = 5,
	LogColorCyan          = 6,
	LogColorWhite         = 7,
	LogColorDefault       = 9,
	LogColorBrightBlack   = 10,
	LogColorBrightRed     = 11,
	LogColorBrightGreen   = 12,
	LogColorBrightYellow  = 13,
	LogColorBrightBlue    = 14,
	LogColorBrightMagenta = 15,
	LogColorBrightCyan    = 16,
	LogColorBrightWhite   = 17
} LogColor;

/// @brief Log header for modules to identify themselfes
typedef MLIBC_STRUCT Logger Logger;
/// @brief Log descriptor on how the log line should be formatted
typedef MLIBC_STRUCT LogDescriptor LogDescriptor;

/**
 * @brief Function pointer that formats and prints a log line
 * @param module	Log header of the module that prints the log
 * @param desc		Log descriptor that describes the formatting
 * @param message	Log message format string
 * @param args		Variable argument list
 */
typedef void (*PrintFn)( Logger const * const module, LogDescriptor const * const desc, cstr const message, va_list args );

struct Logger {
	/// @brief Buffer used by the logger to format strings
	ptr				Buffer;
	/// @brief Format string that provides how to format the logging time stamp
	cstr			TimeFormat;
	/// @brief Prefix string that identifies the logger of a log
	cstr			Prefix;
	/// @brief Bitflag of logs that are enabled or disabled ( bit set = enabled )
	LogLevelFlag	Level;
	/// @brief Maximum length of a single log line
	u32				Length;
};

struct LogDescriptor {
	/// @brief Prefix that identifies the log level
	cstr			Prefix;
	/// @brief Pointer to the function that formats and prints a log line
	PrintFn			PrintFn;
	/// @brief Foreground console color
	LogColor		Foreground;
	/// @brief Background console color
	LogColor		Background;
	/// @brief Log level assigned on registration
	LogLevel		Level;
	/// @brief Log level bitmask representing the assigned log level
	LogLevelFlag	LevelBit;
};

/**
 * @brief Registers a log level via LogDescriptor
 * @param log		Log descriptive meta information.
 * @return uint32_t	Level assigned to the log. If a log level is set, it will be used if available
 */
LogLevel RegisterLogLevel( LogDescriptor * const log );

/**
 * @brief Formats and prints a log line
 * @param module	Log header of the module that prints the log
 * @param desc		Log descriptor that describes the formatting
 * @param message	Log message format string
 * @param args		Variable argument list
 */
void Print( Logger const * const module, LogDescriptor const * const desc, cstr const message, va_list args );

/**
 * @brief Sets the font color of the terminal text.
 * @param color	Base color
 * @param level Brightness level of the color
 */
void SetPrintForeColor( LogColor const color );

/**
 * @brief Sets the background color of the terminal text.
 * @param color	Base color
 * @param level Brightness level of the color
 */
void SetPrintBackColor( LogColor const color );

/**
 * @brief Resets the color of the terminal text to all default
 */
void ResetPrintColor( void );

/**
* @brief Get a readonly pointer to the global logger module
 * @return Logger const* Readonly pointer to the global logger
 */
Logger const * LogGetGlobal( void );

/**
 * @brief Set the log level of the global logger
 * @param level	Bitwise OR'd log level flags
 */
void LogSetLevel( LogLevelFlag const level );

/**
 * @brief Set the maximum log line length of the global logger
 * @param length	Maximum length a log line can be
 */
void LogSetLength( u32 const length );

/**
 * @brief Set the timestamp format of the global logger
 * @param time_format	Format string conforming the strftime format
 */
void LogSetTimeFormat( cstr const time_format );

/**
 * @brief Set the module prefix of the global logger
 * @param prefix	Module prefix string
 */
void LogSetPrefix( cstr const prefix );

/**
 * @brief Print a log of a specifig log level
 * @param level		Level to print the log to
 * @param message	Message to format
 * @param ...		List of parameters for the string format
 */
void Log( LogLevel const level, cstr const message, ... );

/**
 * @brief Print a log of a specifig log level
 * @param level		Level to print the log to
 * @param message	Message to format
 * @param args		Variadic list of parameters as for the string format
 */
void LogV( LogLevel const level, cstr const message, va_list args );

/**
 * @brief Creates a new Logger state
 * @param level			The initial log level of the logger
 * @param length		Maximum length of a single log line
 * @param time_format	[OPTIONAL] Format string that provides how to format the logging time stamp
 * @param prefix		[OPTIONAL] Prefix string that identifies the logger of a log
 * @return Logger		Logger state
 */
Logger LoggerCreate( LogLevelFlag const level, u32 const length, cstr const time_format, cstr const prefix );

/**
 * @brief Set the log level of the global logger
 * @param module	Pointer to the logger module
 * @param level	Bitwise OR'd log levels
 */
void LogSetLevelEx( Logger * const module, LogLevelFlag const level );

/**
 * @brief Set the maximum log line length of the global logger
 * @param module	Pointer to the logger module
 * @param length	Maximum length a log line can be
 */
void LogSetLengthEx( Logger * const module, u32 const length );

/**
 * @brief Set the timestamp format of the global logger
 * @param module	Pointer to the logger module
 * @param time_format	Format string conforming the strftime format
 */
void LogSetTimeFormatEx( Logger * const module, cstr const time_format );

/**
 * @brief Set the module prefix of the global logger
 * @param module	Pointer to the logger module
 * @param prefix	Module prefix string
 */
void LogSetPrefixEx( Logger * const module, cstr const prefix );

/**
 * @brief Print a log of a specifig log level
 * @param module	Pointer to the logger module
 * @param level		Level to print the log to
 * @param message	Message to format
 * @param ...		List of parameters for the string format
 */
void LogEx( Logger const * const module, LogLevel const level, cstr const message, ... );

/**
 * @brief Print a log of a specifig log level
 * @param module	Pointer to the logger module
 * @param level		Level to print the log to
 * @param message	Message to format
 * @param args		Variadic list of parameters for the string format
 */
void LogExV( Logger const * const module, LogLevel const level, cstr const message, va_list args );

/**
 * @brief Default log print
 * @param message	Message format string
 * @param ...		List of parameters for the string format
 */
static inline void Info( char const * const message, ... ) {
	va_list args;
	va_start( args, message );
	LogV( LogInfo, message, args );
	va_end( args );
}

/**
 * @brief Default log print
 * @param module	Pointer to the logger module
 * @param message	Message format string 
 * @param ...		List of parameters for the string format
 */
static inline void InfoEx( Logger const * const logger, char const * const message, ... ) {
	va_list args;
	va_start( args, message );
	LogExV( logger, LogInfo, message, args );
	va_end( args );
}

/**
 * @brief Warning log prints, color highlighted, indicating possible error sources.
 * @param message	Message format string 
 * @param ...		List of parameters for the string format
 */
static inline void Warning( char const * const message, ... ) {
	va_list args;
	va_start( args, message );
	LogV( LogWarning, message, args );
	va_end( args );
}

/**
 * @brief Warning log prints, color highlighted, indicating possible error sources.
 * @param module	Pointer to the logger module
 * @param message	Message format string 
 * @param ...		List of parameters for the string format
 */
static inline void WarningEx( Logger const * const logger, char const * const message, ... ) {
	va_list args;
	va_start( args, message );
	LogExV( logger, LogWarning, message, args );
	va_end( args );
}

/**
 * @brief Error log prints, color highlighted, indicating a non-fatal error
 * @param message	Message format string 
 * @param ...		List of parameters for the string format
 */
static inline void Error( char const * const message, ... ) {
	va_list args;
	va_start( args, message );
	LogV( LogError, message, args );
	va_end( args );
}

/**
 * @brief Error log prints, color highlighted, indicating a non-fatal error
 * @param module	Pointer to the logger module
 * @param message	Message format string 
 * @param ...		List of parameters for the string format
 */
static inline void ErrorEx( Logger const * const logger, char const * const message, ... ) {
	va_list args;
	va_start( args, message );
	LogExV( logger, LogError, message, args );
	va_end( args );
}

/**
 * @brief Fatal error log prints, color highlighted, indicating a fatal error
 * @param message	Message format string 
 * @param ...		List of parameters for the string format
 */
static inline void Fatal( char const * const message, ... ) {
	va_list args;
	va_start( args, message );
	LogV( LogFatal, message, args );
	va_end( args );
}

/**
 * @brief Fatal error log prints, color highlighted, indicating a fatal error
 * @param module	Pointer to the logger module
 * @param message	Message format string 
 * @param ...		List of parameters for the string format
 */
static inline void FatalEx( Logger const * const logger, char const * const message, ... ) {
	va_list args;
	va_start( args, message );
	LogExV( logger, LogFatal, message, args );
	va_end( args );
}

/**
 * @brief Debug log prints, color highlighted, only activated in debug builds unless manually activated
 * @param message	Message format string 
 * @param ...		List of parameters for the string format
 */
static inline void Debug( char const * const message, ... ) {
	va_list args;
	va_start( args, message );
	LogV( LogDebug, message, args );
	va_end( args );
}

/**
 * @brief Debug log prints, color highlighted, only activated in debug builds unless manually activated
 * @param module	Pointer to the logger module
 * @param message	Message format string 
 * @param ...		List of parameters for the string format
 */
static inline void DebugEx( Logger const * const logger, char const * const message, ... ) {
	va_list args;
	va_start( args, message );
	LogExV( logger, LogDebug, message, args );
	va_end( args );
}

/* ********************** */
/* **** MLIBC Timing **** */
/* ********************** */

typedef u64 TimeStamp;
typedef MLIBC_STRUCT DateTime DateTime;

struct DateTime {
	/// @brief Nanosecond part of the represented time
	u32  NanoSecond;
	/// @brief Microsecond part of the represented time
	u32  MicroSecond;
	/// @brief Millisecond part of the represented time
	u32  MilliSecond;
	/// @brief Second part of the represented time
	u32  Second;
	/// @brief Minute part of the represented time
	u32  Minute;
	/// @brief Hour part of the represented time
	u32  Hour;
	/// @brief Day of the month of the represented date
	u32  Day;
	/// @brief Day of the week of the represented date
	u32  WeekDay;
	/// @brief Day of the year of the represented date
	u32  YearDay;
	/// @brief Month of the represented date
	u32  Month;
	/// @brief Year of the represented date
	u32  Year;
	/// @brief UTC Time zone of the represented date and time
	cstr UTC;
	/// @brief Daylight summer time offset
	u32  DST;
};

/**
 * @brief Get the current date and time
 * return DateTime	Current date and time
 */
void DateTimeCurrent( DateTime * const out_datetime );

/**
 * @brief Get the current date
 * return DateTime	Current date
 */
void DateTimeToday( DateTime * const out_datetime );

/**
 * @brief Get the current time
 * return DateTime	Current time
 */
void DateTimeNow( DateTime * const out_datetime );

/**
 * @brief Get a high precision date independant time stamp in nanoseconds
 * return TimeStamp	Time stamp in nanoseconds
 */
TimeStamp TimeStampNow( void );

/**
 * @brief Formats date and time into a string buffer given by the specified datetime format
 * @details For a more detailed documentation visit github.com/SWiRaaki/mlibc
 * @param out_buffer	Output string buffer
 * @param buffer_size	Number of bytes available in the output buffer
 * @param format		Format string
 * @param datetime		Date and time to format
 */
u64 DateTimeFormat( str const out_buffer, u64 const buffer_size, cstr const format, DateTime const datetime );

/* *************************** */
/* **** MLIBC Collections **** */
/* *************************** */

/// @brief Node of a linked list
typedef MLIBC_STRUCT ListNode ListNode;
/// @brief Linked list
typedef MLIBC_STRUCT List List;
/// @brief Dynamic array
//typedef MLIBC_STRUCT ArrayList ArrayList;
/// @brief Dynamic array with an upper limit
//typedef MLIBC_STRUCT CappedList CappedList;



typedef int (*ListIteratorFn)( u64 const position, ptr const item, cptr const params );

struct ListNode {
	/// @brief Reference to the previous node
	ListNode * Previous;
	/// @brief Reference to the next node 
	ListNode * Next;
	/// @brief Item pointer
	ptr        Item;
	/// @brief Optional pointer to meta-information
	ptr        Meta;
};

struct List {
	/// @brief Reference to the first node in the list
	ListNode * Head;
	/// @brief Reference to the last node in the list
	ListNode * Tail;
	/// @brief Optional pointer to meta-information
	ptr        Meta;
	/// @brief Number of items currently in the list
	u64        Count;
};

/**
 * @brief Create a new empty list
 * @return List	New and empty list
 */
List ListCreate( void );

/**
 * @brief Destroys a list and frees its resources
 * @param list	Pointer to the list to destroy
 */
void ListDestroy( List * const list );

/**
 * @brief Adds a new item to the end of the list
 * @param list	Pointer to the list to add to
 * @param item	Item to add
 */
void ListAppend( List * const list, ptr const item );

/**
 * @brief Adds a new item with meta information to the end of the list
 * @param list	Pointer to the list to add to
 * @param item	Item to add
 * @param meta	Meta-information of the item
 */
void ListAppendEx( List * const list, ptr const, ptr const meta );

/**
 * @brief Adds a new item at the given position, moving all items starting from the position one up
 * @param list		Pointer to the list to add to
 * @param item		Item to add
 * @param position	Position to insert to
 */
void ListInsert( List * const list, ptr const item, u64 const position );

/**
 * @brief Adds a new item at the given position, moving all items starting from the position one up
 * @param list		Pointer to the list to add to
 * @param item		Item to add
 * @param meta		Meta-information of the item
 * @param position	Position to insert to
 */
void ListInsertEx( List * const list, ptr const item, ptr const meta, u64 const position );

/**
 * @brief Adds a new item with meta information to the start of the list
 * @param list	Pointer to the list to add to
 * @param item	Item to add
 */
void ListPrepend( List * const list, ptr const item );

/**
 * @brief Adds a new item with meta information to the start of the list
 * @param list	Pointer to the list to add to
 * @param item	Item to add
 * @param meta	Meta-information of the item
 */
void ListPrependEx( List * const list, ptr const item, ptr const meta );

/**
 * @brief Searches for an item and removes the first occurance of that item from the list and returns it to the caller
 * @param list		Pointer to the list to remove from
 * @param position	Position to remove from
 * @return ptr		Item removed from the list or NULL
 */
ptr ListReleaseAt( List * const list, u64 const position );

/**
 * @brief Searches for the item with a filter and releases it
 * @param list	Pointer to the list to search from
 * @return u64	Index of the item or UINT64_MAX if not found
 */
ptr ListReleaseFiltered( List * const list, ListIteratorFn const iterator, int const direction, cptr const params );

/**
 * @brief Searches for an item and removes the first occurance of that item from the list
 * @param list	Pointer to the list to remove from
 * @param item	Item to remove
 */
void ListRemove( List * const list, ptr const item );

/**
 * @brief Removes an item at a specific position in the list
 * @param list		Pointer to the list to remove from
 * @param position	Position to remove from
 */
void ListRemoveAt( List * const list, u64 const position );

/**
 * @brief Retrieves a node at a specific position. Mostly relevant for custom list manipulation
 * @param list			Pointer to the list to retrieve from
 * @param position		Position of the nodes item
 * @return ListNode *	Pointer to the items node
 */
ListNode * ListNodeAt( List const * const list, u64 const position );

/**
 * @brief Accesses an item at a specific Position
 * @param list		Pointer to the list to access from
 * @param position	Position of the item to access
 * @return ptr		Item at the position
 */
ptr ListItemAt( List const * const list, u64 const position );

/**
 * @brief Searches for a node that contains a specific item
 * @param list			Pointer to the list to search from
 * @param item			Item to search for
 * @return ListNode *	Pointer to the items node
 */
ListNode * ListNodeFind( List const * const list, cptr const item );

/**
 * @brief Searches for the index of a specific item
 * @param list	Pointer to the list to search from
 * @param item	Item to search for
 * @return u64	Index of the item or UINT64_MAX if not found
 */
u64 ListItemFind( List const * const list, cptr const item );

/**
 * @brief Searches for the index of a specific item and fills the items meta, if found
 * @param list	   Pointer to the list to search from
 * @param item	   Item to search for
 * @param out_meta Pointer that gets filled with the meta object if found
 * @return u64	Index of the item or UINT64_MAX if not found
 */
u64 ListItemFindEx( List const * const list, cptr const item, ptr * const out_meta );

ptr ListItemSearch( List const * const list, ListIteratorFn const iterator, int const direction, cptr const params );

/**
 * @brief Counts all items in the list and, if needed, corrects the Count member
 * @param list	Pointer to the list to count
 * @return u64	Number of items in the list
 */
u64 ListItemCount( List const * const list );

int ListIterate( List const * const list, ListIteratorFn const iterator, int const direction, cptr const params );

/* *********************** */
/* **** MLIBC Strings **** */
/* *********************** */

#define U8(str)   (str8)(str)
#define U8C(str)  (cstr8)(str)

#define U16(str)  (str16)(str)
#define U16C(str) (cstr16)(str)

#define U32(str)  (str32)(str)
#define U32C(str) (cstr32)(str)

/// @brief Cast-Style union for the different encoding strings
typedef union StringBuffer  StringBuffer;
/// @brief Cast-Style union for the different encoding strings
typedef union CStringBuffer CStringBuffer;

/// @brief Stateful string structure with owned memory
typedef MLIBC_STRUCT String String;
/// @brief Stateful readonly string view structure without ownership
typedef MLIBC_STRUCT StringView StringView;
/// @brief Collection of encoding related functions
typedef MLIBC_STRUCT Encoding Encoding;

/**
 * @brief Function pointer for string conversions
 * @param from		Source string
 * @return String	Converted string
 */
typedef String (*ConvertFn)( String const * const from );

/**
 * @brief Function pointer for string copy-creation
 * @param raw		Raw string pointer
 * @return String	Converted string
 */
typedef String (*CreateFn)( cptr const raw );

/**
 * @brief Function pointer for string move-creation
 * @param raw		Raw string pointer
 * @return String	Converted string
 */
typedef String (*MakeFn)( ptr const raw );

/**
 * @brief Function pointer for string move-creation
 * @param raw			Raw string pointer
 * @param out_length	Pointer to the 64-bit integer to receive the number of characters
 * @param out_capacity	Pointer to the 64-bit integer to receive the number of objects
 */
typedef int (*CountFn)( cptr const raw, u64 * const out_length, u64 * const out_size );

/**
 * @brief Function pointer for releasing resources allocated for a string
 * @param string String to release resources from
 */
typedef void (*DestroyFn)( String * const string );

/**
 * @brief Cast-Style union for better and clearer string manipulation
 */
union StringBuffer {
	/// @brief Raw pointer to the string data
	ptr   Raw;
	/// @brief char pointer to the string data, ascii string usage
	str   Ascii;
	/// @brief Char8 pointer to the string data, utf8 string usage
	str8  Utf8;
	/// @brief Char16 pointer to the string data, utf16 string usage
	str16 Utf16;
	/// @brief Char32 pointer to the string data, utf32 string usage
	str32 Utf32;
};

/**
 * @brief Cast-Style union for better and clearer readonly string access
 */
union CStringBuffer {
	/// @brief Raw pointer to the string data
	cptr   Raw;
	/// @brief char pointer to the string data, ascii string usage
	cstr   Ascii;
	/// @brief Char8 pointer to the string data, utf8 string usage
	cstr8  Utf8;
	/// @brief Char16 pointer to the string data, utf16 string usage
	cstr16 Utf16;
	/// @brief Char32 pointer to the string data, utf32 string usage
	cstr32 Utf32;
};

/**
 * @brief Encoding-Aware string with known lenght and size
 */
struct String {
	StringBuffer As;
	u64          Length;
	u64          Size;
	u64          Encoding;
};

/**
 * @brief Encoding-Aware string with known lenght and size, but readonly buffer.
 * @details Practically a const buffer to read through data.
 */
struct StringView {
	CStringBuffer As;
	u64           Length;
	u64           Size;
	u64           Encoding;
};

/**
 * @brief Structure containing properties and functions used to apply that encoding
 */
struct Encoding {
	CreateFn  CreateString;
	MakeFn    MakeString;
	DestroyFn DestroyString;
	CountFn   Count;
	CountFn   CountForSelf;
	CountFn   CountForUtf8;
	ConvertFn FromUtf8;
	ConvertFn ToUtf8;
	cstr8     Name;
	cstr8     Preamble;
	u64       PreambleLength;
	u64       EncodingId;
};

/// @brief Ascii-Encoding
extern Encoding const * const EncodingAscii;
/// @brief UTF-8-Encoding
extern Encoding const * const EncodingUtf8;
/// @brief UTF-16-Encoding using Little Endian
extern Encoding const * const EncodingUtf16Le;
/// @brief UTF-16-Encoding using Big Endian
extern Encoding const * const EncodingUtf16Be;
/// @brief UTF-16-Encoding using the systems endianness
extern Encoding const * const EncodingUtf16;
/// @brief UTF-32-Encoding using Little Endian
extern Encoding const * const EncodingUtf32Le;
/// @brief UTF-32-Encoding using Big Endian
extern Encoding const * const EncodingUtf32Be;
/// @brief UTF-32-Encoding using the systems endianness
extern Encoding const * const EncodingUtf32;
/// @brief Platform dependnt encoding the systems wchar_t encodes
extern Encoding const * const EncodingWString;

/**
 * @brief Register a custom encoding into the list to be able to be autodetected with preamble
 * @param encoding Pointer to the encoding to register
 */
void RegisterEncoding( Encoding * const encoding );

/**
 * @brief Create a new string
 * @param raw Readonly pointer to the raw string
 */
String StringCreate( cptr const raw );

/**
 * @brief Create a string taking ownership of the pointer
 * @param raw Pointer to the raw string
 */
String StringMake( ptr const raw );

/**
 * @brief Free up resources allocated by creating a string and invalidate it
 * @param string Pointer to the string
 */
void StringDestroy( String * const string );

/**
 * @brief Calculate the length and size in bytes from a raw string
 * @param raw Pointer to the raw string
 */
void StringLength( cptr const raw, u64 * const out_length, u64 * const out_size );
//StringView CreateStringView( cptr const raw );

/* *********************** */
/* **** MLIBC Sockets **** */
/* *********************** */

/// @brief Implementation-defined socket descriptor as unsigned 64-bit integer
typedef u64 Socket;
/// @brief Invalid socket descriptor constant
extern Socket const InvalidSocket;

/// @brief IPv4 address type
typedef u32 addr_v4_t;
/// @brief IPv6 address type
typedef u8  addr_v6_t[16];

/// @brief Enumeration of available protocol families for the socket
typedef enum {
	ProtocolFamilyIPv4,
	ProtocolFamilyIPv6
} ProtocolFamily;

/// @brief Enumeration of available socket types for the socket
typedef enum {
	SocketTypeStream,
	SocketTypeDatagram,
	SocketTypeRaw,
	SocketTypeRdm,
	SocketTypeSequencedPacket
} SocketType;


typedef enum {
	SocketChannelRead,
	SocketChannelWrite,
	SocketChannelBoth
} SocketChannel;

/**
 * @brief Meta information of a socket description
 */
typedef MLIBC_STRUCT SocketInfo {
	/// @brief Socket descriptor described in this structure
	Socket         Socket;
	/// @brief Address family specification
	ProtocolFamily Domain;
	/// @brief Socket type specification
	SocketType     Type;
	/// @brief Protocol specificatoin. 0 = Default for domain & type
	i32            Protocol;
	/// @brief Socket port
	u16            Port;
	/// @brief Address union to represent IPv4 OR IPv6 addresses
	union {
		/// @brief IPv4 Address
		addr_v4_t  IPv4;
		/// @brief IPv6 Address
		addr_v6_t  IPv6;
	}              Address;
	/// @brief IPv6 flow info
	u32            FlowInfo;
	/// @brief IPv6 set of interfaces for a scope
	u32            Scope;
} SocketInfo;

/**
 * @brief Initialize os-level resources to use sockets
 * @return Result Result description
 */
Result SocketInitialize( void );

/**
 * @brief Release os-level resources to the os after finishing socket usage
 * @return Result Result description
 */
Result SocketRelease( void );

/**
 * @brief Create a socket endpoint for communication
 * @param info Socket description structure retrieving the socket descriptor
 * @return Result Result description
 */
Result SocketCreate( SocketInfo * const info );

/**
 * @brief Close an open socket
 * @param info Socket description
 * @return Result Result description
 */
Result SocketClose( SocketInfo * const info );

/**
 * @brief Bind a socket to an open address
 * @param socket  Descriptor of the socket to bind
 * @param address Address string
 * @param port    Port string
 * @return Result Result description
 */
Result SocketBind( SocketInfo * const info, cstr const address, cstr const port );

/**
 * @brief Sets the socket into a state where it listens for new connections
 * @param info    Socket description
 * @param backlog Maximum number of connections in the queue
 * @return Result Result description
 */
Result SocketListen( SocketInfo const * const info, i32 const backlog );

/**
 * @brief Accepts a pending socket connection
 * @param info Socket description of the listening socket
 * @param out_client Socket description that gets filled with the accepted client as a socket
 * @return Result Result description
 */
Result SocketAccept( SocketInfo const * const info, SocketInfo * const out_client );

/**
 * @brief Initiate a connection on the socket
 * @param info        Socket description to connect
 * @param address     Address to connect to
 * @param out_connect Socket description the socket connected to
 * @return Result Result description
 */
Result SocketConnect( SocketInfo const * const info, cstr const address, cstr const port, SocketInfo * const out_connect );

/**
 * @brief Get the size of the current receive buffer of the specified socket
 * @param socket   Socket to get the buffer size from
 * @param out_size Pointer to the size variable to set
 * @return Result Result description
 */
Result SocketGetReceiveSize( Socket const socket, u64 * const out_size );

/**
 * @brief Send a message over the socket
 * @param socket      Socket descriptor
 * @param buffer      Message byte data
 * @param buffer_size Number of bytes in the buffer to send
 * @return Result Result description
 */
Result SocketSend( Socket const socket, cptr const buffer, u64 const buffer_size, u64 * const bytes_sent );

/**
 * @brief Receive a message over the socket
 * @param socket	  Socket descriptor
 * @param buffer      Buffer to retrieve data
 * @param buffer_size Size of the buffer in bytes
 * @return Result Result description
 */
Result SocketReceive( Socket const socket, ptr * const buffer, u64 * const buffer_size, u64 * const bytes_read );

/**
 * @brief Shuts down one or both socket channels.
 * @param socket Socket descriptor
 * @param how    Socket channel to shutdown
 * @return Result Result description
*/
Result SocketShutdown( SocketInfo * const socket, SocketChannel channel );

/* *********************** */
/* **** MLIBC Threads **** */
/* *********************** */

/// @brief Implementation-defined thread descriptor as unsigned 64-bit integer
typedef struct {
	u64 _[2];
} Thread;
/// @brief Function pointer type for thread functions
typedef i32 (*ThreadStart)( ptr args );

typedef u64 Mutex;

/// @brief Status of a thread
typedef enum {
	ThreadStatusSuccess,
	ThreadStatusTimedOut,
	ThreadStatusBusy,
	ThreadStatusNoMemory,
	ThreadStatusError
} ThreadStatus;

typedef enum {
	MutexTypeSimple,
	MutexTypeRecursive,
} MutexType;

/**
 * @brief Create and run a function as a new thread
 * @param thread   Pointer to fill with the thread descriptor
 * @param function Pointer to the function that will run in the thread
 * @param args     Pointer to the arguments of the function
 * @return ThreadStatus Status code of the thread creation
 */
ThreadStatus ThreadCreate( Thread * const thread, ThreadStart const function, ptr const args );

/**
 * @brief Compare two thread descriptors for equality
 * @param thread1 First thread descriptor to compare
 * @param thread2 Second thread descriptor to compare
 * @return i32 Non-zero if both descriptors refer to the same thread, zero otherwise
 */
i32 ThreadIsEqual( Thread const thread1, Thread const thread2 );

/**
 * @brief Get the current thread
 * @return Thread Thread descriptor of the current thread
 */
Thread ThreadCurrent( void );

/**
 * @brief Pause the current thread
 * @param milliseconds Time in milliseconds the thread should sleep
 * @return i32 Zero on success, non-zero on failure
 */
i32 ThreadSleep( u64 const milliseconds );

/**
 * @brief Pause the current thread
 * @param ts Time in seconds and nanoseconds provided the thread should sleep
 * @return i32 Zero on success, non-zero on failure
 */
i32 ThreadSleepTs( struct timespec const ts );

/**
 * @brief Yield the current thread to run other threads
 */
void ThreadYield( void );

/**
 * @brief Exit the thread and run all destructors set
 * @param code Exit code of the thread
 */
void ThreadExit( i32 const code );

/**
 * @brief Detach the given thread from the current environment
 * @param thread Thread descriptor of the thread to detach
 * @param ThreadStatus ThreadStatusSuccess or ThreadStatusError
 */
ThreadStatus ThreadDetach( Thread const thread );

/**
 * @brief Block the current thread until the given thread finished executing
 * @param thread Thread descriptor of the thread to await
 * @param ThreadStatus ThreadStatusSuccess or ThreadStatusError
 */
ThreadStatus ThreadJoin( Thread const thread, i32 * const result );

/**
 * @brief Create a bidirectional mutex
 * @param mutex Pointer to the mutex description
 * @param type  Type of mutex
 * @return ThreadStatus ThreadStatusSuccess or ThreadStatusError
 */
ThreadStatus MutexCreate( Mutex * const mutex, MutexType const type );

ThreadStatus MutexLock( Mutex * const mutex );

ThreadStatus MutexTryLock( Mutex * const mutex );

ThreadStatus MutexUnlock( Mutex * const mutex );

ThreadStatus MutexDestroy( Mutex * const mutex );

/*************************/
/***** MLIBC Servers *****/
/*************************/

typedef i32 (*OnReadFn)( SocketInfo const * const, u8 const * const data, u64 const data_length );

typedef enum {
	ServerStatusUninitialized,
	ServerStatusIdle,
	ServerStatusRunning
} ServerStatus;

typedef struct {
	SocketInfo   Socket;
	Thread       Listener;
	MemoryPool   Threads;
	u64          ThreadLimit;
	u64          ClientsPerThread;
	ServerStatus Status;
	OnReadFn     OnRead;
} TcpServer;

/**
 * @brief Create a new multithreaded tcp server
 * @param max_clients The maximum number of clients that can be concurrently connected per thread
 * @param max_threads The maximum number of threads the server should run concurrently
 * @return TcpServer Tcp server description
 */
TcpServer TcpServerCreate( u64 const max_clients, u64 const max_threads, OnReadFn const read_fn );

/**
 * @brief Close a server and return all allocated resources to the operating system
 * @param server Pointer to the server description
 */
void TcpServerClose( TcpServer * const server );

Result TcpServerStart( TcpServer * const server, cstr const address, cstr const port, i32 const backlog );



/* ******************** */
/* **** MLIBC Http **** */
/* ******************** */

typedef struct {
	String OriginalUri;
	String AbsoluteUri;
	String RelativeUri;
	String NetPath;
	String AbsolutePath;
	String RelativePath;
	String Path;
	String * Segments;
	String * Params;
} Uri;

#endif // MLIBC_H
