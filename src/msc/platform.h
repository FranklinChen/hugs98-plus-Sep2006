/*
 * configure-sussed platform #defines.
 */
#ifndef __PLATFORM_H__
#define __PLATFORM_H__

#define HostPlatform   i386_unknown_msvc
#define TargetPlatform i386_unknown_msvc
#define BuildPlatform  i386_unknown_msvc

/* Definitions suitable for use in CPP conditionals */
#define i386_unknown_msvc_HOST 1
#define i386_unknown_msvc_TARGET 1
#define i386_unknown_msvc_BUILD 1

#define i386_HOST_ARCH 1
#define i386_TARGET_ARCH 1
#define i386_BUILD_ARCH 1

#define msvc_HOST_OS 1
#define msvc_TARGET_OS 1
#define msvc_BUILD_OS 1

/* Definitions of strings for use in C or Haskell code */
#define HOST_ARCH	"i686"
#define TARGET_ARCH	"i686"
#define BUILD_ARCH	"i686"

#define HOST_OS		"msvc"
#define TARGET_OS	"msvc"
#define BUILD_OS	"msvc"

#endif /* __PLATFORM_H__ */
