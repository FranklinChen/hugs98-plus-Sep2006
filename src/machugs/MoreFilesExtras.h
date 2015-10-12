/*
**	Apple Macintosh Developer Technical Support
**
**	A collection of useful high-level File Manager routines.
**
**	by Jim Luther, Apple Developer Technical Support Emeritus
**
**	File:		MoreFilesExtras.h
**
**	Copyright © 1992-1998 Apple Computer, Inc.
**	All rights reserved.
**
**	You may incorporate this sample code into your applications without
**	restriction, though the sample code has been provided "AS IS" and the
**	responsibility for its operation is 100% yours.  However, what you are
**	not permitted to do is to redistribute the source as "DSC Sample Code"
**	after having made changes. If you're going to re-distribute the source,
**	we require that you make it clear in the source that the code was
**	descended from Apple Sample Code, but that you've made changes.
*/

#ifndef __MOREFILESEXTRAS__
#define __MOREFILESEXTRAS__

#include <Types.h>
#include <Files.h>

#include "Optimization.h"

#ifdef __cplusplus
extern "C" {
#endif

/*****************************************************************************/

/* Constants and types from Universal Interfaces 3.0.1 Files.h */

#if	UNIVERSAL_INTERFACES_VERSION < 0x0301

enum {
	volMountNoLoginMsgFlagBit	= 0,							/* Input to VolumeMount: If set, the file system */
	volMountNoLoginMsgFlagMask	= 0x0001,						/*  should suppresss any log-in message/greeting dialog */
	volMountExtendedFlagsBit	= 7,							/* Input to VolumeMount: If set, the mount info is a */
	volMountExtendedFlagsMask	= 0x0080						/*  AFPXVolMountInfo record for 3.7 AppleShare Client */
};

/* AFPXVolMountInfo is the new AFP volume mount info record, requires the 3.7 AppleShare Client */

struct AFPXVolMountInfo {
	short 							length;						/* length of location data (including self) */
	VolumeType 						media;						/* type of media */
	short 							flags;						/* bits for no messages, no reconnect */
	SInt8 							nbpInterval;				/* NBP Interval parameter (IM2, p.322) */
	SInt8 							nbpCount;					/* NBP Interval parameter (IM2, p.322) */
	short 							uamType;					/* User Authentication Method type */
	short 							zoneNameOffset;				/* short positive offset from start of struct to Zone Name */
	short 							serverNameOffset;			/* offset to pascal Server Name string */
	short 							volNameOffset;				/* offset to pascal Volume Name string */
	short 							userNameOffset;				/* offset to pascal User Name string */
	short 							userPasswordOffset;			/* offset to pascal User Password string */
	short 							volPasswordOffset;			/* offset to pascal Volume Password string */
	short 							extendedFlags;				/* extended flags word */
	short 							uamNameOffset;				/* offset to a pascal UAM name string */
	short 							alternateAddressOffset;		/* offset to Alternate Addresses in tagged format */
	char 							AFPData[176];				/* variable length data may follow */
};
typedef struct AFPXVolMountInfo			AFPXVolMountInfo;
typedef AFPXVolMountInfo *				AFPXVolMountInfoPtr;

enum {
	kAFPExtendedFlagsAlternateAddressMask = 1					/*  bit in AFPXVolMountInfo.extendedFlags that means alternateAddressOffset is used*/
};

enum {
																/* constants for use in AFPTagData.fType field*/
	kAFPTagTypeIP				= 0x01,
	kAFPTagTypeIPPort			= 0x02,
	kAFPTagTypeDDP				= 0x03							/* Currently unused*/
};

enum {
																/* constants for use in AFPTagData.fLength field*/
	kAFPTagLengthIP				= 0x06,
	kAFPTagLengthIPPort			= 0x08,
	kAFPTagLengthDDP			= 0x06
};

struct AFPTagData {
	UInt8 							fLength;					/* length of this data tag including the fLength field */
	UInt8 							fType;
	UInt8 							fData[1];					/* variable length data */
};
typedef struct AFPTagData				AFPTagData;

struct AFPAlternateAddress {
	UInt8 							fAddressCount;
	UInt8 							fAddressList[1];			/* actually variable length packed set of AFPTagData */
};
typedef struct AFPAlternateAddress		AFPAlternateAddress;

#endif

#if PRAGMA_ALIGN_SUPPORTED
#pragma options align=mac68k
#endif

#if PRAGMA_ALIGN_SUPPORTED
#pragma options align=reset
#endif

/*****************************************************************************/

pascal	void	TruncPString(StringPtr destination,
							 ConstStr255Param source,
							 short maxLength);
/*	¶ International friendly string truncate routine.
	The TruncPString function copies up to maxLength characters from
	the source Pascal string to the destination Pascal string. TruncPString
	ensures that the truncated string ends on a single-byte character, or on
	the last byte of a multi-byte character.
	
	destination		output:	destination Pascal string.
	source			input:	source Pascal string.
	maxLength		output:	The maximum allowable length of the destination
							string.
*/

/*****************************************************************************/

pascal	OSErr	GetVolumeInfoNoName(ConstStr255Param pathname,
				short vRefNum,
				HParmBlkPtr pb);
/*	¶ Call PBHGetVInfoSync ignoring returned name.
	GetVolumeInfoNoName uses pathname and vRefNum to call PBHGetVInfoSync
	in cases where the returned volume name is not needed by the caller.
	The pathname and vRefNum parameters are not touched, and the pb
	parameter is initialized by PBHGetVInfoSync except that ioNamePtr in
	the parameter block is always returned as NULL (since it might point
	to GetVolumeInfoNoName's local variable tempPathname).

	I noticed using this code in several places, so here it is once.
	This reduces the code size of MoreFiles.

	pathName	input:	Pointer to a full pathname or nil.  If you pass in a 
						partial pathname, it is ignored. A full pathname to a
						volume must end with a colon character (:).
	vRefNum		input:	Volume specification (volume reference number, working
						directory number, drive number, or 0).
	pb			input:	A pointer to HParamBlockRec.
				output:	The parameter block as filled in by PBHGetVInfoSync
						except that ioNamePtr will always be NULL.
	
	Result Codes
		noErr				0		No error
		nsvErr				-35		No such volume
		paramErr			-50		No default volume, or pb was NULL
*/

/*****************************************************************************/

pascal	OSErr GetCatInfoNoName(short vRefNum,
							   long dirID,
							   ConstStr255Param name,
							   CInfoPBPtr pb);
/*	¶ Call PBGetCatInfoSync ignoring returned name.
	GetCatInfoNoName uses vRefNum, dirID and name to call PBGetCatInfoSync
	in cases where the returned object is not needed by the caller.
	The vRefNum, dirID and name parameters are not touched, and the pb
	parameter is initialized by PBGetCatInfoSync except that ioNamePtr in
	the parameter block is always returned as NULL (since it might point
	to GetCatInfoNoName's local variable tempName).

	I noticed using this code in several places, so here it is once.
	This reduces the code size of MoreFiles.

	vRefNum			input:	Volume specification.
	dirID			input:	Directory ID.
	name			input:	Pointer to object name, or nil when dirID
							specifies a directory that's the object.
	pb				input:	A pointer to CInfoPBRec.
					output:	The parameter block as filled in by
							PBGetCatInfoSync except that ioNamePtr will
							always be NULL.
	
	Result Codes
		noErr				0		No error
		nsvErr				-35		No such volume
		ioErr				-36		I/O error
		bdNamErr			-37		Bad filename
		fnfErr				-43		File not found
		paramErr			-50		No default volume
		dirNFErr			-120	Directory not found or incomplete pathname
		afpAccessDenied		-5000	User does not have the correct access
		afpObjectTypeErr	-5025	Directory not found or incomplete pathname
		
*/

/*****************************************************************************/

pascal	OSErr	DetermineVRefNum(ConstStr255Param pathname,
								 short vRefNum,
								 short *realVRefNum);
/*	¶ Determine the real volume reference number.
	The DetermineVRefNum function determines the volume reference number of
	a volume from a pathname, a volume specification, or a combination
	of the two.
	WARNING: Volume names on the Macintosh are *not* unique -- Multiple
	mounted volumes can have the same name. For this reason, the use of a
	volume name or full pathname to identify a specific volume may not
	produce the results you expect.  If more than one volume has the same
	name and a volume name or full pathname is used, the File Manager
	currently uses the first volume it finds with a matching name in the
	volume queue.

	pathName	input:	Pointer to a full pathname or nil.  If you pass in a 
						partial pathname, it is ignored. A full pathname to a
						volume must end with a colon character (:).
	vRefNum		input:	Volume specification (volume reference number, working
						directory number, drive number, or 0).
	realVRefNum	output:	The real volume reference number.
	
	Result Codes
		noErr				0		No error
		nsvErr				-35		No such volume
		paramErr			-50		No default volume
*/

/*****************************************************************************/

#if OLDROUTINENAMES
#define	GetDirID(vRefNum, dirID, name, theDirID, isDirectory)	\
		GetDirectoryID(vRefNum, dirID, name, theDirID, isDirectory)
#endif

pascal	OSErr	GetDirectoryID(short vRefNum,
							   long dirID,
							   ConstStr255Param name,
							   long *theDirID,
							   Boolean *isDirectory);
/*	¶ Get the directory ID number of the directory specified.
	The GetDirectoryID function gets the directory ID number of the
	directory specified.  If a file is specified, then the parent
	directory of the file is returned and isDirectory is false.  If
	a directory is specified, then that directory's ID number is
	returned and isDirectory is true.
	WARNING: Volume names on the Macintosh are *not* unique -- Multiple
	mounted volumes can have the same name. For this reason, the use of a
	volume name or full pathname to identify a specific volume may not
	produce the results you expect.  If more than one volume has the same
	name and a volume name or full pathname is used, the File Manager
	currently uses the first volume it finds with a matching name in the
	volume queue.
	
	vRefNum			input:	Volume specification.
	dirID			input:	Directory ID.
	name			input:	Pointer to object name, or nil when dirID
							specifies a directory that's the object.
	theDirID		output:	If the object is a file, then its parent directory
							ID. If the object is a directory, then its ID.
	isDirectory		output:	True if object is a directory; false if
							object is a file.
	
	Result Codes
		noErr				0		No error
		nsvErr				-35		No such volume
		ioErr				-36		I/O error
		bdNamErr			-37		Bad filename
		fnfErr				-43		File not found
		paramErr			-50		No default volume
		dirNFErr			-120	Directory not found or incomplete pathname
		afpAccessDenied		-5000	User does not have the correct access
		afpObjectTypeErr	-5025	Directory not found or incomplete pathname
*/

/*****************************************************************************/

pascal	OSErr	GetDirItems(short vRefNum,
							long dirID,
							ConstStr255Param name,
							Boolean getFiles,
							Boolean getDirectories,
							FSSpecPtr items,
							short reqItemCount,
							short *actItemCount,
							short *itemIndex);
/*	¶ Return a list of items in a directory.
	The GetDirItems function returns a list of items in the specified
	directory in an array of FSSpec records. File, subdirectories, or
	both can be returned in the list.
	
	A noErr result indicates that the items array was filled
	(actItemCount == reqItemCount) and there may be additional items
	left in the directory. A fnfErr result indicates that the end of
	the directory list was found and actItemCount items were actually
	found this time.

	vRefNum			input:	Volume specification.
	dirID			input:	Directory ID.
	name			input:	Pointer to object name, or nil when dirID
							specifies a directory that's the object.
	getFiles		input:	Pass true to have files added to the items list.
	getDirectories	input:	Pass true to have directories added to the
							items list.
	items			input:	Pointer to array of FSSpec where the item list
							is returned.
	reqItemCount	input:	Maximum number of items to return (the number
							of elements in the items array).
	actItemCount	output: The number of items actually returned.
	itemIndex		input:	The current item index position. Set to 1 to
							start with the first item in the directory.
					output:	The item index position to get the next item.
							Pass this value the next time you call
							GetDirItems to start where you left off.
	
	Result Codes
		noErr				0		No error, but there are more items
									to list
		nsvErr				-35		No such volume
		ioErr				-36		I/O error
		bdNamErr			-37		Bad filename
		fnfErr				-43		File not found, there are no more items
									to be listed.
		paramErr			-50		No default volume or itemIndex was <= 0
		dirNFErr			-120	Directory not found or incomplete pathname
		afpAccessDenied		-5000	User does not have the correct access
		afpObjectTypeErr	-5025	Directory not found or incomplete pathname
*/

/*****************************************************************************/

#ifdef __cplusplus
}
#endif

#include "OptimizationEnd.h"

#endif	/* __MOREFILESEXTRAS__ */
