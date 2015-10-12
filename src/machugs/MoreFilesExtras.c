/*
**	Apple Macintosh Developer Technical Support
**
**	A collection of useful high-level File Manager routines.
**
**	by Jim Luther, Apple Developer Technical Support Emeritus
**
**	File:		MoreFilesExtras.c
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

#include <Files.h>
#include <TextUtils.h>
#include <Errors.h>

#define	__COMPILINGMOREFILES

#include "MoreFilesExtras.h"

/*****************************************************************************/

pascal	void	TruncPString(StringPtr destination,
							 ConstStr255Param source,
							 short maxLength)
{
	short	charType;
	
	if ( source != NULL && destination != NULL )	/* don't do anything stupid */
	{
		if ( source[0] > maxLength )
		{
			/* Make sure the string isn't truncated in the middle of */
			/* a multi-byte character. */
			while (maxLength != 0)
			{
				charType = CharByte((Ptr)&source[1], maxLength);
				if ( (charType == smSingleByte) || (charType == smLastByte) )
					break;	/* source[maxLength] is now a valid last character */ 
				--maxLength;
			}
		}
		else
		{
			maxLength = source[0];
		}
		/* Set the destination string length */
		destination[0] = maxLength;
		/* and copy maxLength characters (if needed) */
		if ( source != destination )
		{
			while ( maxLength != 0 )
			{
				destination[maxLength] = source[maxLength];
				--maxLength;
			}
		}
	}
}

/*****************************************************************************/

/*
**	GetVolumeInfoNoName uses pathname and vRefNum to call PBHGetVInfoSync
**	in cases where the returned volume name is not needed by the caller.
**	The pathname and vRefNum parameters are not touched, and the pb
**	parameter is initialized by PBHGetVInfoSync except that ioNamePtr in
**	the parameter block is always returned as NULL (since it might point
**	to the local tempPathname).
**
**	I noticed using this code in several places, so here it is once.
**	This reduces the code size of MoreFiles.
*/
pascal	OSErr	GetVolumeInfoNoName(ConstStr255Param pathname,
									short vRefNum,
									HParmBlkPtr pb)
{
	Str255 tempPathname;
	OSErr error;
	
	/* Make sure pb parameter is not NULL */ 
	if ( pb != NULL )
	{
		pb->volumeParam.ioVRefNum = vRefNum;
		if ( pathname == NULL )
		{
			pb->volumeParam.ioNamePtr = NULL;
			pb->volumeParam.ioVolIndex = 0;		/* use ioVRefNum only */
		}
		else
		{
			BlockMoveData(pathname, tempPathname, pathname[0] + 1);	/* make a copy of the string and */
			pb->volumeParam.ioNamePtr = (StringPtr)tempPathname;	/* use the copy so original isn't trashed */
			pb->volumeParam.ioVolIndex = -1;	/* use ioNamePtr/ioVRefNum combination */
		}
		error = PBHGetVInfoSync(pb);
		pb->volumeParam.ioNamePtr = NULL;	/* ioNamePtr may point to local	tempPathname, so don't return it */
	}
	else
	{
		error = paramErr;
	}
	return ( error );
}

/*****************************************************************************/

pascal	OSErr GetCatInfoNoName(short vRefNum,
							   long dirID,
							   ConstStr255Param name,
							   CInfoPBPtr pb)
{
	Str31 tempName;
	OSErr error;
	
	/* Protection against File Sharing problem */
	if ( (name == NULL) || (name[0] == 0) )
	{
		tempName[0] = 0;
		pb->dirInfo.ioNamePtr = tempName;
		pb->dirInfo.ioFDirIndex = -1;	/* use ioDirID */
	}
	else
	{
		pb->dirInfo.ioNamePtr = (StringPtr)name;
		pb->dirInfo.ioFDirIndex = 0;	/* use ioNamePtr and ioDirID */
	}
	pb->dirInfo.ioVRefNum = vRefNum;
	pb->dirInfo.ioDrDirID = dirID;
	error = PBGetCatInfoSync(pb);
	pb->dirInfo.ioNamePtr = NULL;
	return ( error );
}

/*****************************************************************************/

pascal	OSErr	DetermineVRefNum(ConstStr255Param pathname,
								 short vRefNum,
								 short *realVRefNum)
{
	HParamBlockRec pb;
	OSErr error;

	error = GetVolumeInfoNoName(pathname,vRefNum, &pb);
	if ( error == noErr )
	{
		*realVRefNum = pb.volumeParam.ioVRefNum;
	}
	return ( error );
}

/*****************************************************************************/

pascal	OSErr	GetDirectoryID(short vRefNum,
							   long dirID,
							   ConstStr255Param name,
							   long *theDirID,
							   Boolean *isDirectory)
{
	CInfoPBRec pb;
	OSErr error;

	error = GetCatInfoNoName(vRefNum, dirID, name, &pb);
	if ( error == noErr )
	{
		*isDirectory = (pb.hFileInfo.ioFlAttrib & ioDirMask) != 0;
		if ( *isDirectory )
		{
			*theDirID = pb.dirInfo.ioDrDirID;
		}
		else
		{
			*theDirID = pb.hFileInfo.ioFlParID;
		}
	}
	
	return ( error );
}

/*****************************************************************************/

pascal	OSErr	GetDirItems(short vRefNum,
							long dirID,
							ConstStr255Param name,
							Boolean getFiles,
							Boolean getDirectories,
							FSSpecPtr items,
							short reqItemCount,
							short *actItemCount,
							short *itemIndex) /* start with 1, then use what's returned */
{
	CInfoPBRec pb;
	OSErr error;
	long theDirID;
	Boolean isDirectory;
	FSSpec *endItemsArray;
	
	if ( *itemIndex > 0 )
	{
		/* NOTE: If I could be sure that the caller passed a real vRefNum and real directory */
		/* to this routine, I could rip out calls to DetermineVRefNum and GetDirectoryID and this */
		/* routine would be much faster because of the overhead of DetermineVRefNum and */
		/* GetDirectoryID and because GetDirectoryID blows away the directory index hint the Macintosh */
		/* file system keeps for indexed calls. I can't be sure, so for maximum throughput, */
		/* pass a big array of FSSpecs so you can get the directory's contents with few calls */
		/* to this routine. */
		
		/* get the real volume reference number */
		error = DetermineVRefNum(name, vRefNum, &pb.hFileInfo.ioVRefNum);
		if ( error == noErr )
		{
			/* and the real directory ID of this directory (and make sure it IS a directory) */
			error = GetDirectoryID(vRefNum, dirID, name, &theDirID, &isDirectory);
			if ( error == noErr )
			{
				if ( isDirectory )
				{
					*actItemCount = 0;
					endItemsArray = items + reqItemCount;
					while ( (items < endItemsArray) && (error == noErr) )
					{
						pb.hFileInfo.ioNamePtr = (StringPtr) &items->name;
						pb.hFileInfo.ioDirID = theDirID;
						pb.hFileInfo.ioFDirIndex = *itemIndex;
						error = PBGetCatInfoSync(&pb);
						if ( error == noErr )
						{
							items->parID = pb.hFileInfo.ioFlParID;	/* return item's parID */
							items->vRefNum = pb.hFileInfo.ioVRefNum;	/* return item's vRefNum */
							++*itemIndex;	/* prepare to get next item in directory */
							
							if ( (pb.hFileInfo.ioFlAttrib & ioDirMask) != 0 )
							{
								if ( getDirectories )
								{
									++*actItemCount; /* keep this item */
									++items; /* point to next item */
								}
							}
							else
							{
								if ( getFiles )
								{
									++*actItemCount; /* keep this item */
									++items; /* point to next item */
								}
							}
						}
					}
				}
				else
				{
					/* it wasn't a directory */
					error = dirNFErr;
				}
			}
		}
	}
	else
	{
		/* bad itemIndex */
		error = paramErr;
	}
	
	return ( error );
}

/*****************************************************************************/

