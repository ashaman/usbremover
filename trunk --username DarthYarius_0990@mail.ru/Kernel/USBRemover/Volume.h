/*
	Volume.h
	Started: 18.06.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Volume class: definitions
*/

#ifndef _H_VOLUME
#define _H_VOLUME

#ifdef DEBUG
	#define _CRTDBG_MAP_ALLOC
	#include <stdlib.h>
	#include <crtdbg.h>
#endif

//Windows & SDK headers
#include <windows.h>
#include <shlobj.h>
#include "ApiWrapper.h"

//STD & classes headers
#include "Device.h"
#include "IMessageListener.h"

/*
	Description:
		Volume class: inherited from Device.
		Represents a logical disk (or volume)
*/
class Volume: public Device, public IMessageListener
{
private:
	DWORD diskNumber; //disk number
	StringList *mountPoints; //volume mount points

	void GetVolumeInformation(); //get the information
	void NotifySystem(); //notify system about the successful device removal
	void RefreshMountPoints(); //refreshes the mount point list
public:
	Volume(LPTSTR path, DeviceDataSet dataSet, HANDLE informationSet);
	~Volume();
	DWORD DiskNumber() const {return this->diskNumber;}; //gets the disk number
	StringPairList DosMountPoints() const; //DOS mount points of the device
	bool Eject(); //eject the device
	StringList* MountPoints() const {return this->mountPoints;}; //get mount points
	void RefreshState(); //refreshes the mount point list
};

#endif