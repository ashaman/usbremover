/*
	DeviceManager.h
	Started: 22.05.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Device manager base class
*/

#ifndef _H_DEVICEMANAGER
#define _H_DEVICEMANAGER

#ifdef DEBUG
	#define _CRTDBG_MAP_ALLOC
	#include <stdlib.h>
	#include <crtdbg.h>
#endif

//System and wrapper headers
#include "ApiWrapper.h"

//STL and classes headers
#include <vector>
#include <set>

#include "Device.h"
#include "Volume.h"
#include "Drive.h"
#include "USBDevice.h"
#include "MessageManager.h"
#include "VolumeManager.h"

//Type definition for the device lists
typedef std::vector<Volume*> VolumeList;
typedef std::vector<DiskDrive*> DriveList;
typedef std::vector<USBDevice*> USBDeviceList;

/*
	Description:
		This class is the core of the project - it manages all the removable
		devices found in system. There may be some problems with threads (I don't
		really know if there are any, I've tested it on my machine and there were
		no memory or thread problems). Anyway, it builds a list of device "hives"
		from the bottom, and that's why it may skip card readers with empty slots.
		(if I have an opportunity to test it on the card readers, I will do it)
*/
class DeviceManager : public IMessageListener, public IVolumeListener
{
private:
	DelegateSet *listeners; //all manager listeners
	VolumeManager *volmgr; //volume manager
	VolumeList *volumes; //all volumes in the manager
	DriveList *drives; //all drives in the manager
	USBDeviceList *sticks; //all USB sticks/telephones/cardreaders
	std::set<DWORD> disknums; //disk numbers
	HANDLE hVolumeInformationSet; //info set handle for volumes
	HANDLE hDriveInformationSet; //info set handle for drives
	HANDLE hDeviceInformationSet; //info set handle for devices

	void Clean(); //cleans all internal data fields
	void FireEvent(); //fires an event
	void GetDevices(); //get all usb devices
	void GetDrives(DeviceDataSet driveDataSet); //get all the removable drives
	void GetVolumes(DeviceDataSet volumeDataSet); //get all the removable volumes
	void LinkVD(); //links volumes and drives
	void Refresh(); //refreshes information about devices
public:
	DeviceManager(); //constructor
	~DeviceManager(); //destructor
	void AttachListener(IMessageListener *listener); //attaches a listener
	void DetachListener(IMessageListener *listener); //detaches a listener
	USBDeviceList Devices() const {return *this->sticks;}; //all the devices - do everything!
	void RefreshState(); //implements IMessageListener::RefreshState();
	void RefreshVolumeState(); //implements IVolumeListener::RefreshVolumeState();
	VolumeManager& VolumeManager() const {return *this->volmgr;}; //volume manager

	/*
		WARNING!
		Devices() method is not thread safe!!! (for ejecting, of course.
		And for reading maybe)
		DO NOT TRY TO EJECT DEVICES IN THE DIFFERENT THREADS!!!
		If I find the solution, I will fix this... weakness
	*/
};

#endif