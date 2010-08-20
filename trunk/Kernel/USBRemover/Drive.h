/*
	Drive.h
	Started: 24.05.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Disk drive class
*/
#ifndef _H_DRIVE
#define _H_DRIVE

#ifdef DEBUG
	#define _CRTDBG_MAP_ALLOC
	#include <stdlib.h>
	#include <crtdbg.h>
#endif

#include "Device.h"

/*
	Disk drive class
	Represents the disk device
*/
class DiskDrive: public Device
{
private:
	DWORD diskNumber;
public:
	DiskDrive(DWORD number, LPTSTR path, DeviceDataSet dataSet, HANDLE informationSet); //constructor
	~DiskDrive(); //destructor
	DWORD DiskNumber() const {return this->diskNumber;}; //disk number
	bool Eject(); //overrides abstract method
};

#endif