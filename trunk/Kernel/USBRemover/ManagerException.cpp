/*
	ManagerException.cpp
	Started: 19.07.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>
	
	ManagerException class: implementation
*/
#include "ManagerException.h"

ManagerException::ManagerException(LPTSTR message)
{
	this->message = new TCHAR[_tcslen(message)+1];
	_tcscpy(this->message, message);
}

ManagerException::~ManagerException()
{
	if (this->message != NULL)
		delete [] this->message;
}