#include "BaseException.h"

BaseException::BaseException()
{
	this->innerException = NULL;
}

BaseException::BaseException(BaseException* inner)
{
	this->innerException = inner;
}

BaseException::~BaseException()
{
	if (this->innerException != NULL)
		delete this->innerException;
}