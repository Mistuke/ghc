/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2015
 *
 * RTS Symbol Info
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_SYMBOLINFO_H
#define RTS_SYMBOLINFO_H

#include "LinkerInternals.h"

HsBool isSymbolWeak(ObjectCode *owner, void *value);
void setWeakSymbol(ObjectCode *owner, void *value);

#endif /* RTS_SYMBOLINFO_H */
