/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2015
 *
 * RTS Symbols
 *
 * ---------------------------------------------------------------------------*/

#include "ghcplatform.h"
#include "RtsSymbolInfo.h"

#include "Rts.h"
#include "HsFFI.h"

#include "Hash.h"

/* -----------------------------------------------------------------------------
* Performs a check to see if the symbol at the given address
* a weak symbol or not.
*
* Returns: HS_BOOL_TRUE on symbol being weak, else HS_BOOL_FALSE
*/
HsBool isSymbolWeak(ObjectCode *owner, void *value)
{
    if (owner
        && value
        && owner->weakSymbols
        && lookupStrHashTable(owner->weakSymbols, value) != NULL)
    {
        return HS_BOOL_TRUE;
    }

    return HS_BOOL_FALSE;
}

/* -----------------------------------------------------------------------------
* Marks the symbol at the given address as weak or not.
* If the weak symbols table has not been initialized
* yet this will create and allocate a new Hashtable
*/
void setWeakSymbol(ObjectCode *owner, void *value)
{
    if (owner && value)
    {
        if (!owner->weakSymbols)
        {
            owner->weakSymbols = allocStrHashTable();
        }

        insertStrHashTable(owner->weakSymbols, value, (void*)HS_BOOL_TRUE);
    }
}
