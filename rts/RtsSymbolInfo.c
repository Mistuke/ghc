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
#include "RtsUtils.h"

typedef struct _SymbolInfo {
    /* Determines if the
       symbol is weak */
    HsBool isWeak;

    /* Determines if the value
       of the symbol is > 0 */
    HsBool hasValue;
} SymbolInfo;

/* -----------------------------------------------------------------------------
* Performs a check to see if the symbol at the given address
* is a weak symbol or not.
*
* Returns: HS_BOOL_TRUE on symbol being weak, else HS_BOOL_FALSE
*/
HsBool isSymbolWeak(ObjectCode *owner, void *label)
{
    SymbolInfo *info;
    if (owner
        && label
        && owner->extraInfos
        && (info = lookupStrHashTable(owner->extraInfos, label)) != NULL)
    {
        return info->isWeak;
    }

    return HS_BOOL_FALSE;
}

/* -----------------------------------------------------------------------------
* Performs a check to see if the symbol at the given address
* has a non-zero value or not.
*
* Returns: HS_BOOL_TRUE on symbol has a non-zero value, else HS_BOOL_FALSE
*/
HsBool isSymbolEmpty(ObjectCode *owner, void *label)
{
    SymbolInfo *info;
    if (owner
        && label
        && owner->extraInfos
        && (info = lookupStrHashTable(owner->extraInfos, label)) != NULL)
    {
        return info->hasValue;
    }

    return HS_BOOL_FALSE;
}

/* -----------------------------------------------------------------------------
* Marks the symbol at the given address as weak or not.
* If the extra symbol infos table has not been initialized
* yet this will create and allocate a new Hashtable
*/
void setWeakSymbol(ObjectCode *owner, void *label)
{
    SymbolInfo *info;
    if (owner && label)
    {
        if (!owner->extraInfos)
        {
            owner->extraInfos = allocStrHashTable();
            info = stgMallocBytes(sizeof(SymbolInfo), "setWeakSymbol");
        }
        else {
            info = lookupStrHashTable(owner->extraInfos, label);
        }

        info->isWeak = HS_BOOL_TRUE;

        insertStrHashTable(owner->extraInfos, label, info);
    }
}


/* -----------------------------------------------------------------------------
* Marks the symbol at the given address as having a zero value.
* If the extra symbol infos table has not been initialized
* yet this will create and allocate a new Hashtable
*/
void setSymbolIsEmpty(ObjectCode *owner, void *label)
{
    SymbolInfo *info;
    if (owner && label)
    {
        if (!owner->extraInfos)
        {
            owner->extraInfos = allocStrHashTable();
            info = stgMallocBytes(sizeof(SymbolInfo), "setSymbolIsEmpty");
        }
        else {
            info = lookupStrHashTable(owner->extraInfos, label);
        }

        info->hasValue = HS_BOOL_TRUE;

        insertStrHashTable(owner->extraInfos, label, info);
    }
}
