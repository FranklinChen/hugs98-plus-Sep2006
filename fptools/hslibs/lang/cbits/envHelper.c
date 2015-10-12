/* 
 * (c) The University of Glasgow 2002
 *
 * environment operations
 */

#include "HsBase.h"
#include "HsLang.h"
#include <stdlib.h>

/* ToDo: write a feature test that doesn't assume 'environ' to
   be in scope at link-time. */
extern char** environ;

HsAddr
getEnvBlock()
{
  return (HsAddr)environ;
}
