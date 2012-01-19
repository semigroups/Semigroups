#include <stdio.h>

int main()
{
   int a;
   scanf("%d",&a);
   a = a * 2;
   printf("The answer is %d",a);
   return 0;
}

#include        "system.h"              /* system dependent part           */

#include        "gasman.h"              /* garbage collector               */
#include        "objects.h"             /* objects                         */
#include        "scanner.h"             /* scanner                         */

#include        "gap.h"                 /* error handling, initialisation  */

#include        "gvars.h"               /* global variables                */

#include        "calls.h"               /* generic call mechanism          */
#include        "opers.h"               /* generic operations              */

#include        "ariths.h"              /* basic arithmetic                */

#include        "bool.h"                /* booleans                        */

#include        "integer.h"             /* integers                        */

#define INCLUDE_DECLARATION_PART
#include        "permutat.h"            /* permutations                    */
#undef  INCLUDE_DECLARATION_PART

#include        "records.h"             /* generic records                 */
#include        "precord.h"             /* plain records                   */

#include        "lists.h"               /* generic lists                   */
#include        "plist.h"               /* plain lists                     */
#include        "range.h"               /* ranges                          */
#include        "string.h"              /* strings                         */

#include        "saveload.h"            /* saving and loading              */

