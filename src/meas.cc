/* -------
 * PMPMEAS
 * -------
 *
 * Copyright 2022 Dirk Pleiter (pleiter@kth.se)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. The origin of this software must not be misrepresented; you must
 *    not claim that you wrote the original software.  If you use this
 *    software in a product, an acknowledgment in the product
 *    documentation would be appreciated but is not required.
 *
 * 3. Altered source versions must be plainly marked as such, and must
 *    not be misrepresented as being the original software.
 *
 * 4. The name of the author may not be used to endorse or promote
 *    products derived from this software without specific prior written
 *    permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 * GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <cstdio>
#include "meas.h"

using namespace std;
using namespace PMPMEAS;

PapiInf Meas::_papi;
PerfInf Meas::_perf;

/*
 * Dump measurement results
 */

void Meas::dump(FILE* fp)
{
    fprintf(fp, "Tag:         %s\n", _tag.c_str());
    fprintf(fp,"Nmeas:       %d\n", nmeas());
    fprintf(fp, "Avg. Weight: %.4e\n", avweight());
    for (int i = 0; i < _cnt; i++)
    {
        const char* prefix = "";
        switch (_type())
        {
        case MeasType::PAPI:
            prefix = "PAPI=";
            break;
        case MeasType::PERF:
            prefix = "PERF=";
            break;
        }
        fprintf(fp, "Type[%d]:     %s%s\n", i, prefix, _type.typestr(i));

        fprintf(fp, "Min[%d]:      %.4e\n", i, min(i));
        fprintf(fp, "Mean[%d]:     %.4e\n", i, mean(i));
        fprintf(fp, "Max[%d]:      %.4e\n", i, max(i));
    }
    fprintf(fp, "\n");
}
