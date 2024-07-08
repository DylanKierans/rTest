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

#ifndef PMPMEAS_MEASTYPES_H
#define PMPMEAS_MEASTYPES_H

#include <cstdlib>
#include <cstdio>
#include <string>
#include <vector>
#include <string.h>
#include "utils.h"
#include "config.h"

using namespace std;

namespace PMPMEAS {

class MeasType
{
public:
    enum Type {
        TIME_BOOT,
        TIME_CPU,
        TIME_THRD,
        PAPI,
        PERF,
    };

private:
    Type _type;
    vector<string> _subtype;

public:
    MeasType(Type type)
        : _type(type)
    {
        _subtype.resize(1);
        switch(_type)
        {
        case TIME_BOOT:
            _subtype[0] = "TIME_BOOT";
            break;
        case TIME_CPU:
            _subtype[0] = "TIME_CPU";
            break;
        case TIME_THRD:
            _subtype[0] = "TIME_THRD";
            break;
        default:
            _subtype[0] = "";
            break;
        }
    }

    MeasType(char* type_str)
    {
        if      (strncmp(type_str, "TIME_BOOT", 9) == 0)
        {
            _type = TIME_BOOT;
            _subtype.resize(1);
            _subtype[0] = type_str;
        }
        else if (strncmp(type_str, "TIME_CPU", 8) == 0)
        {
            _type = TIME_CPU;
            _subtype.resize(1);
            _subtype[0] = type_str;
        }
        else if (strncmp(type_str, "TIME_THRD", 9) == 0)
        {
            _type = TIME_THRD;
            _subtype.resize(1);
            _subtype[0] = type_str;
        }
        else if (strncmp(type_str, "PAPI=", 5) == 0)
        {
            _type = PAPI;
            char *c1 = (char *) &type_str[5];
            do {
                char *c0 = c1;
                for (; (*c1 != '\0') && (*c1 != ','); c1++);
                if (*c1 == ',') {
                    *c1 = '\0';
                    _subtype.push_back(string(c0));
                }
                else {
                    _subtype.push_back(string(c0));
                    break;
                }
            } while (c1++);
        }
        else if (strncmp(type_str, "PERF=", 5) == 0)
        {
            _type = PERF;
            char *c1 = (char *) &type_str[5];
            do {
                char *c0 = c1;
                for (; (*c1 != '\0') && (*c1 != ','); c1++);
                if (*c1 == ',') {
                    *c1 = '\0';
                    _subtype.push_back(string(c0));
                }
                else {
                    _subtype.push_back(string(c0));
                    break;
                }
            } while (c1++);
        }
        else
        {
            #ifdef RTRACE_SUPPORT
            char err_msg[100];
            snprintf(err_msg, 100, "[PMPMEAS] Unknown measurement type \"%s\"\n", type_str);
            report_and_exit(err_msg);
            #else
            fprintf(stderr, "[PMPMEAS] Unknown measurement type \"%s\"\n", type_str);
            exit(1);
            #endif /* ifdef rTrace */
        }
    }

    const MeasType::Type operator()() const {
        return _type;
    }

    const int cnt() const {
        return _subtype.size();
    }

    const char* typestr(int i) const {
        return _subtype[i].c_str();
    }
};

}

#endif