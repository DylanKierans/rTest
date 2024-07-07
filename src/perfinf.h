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

/*
 * Code for access to perf counters
*/

#ifndef PMPMEAS_PERFINF_H
#define PMPMEAS_PERFINF_H

#include <string>
#include <inttypes.h>
#include <unistd.h>

#define PERF_CNTMAX 5       //!< Maximum number of events

class PerfInf
{
public:
    enum Type {
#       define xx(a, b, c) a,
#       include "perfinftypesxx.h"
    };

private:
    static int _cnt;                    //!< Number of instances (must be 0 or 1)

    int _nevent;
    int _fd[PERF_CNTMAX];
    std::string _ename[PAPICNTMAX];     //!< Event name
    uint64_t _eid[PERF_CNTMAX];         //!< Event ID
    uint64_t _eval[PERF_CNTMAX];        //!< Event value

    char* _buf[4096];

public:
    PerfInf(void);

    int create(const std::string&);
    void cleanup();

    void start(void);
    void stop(void);
    void pread(void);

    uint64_t eval(int i) const
    {
        return _eval[i];
    }

    const char* ename(int i) const
    {
        return _ename[i].c_str();
    }

    int nevent() const
    {
        return _nevent;
    }

private:
    //long _perf_event_open(struct perf_event_attr *, pid_t, int, int, unsigned long);
};

#endif
