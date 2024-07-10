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

#include <cstdlib>
#include <cstdio>
#include <time.h>
#include <unistd.h>
#include <list>
#include <stdbool.h>
#include "pmpmeas.h"
#include "meas.h"
#include "utils.h"

using namespace std;
using namespace PMPMEAS;

list<MeasType*> pmpmeas_type_lst;
list<Meas*> pmpmeas_meas_lst;
list<Meas*> pmpmeas_match_lst;


#define LENGTH_ERR_MSG 1280

string ctag;

time_t tstart;

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Library initialisation function
 */

void pmpmeas__init()
{
    tstart = time(NULL);

    char* t = getenv("PMPMEAS_MEAS_TYPES");
    if (t != NULL) {
        char *c1 = t;
        do {
            char *c0 = c1;
            for (; (*c1 != '\0') && (*c1 != ';'); c1++);
            if (*c1 == ';') {
                *c1 = '\0';
                pmpmeas_type_lst.push_back(new MeasType(c0));
            }
            else {
                pmpmeas_type_lst.push_back(new MeasType(c0));
                break;
            }
        } while (c1++);
    }
}

/*
 * Start measurement
 */

void pmpmeas__start(const char *tag)
{
    ctag = tag;

    pmpmeas_match_lst.clear();

    for (list<Meas*>::iterator m = pmpmeas_meas_lst.begin(); m != pmpmeas_meas_lst.end(); m++)
        if ((*m)->tag() == ctag)
            pmpmeas_match_lst.push_back(*m);

    if (pmpmeas_match_lst.empty())
        for (list<MeasType*>::iterator mt = pmpmeas_type_lst.begin(); mt != pmpmeas_type_lst.end(); mt++)
        {
            Meas *m = new Meas(tag, *(*mt));
            pmpmeas_meas_lst.push_back(m);
            pmpmeas_match_lst.push_back(m);

        }

    for (list<Meas*>::iterator m = pmpmeas_match_lst.begin(); m != pmpmeas_match_lst.end(); m++)
        (*m)->start();

}

// NEW FUNCTION: Read (without stopping if possible) metric values
Pmpmeas_vals pmpmeas__read()
{
    Pmpmeas_vals vals;
    int index = 0;

    for (list<Meas*>::iterator m = pmpmeas_meas_lst.begin(); m != pmpmeas_meas_lst.end(); m++){
        memcpy( &(vals.data[index]), (*m)->read(), (*m)->cnt()*sizeof(long long));
        index += (*m)->cnt();
    }
    vals.n = index;
    return vals;
}

void pmpmeas__stop(float weight)
{
    for (list<Meas*>::iterator m = pmpmeas_match_lst.begin(); m != pmpmeas_match_lst.end(); m++)
        (*m)->stop(weight);
}

void pmpmeas__finish()
{
    const int len = 1024;
    struct tm * timeinfo;
    char buf[len];
    char fname[len];
    pid_t pid = getpid();

    snprintf(fname, len, "pmpmeas_%06d_XXXXXX", pid);
    int ret = mkstemp(fname);
    FILE *fp = fopen(fname, "w");
    if (fp == NULL)
    {
        #ifdef RTRACE_SUPPORT
        char err_msg[LENGTH_ERR_MSG];
        snprintf(err_msg, LENGTH_ERR_MSG, "Failed to open file \"%s\" for writing\n", fname);
        report_and_exit(err_msg);
        #else
        fprintf(stderr, "Failed to open file \"%s\" for writing\n", fname);
        exit(1);
        #endif /* ifdef rTrace */
    }

    timeinfo = localtime(&tstart);
    strftime (buf, len, "%F %T", timeinfo);
    fprintf(fp, "Start:       %s\n", buf);
    fprintf(fp, "\n");

    gethostname(buf, len);
    fprintf(fp, "Hostname:    %s\n", buf);
    fprintf(fp, "\n");

    for (list<Meas*>::iterator m = pmpmeas_meas_lst.begin(); m != pmpmeas_meas_lst.end(); m++)
        (*m)->dump(fp);

    time_t tend = time(NULL);
    timeinfo = localtime(&tend);
    strftime (buf, len, "%F %T", timeinfo);
    fprintf(fp, "End:         %s\n", buf);

    fclose(fp);
}

#ifdef __cplusplus
}
#endif
