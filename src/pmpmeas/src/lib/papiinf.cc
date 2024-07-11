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

#include "papiinf.h"
#include "utils.h"
#include "config.h"

using namespace PMPMEAS;

int PapiInf::_cnt = 0;

PapiInf::PapiInf(void)
{
    int ret;

#ifdef USEPAPI
    _cnt++;
    if (_cnt > 1)
    {
        #ifdef RTRACE_SUPPORT
            report_and_exit("At most one object of class PapiInf must be instantiated!\n");
        #else
            fprintf(stderr, "At most one object of class PapiInf must be instantiated!\n");
            exit(1);
        #endif /* ifdef rTrace */
    }

    if (PAPI_library_init(PAPI_VER_CURRENT) != PAPI_VER_CURRENT)
    {
        #ifdef RTRACE_SUPPORT
            report_and_exit("PAPI library init error!\n");
        #else
            fprintf(stderr, "PAPI library init error!\n");
            exit(1);
        #endif /* ifdef rTrace */
    }

    _ctx = PAPI_NULL;
    ret = PAPI_create_eventset(&_ctx);
    if (ret != PAPI_OK)
    {
        #ifdef RTRACE_SUPPORT
            char err_msg[100];
            snprintf(err_msg, 100, "Failed to create event set (%s)\n", PAPI_strerror(ret));
            report_and_exit(err_msg);
        #else
            fprintf(stderr, "Failed to create event set (%s)\n", PAPI_strerror(ret));
            exit(1);
        #endif /* ifdef rTrace */
    }
#endif

    _nevent = 0;
}

int PapiInf::create(const std::string& ename)
{
    int ret;

#ifdef USEPAPI
    if (_nevent == PAPICNTMAX)
    {
        #ifdef RTRACE_SUPPORT
        report_and_exit("Exceeding upper limit of number of PAPI events\n");
        #else
        fprintf(stderr, "Exceeding upper limit of number of PAPI events\n");
        exit(1);
        #endif /* ifdef rTrace */
    }

    ret = PAPI_add_named_event(_ctx, (char*) ename.c_str());
    if (ret != PAPI_OK)
    {
        #ifdef RTRACE_SUPPORT
        char err_msg[100];
        snprintf(err_msg, 100, "WARNING: PAPI_add_named_event failed (%s,\"%s\")\n",
                PAPI_strerror(ret), ename.c_str());
        report_and_exit(err_msg);
        #else
        fprintf(stderr, "WARNING: PAPI_add_named_event failed (%s,\"%s\")\n",
                PAPI_strerror(ret), ename.c_str());
        return -1;
        #endif /* ifdef rTrace */
    }

    _ename[_nevent] = ename.c_str();
    _nevent++;
//fprintf(stderr, "DEBUG[%s,%d]: ename=%s, nevent=%d\n", __FILE__, __LINE__, ename.c_str(), _nevent);

    return (_nevent - 1);
#else
    return 0;
#endif
}

void PapiInf::cleanup()
{
    int ret;

#ifdef USEPAPI
    ret = PAPI_cleanup_eventset(_ctx);
    if (ret != PAPI_OK)
    {
        #ifdef RTRACE_SUPPORT
        char err_msg[100];
        snprintf(err_msg, 100, "PAPI_cleanup_eventset failed (%s)\n", PAPI_strerror(ret));
        report_and_exit(err_msg);
        #else
        fprintf(stderr, "PAPI_cleanup_eventset failed (%s)\n", PAPI_strerror(ret));
        exit(1);
        #endif /* ifdef rTrace */
    }
#endif

    _nevent = 0;
}

void PapiInf::start(void)
{
#ifdef USEPAPI
    if (PAPI_start(_ctx) != PAPI_OK)
    {
        #ifdef RTRACE_SUPPORT
        report_and_exit("PAPI_start failed\n");
        #else
        fprintf(stderr, "PAPI_start failed\n");
        exit(1);
        #endif /* ifdef rTrace */
    }
#endif
}

void PapiInf::stop(void)
{
    int ret;

#ifdef USEPAPI
    ret = PAPI_stop(_ctx, _eval);
    if (ret != PAPI_OK)
    {
        #ifdef RTRACE_SUPPORT
        report_and_exit("PAPI_stop failed\n");
        #else
        fprintf(stderr, "PAPI_stop failed\n");
        exit(1);
        #endif /* ifdef rTrace */
    }
#endif
}

void PapiInf::read(void)
{
	int ret;

#ifdef USEPAPI
    ret = PAPI_read(_ctx, _eval);
    if (ret != PAPI_OK)
    {
        #ifdef RTRACE_SUPPORT
        char err_msg[100];
        snprintf(err_msg, 100, "PAPI_read failed (%s)\n", PAPI_strerror(ret));
        report_and_exit(err_msg);
        #else
        fprintf(stderr, "PAPI_read failed (%s)\n", PAPI_strerror(ret));
        exit(1);
        #endif /* ifdef rTrace */
    }
#endif
}
