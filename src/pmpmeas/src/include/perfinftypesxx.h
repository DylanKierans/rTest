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

// PMPMEAS name, perf type, perf config
// See: /usr/include/linux/perf_event.h and https://github.com/efficient/memc3/blob/master/perf_count.c

xx(cycles,                  PERF_TYPE_HARDWARE, PERF_COUNT_HW_CPU_CYCLES)
xx(ref_cycles,              PERF_TYPE_HARDWARE, PERF_COUNT_HW_REF_CPU_CYCLES)

xx(L1_dcache_loads,         PERF_TYPE_HW_CACHE, (PERF_COUNT_HW_CACHE_L1D  | (PERF_COUNT_HW_CACHE_OP_READ << 8)  | (PERF_COUNT_HW_CACHE_RESULT_ACCESS << 16)))
xx(L1_dcache_load_misses,   PERF_TYPE_HW_CACHE, (PERF_COUNT_HW_CACHE_L1D  | (PERF_COUNT_HW_CACHE_OP_READ << 8)  | (PERF_COUNT_HW_CACHE_RESULT_MISS << 16)))

xx(L1_dcache_stores,        PERF_TYPE_HW_CACHE, (PERF_COUNT_HW_CACHE_L1D  | (PERF_COUNT_HW_CACHE_OP_WRITE << 8) | (PERF_COUNT_HW_CACHE_RESULT_ACCESS << 16)))

xx(LLC_loads,               PERF_TYPE_HW_CACHE, (PERF_COUNT_HW_CACHE_LL   | (PERF_COUNT_HW_CACHE_OP_READ  << 8) | (PERF_COUNT_HW_CACHE_RESULT_ACCESS << 16)))
xx(LLC_load_misses,         PERF_TYPE_HW_CACHE, (PERF_COUNT_HW_CACHE_LL   | (PERF_COUNT_HW_CACHE_OP_READ  << 8) | (PERF_COUNT_HW_CACHE_RESULT_MISS << 16)))
xx(LLC_stores,              PERF_TYPE_HW_CACHE, (PERF_COUNT_HW_CACHE_LL   | (PERF_COUNT_HW_CACHE_OP_WRITE << 8) | (PERF_COUNT_HW_CACHE_RESULT_ACCESS << 16)))

xx(dTLB_loads,              PERF_TYPE_HW_CACHE, (PERF_COUNT_HW_CACHE_DTLB | (PERF_COUNT_HW_CACHE_OP_READ << 8)  | (PERF_COUNT_HW_CACHE_RESULT_ACCESS << 16)))
xx(dTLB_load_misses,        PERF_TYPE_HW_CACHE, (PERF_COUNT_HW_CACHE_DTLB | (PERF_COUNT_HW_CACHE_OP_READ << 8)  | (PERF_COUNT_HW_CACHE_RESULT_MISS << 16)))

#undef xx
