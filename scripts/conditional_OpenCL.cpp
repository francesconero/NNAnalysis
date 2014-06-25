__kernel void test(__global float *result, const unsigned int output_length, __global const int *spikes,
                      __global const int *num_spikes)
        {
          int gid = get_global_id(0); //this is lag
          int lid = get_local_id(0);
          result[gid] = num_spikes[lid];
        }