__kernel void test(__global float *result, const unsigned int count, __global const int *spikes,
        __global const float *volts, __global const int *num_spikes, __global const int *num_volts)
        {
          int gid = get_global_id(0); //this is lag
          int lid = get_local_id(0);
          result[gid] = lid;
        }