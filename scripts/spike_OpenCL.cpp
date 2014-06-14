__kernel void spike_correlogram(__global float *result, const unsigned int count, __global const int *spikes,
        __global const float *volts, const int num_spikes, const int num_volts)
        {
          int lag = get_global_id(0)
          ; //this is lag
          local int i;
          local float accumulator;
          accumulator = 0.0f;
          local int base;
          
          for(i = 0; i<num_spikes; i++){
            base = spikes[i]+lag;
            if((base+1) < num_volts){
                accumulator += volts[base+1]-volts[base];
            }
          }
          
          result[lag] = accumulator/num_spikes;
        }