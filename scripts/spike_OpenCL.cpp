__kernel void spike_dv_corr(__global float *result, const unsigned int count, __global const int *spikes,
        __global const float *volts, const int num_spikes, const int num_volts)
        {
          local int lag;
          lag = get_global_id(0);
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
        
__kernel void spike_spike_corr(__global float *result, const unsigned int count, __global const int *spikesA,
        __global const int *spikesB, const int num_spikesA, const int num_spikesB)
        {
          local int lag;
          lag = get_global_id(0);
          
          local int accumulator;
          accumulator = 0;
          
          local int a_index;
          local int b_index;
          a_index = 0;
          b_index = 0;
          
          for(b_index = 0; b_index<num_spikesB; b_index++){
            local int b;
            b = spikesB[b_index];
            local int a;
            a = spikesA[a_index]+lag;
            
            if(a==b){
              accumulator++;
              a_index++;
            } else if(b > a) {
              b_index--; //sorry
              a_index++;
            }
            
          }
          
          result[lag] = accumulator;
        }