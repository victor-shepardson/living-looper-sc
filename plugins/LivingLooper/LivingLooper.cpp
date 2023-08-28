// LivingLooper.cpp
// Victor Shepardson (victor.shepardson@gmail.com)

#include <math.h>
#include "LivingLooper.hpp"
#include "SC_PlugIn.hpp"

static InterfaceTable* ft;

namespace LivingLooper {

LivingLooper::LivingLooper() {
    filename_length = in0(0);
    auto path = std::string(filename_length, '!');
    for (int i=0; i<filename_length; i++){
        path[i] = static_cast<char>(in0(i+1));
    }

    model = std::make_unique<LLModel>(mWorld->mFullRate.mSampleRate);
    model->load(path);

    mCalcFunc = make_calc_function<LivingLooper, &LivingLooper::next>();
}

// LivingLooper::~LivingLooper() {
// }

void LivingLooper::write_zeros_kr() {
    RANGE(j, mNumOutputs) out0(j) = 0;
}

void LivingLooper::write_zeros_ar(int i) {
    RANGE(j, mNumOutputs) out(j)[i] = 0;
}

void LivingLooper::next(int nSamples) {
    float buf[MAX_LOOPS];
    float latent_buf[MAX_LOOPS];

    const float* input = in(filename_length+1);
    const int loop_idx = in0(filename_length+2);
    const int oneshot = in0(filename_length+3);
    const int auto_mode = in0(filename_length+4);

    model->loop_idx = loop_idx;
    model->oneshot = oneshot;
    model->auto_mode = auto_mode;

    if (!model->loaded){
        RANGE(i, nSamples) write_zeros_ar(i);
        return;
    }

    RANGE(i, nSamples) {
        model->step(input[i], buf, latent_buf);
        RANGE(j, model->n_loops) {
            out(j)[i] = buf[j];
        }
        RANGE(j, model->n_loops) {
            out(j + model->n_loops)[i] = latent_buf[j];
        }
    }

}
} // namespace LivingLooper

PluginLoad(RAVEUGens) {
    // Plugin magic
    ft = inTable;
    registerUnit<LivingLooper::LivingLooper>(ft, "LivingLooper", false);
}
