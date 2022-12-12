// LivingLooper.cpp
// Victor Shepardson (victor.shepardson@gmail.com)

#include <math.h>
#include "LivingLooper.hpp"
#include "SC_PlugIn.hpp"

static InterfaceTable* ft;

namespace LivingLooper {

auto LivingLooper::models = std::map<std::string, LLModel* >();

LivingLooper::LivingLooper() {
    inIdx = outIdx = 0;
    first_block_done = false;

    filename_length = in0(0);
    // std::cout<<filename_length<<std::endl;
    // char path[filename_length];
    auto path = std::string(filename_length, '!');
    for (int i=0; i<filename_length; i++){
        path[i] = static_cast<char>(in0(i+1));
    }

    auto kv = models.find(path);
    if (kv==models.end()){
        model = new LLModel();
        std::cout << "loading: \"" << path << "\"" << std::endl;
        model->load(path);
        models.insert({path, model});
    } else {
        model = kv->second;
        std::cout << "found \"" << path << "\" already loaded" << std::endl;
    }

    model->reset();

    int block_bytes = model->block_size * sizeof(float);
    inBuffer = (float*)RTAlloc(this->mWorld, block_bytes);
    outBuffer = (float*)RTAlloc(this->mWorld, model->n_loops * block_bytes);

    mCalcFunc = make_calc_function<LivingLooper, &LivingLooper::next>();

    this->ugen_outputs = model->n_loops;
}

LivingLooper::~LivingLooper() {
    RTFree(this->mWorld, inBuffer);
    RTFree(this->mWorld, outBuffer);
}

void LivingLooper::write_zeros_kr() {
    // std::cout<<"write zeros"<<std::endl;
    for (int j=0; j < this->ugen_outputs; ++j){
        out0(j) = 0;
    }
}

void LivingLooper::write_zeros_ar(int i) {
    // std::cout<<"write zeros"<<std::endl;
    for (int j=0; j < this->ugen_outputs; ++j){
        out(j)[i] = 0;
    }
}

void LivingLooper::next(int nSamples) {
    const float* input = in(filename_length+1);
    const int loop_idx = in0(filename_length+2);
    const int oneshot = in0(filename_length+3);

    int n_loops = model->n_loops;
    int model_block = model->block_size;
    int host_block = nSamples;

    // assume model_block and host_block are powers of two
    // handle case when model_block > host_block and the reverse
    int io_blocks = ceil(float(host_block) / model_block);
    int min_block = std::min(model_block, host_block);

    int hostInIdx = 0;
    int hostOutIdx = 0;

    for (int block = 0; block < io_blocks; ++block){

        for (int i = 0; i < min_block; ++i) {
            inBuffer[inIdx] = input[hostInIdx];
            hostInIdx++;
            inIdx++;
            if(inIdx == model_block){
                //process block
                model->forward(inBuffer, loop_idx, oneshot, outBuffer);
                outIdx = inIdx = 0;
                first_block_done = true;
            }
        }

        for (int i = 0; i < min_block; ++i) {
              if (model->loaded && first_block_done){
                for (int j=0; j<n_loops; j++){
                    if (outIdx >= model_block*n_loops) {
                        std::cout<<"indexing error"<<std::endl;
                        outIdx = 0;
                    }
                    // out(j)[i] = outBuffer[outIdx*n_loops + j];
                    out(j)[hostOutIdx] = outBuffer[outIdx];
                    outIdx++;
                }
                hostOutIdx++;
            }
            else {
                write_zeros_ar(i);
            }

        }
    }
}
} // namespace LivingLooper

PluginLoad(RAVEUGens) {
    // Plugin magic
    ft = inTable;
    registerUnit<LivingLooper::LivingLooper>(ft, "LivingLooper", false);
}
