// LivingLooper.cpp
// Victor Shepardson (victor.shepardson@gmail.com)

#include "LivingLooper.hpp"
#include "SC_PlugIn.hpp"

static InterfaceTable* ft;

namespace LivingLooper {

auto LivingLooper::models = std::map<std::string, LLModel* >();

LivingLooper::LivingLooper() {
    bufIdx = 0;
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

    for (int i = 0; i < nSamples; ++i) {
        if (!model->loaded) {
            write_zeros_ar(i);
            continue;
        }

        inBuffer[bufIdx] = input[i];
        bufIdx++;
        if(bufIdx == model->block_size){
            //process block
            model->forward(inBuffer, loop_idx, oneshot, outBuffer);

            bufIdx = 0;
            first_block_done = true;
        }

        if (first_block_done){
            for (int j=0; j<n_loops; j++){
                out(j)[i] = outBuffer[bufIdx*n_loops + j];
            }
        }
        else {
            write_zeros_ar(i);
        }

    }
}

} // namespace LivingLooper

PluginLoad(RAVEUGens) {
    // Plugin magic
    ft = inTable;
    registerUnit<LivingLooper::LivingLooper>(ft, "LivingLooper", false);
}
